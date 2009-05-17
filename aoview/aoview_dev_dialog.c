/*
 * Copyright © 2009 Keith Packard <keithp@keithp.com>
 *
 * This program is free software; you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation; version 2 of the License.
 *
 * This program is distributed in the hope that it will be useful, but
 * WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
 * General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License along
 * with this program; if not, write to the Free Software Foundation, Inc.,
 * 59 Temple Place, Suite 330, Boston, MA 02111-1307 USA.
 */

#include "aoview.h"

static void
aoview_dev_dialog_map(GtkWidget *widget, gpointer data)
{
	GtkTreeView	*dev_list = data;
	GtkListStore	*list_store;
	GtkTreeIter	iter;
	int		ndev, n;
	struct usbdev	**devs;

	list_store = gtk_list_store_new(3,
					G_TYPE_STRING,
					G_TYPE_STRING,
					G_TYPE_STRING);

	ndev = aoview_usb_scan(&devs);
	for (n = 0; n < ndev; n++) {
		gtk_list_store_append(list_store, &iter);
		gtk_list_store_set(list_store, &iter,
				   0, devs[n]->product,
				   1, devs[n]->serial,
				   2, devs[n]->tty,
				   -1);
	}
	gtk_tree_view_set_model (dev_list, GTK_TREE_MODEL(list_store));
	g_object_unref(G_OBJECT(list_store));
	gtk_tree_view_columns_autosize(dev_list);
}

static void
aoview_dev_selected(GtkTreeModel *model,
		    GtkTreePath *path,
		    GtkTreeIter *iter,
		    gpointer data)
{
	gchar *string;
	gtk_tree_model_get(model, iter,
			   2, &string,
			   -1);
	aoview_monitor_connect(string);
}

static GtkWidget	*dialog;

static void
aoview_dev_dialog_connect(GtkWidget *widget, gpointer data)
{
	GtkTreeView		*dev_list = data;
	GtkListStore		*list_store;
	GtkTreeSelection	*tree_selection;

	list_store = GTK_LIST_STORE(gtk_tree_view_get_model(dev_list));
	tree_selection = gtk_tree_view_get_selection(dev_list);
	gtk_tree_selection_selected_foreach(tree_selection,
					    aoview_dev_selected,
					    data);

	gtk_widget_hide(dialog);
}

static void
aoview_dev_disconnect(GtkWidget *widget)
{
	aoview_monitor_disconnect();
}

#define _(a) a

void
aoview_dev_dialog_init(GladeXML *xml)
{
	GtkTreeView	*dev_list;
	GtkWidget	*connect_button;
	GtkTreeSelection	*dev_selection;
	GtkWidget	*ao_disconnect;

	dialog = glade_xml_get_widget(xml, "device_connect_dialog");
	assert(dialog);

	dev_list = GTK_TREE_VIEW(glade_xml_get_widget(xml, "dev_list"));
	assert(dev_list);

	aoview_add_plain_text_column(dev_list, _("Product"), 0, 16);
	aoview_add_plain_text_column(dev_list, _("Serial"),  1, 8);
	aoview_add_plain_text_column(dev_list, _("Device"), 2, 13);

	dev_selection = gtk_tree_view_get_selection(dev_list);
	gtk_tree_selection_set_mode(dev_selection, GTK_SELECTION_SINGLE);

	g_signal_connect(G_OBJECT(dialog), "map",
			 G_CALLBACK(aoview_dev_dialog_map),
			 dev_list);

	connect_button = glade_xml_get_widget(xml, "connect_button");
	assert(connect_button);

	g_signal_connect(G_OBJECT(connect_button), "clicked",
			 G_CALLBACK(aoview_dev_dialog_connect),
			 dev_list);


	ao_disconnect = glade_xml_get_widget(xml, "ao_disconnect");
	assert(ao_disconnect);

	g_signal_connect(G_OBJECT(ao_disconnect), "activate",
			 G_CALLBACK(aoview_dev_disconnect),
			 ao_disconnect);
}
