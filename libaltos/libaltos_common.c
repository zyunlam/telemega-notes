/*
 * Copyright © 2016 Keith Packard <keithp@keithp.com>
 *
 * This program is free software; you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation; either version 2 of the License, or
 * (at your option) any later version.
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

#include "libaltos_private.h"

PUBLIC int
altos_init(void)
{
	return LIBALTOS_SUCCESS;
}

PUBLIC void
altos_fini(void)
{
}

struct altos_error altos_last_error;

void
altos_set_last_error(int code, char *string)
{
	altos_last_error.code = code;
	strncpy(altos_last_error.string, string, sizeof (altos_last_error.string) -1);
	altos_last_error.string[sizeof(altos_last_error.string)-1] = '\0';
}

PUBLIC void
altos_get_last_error(struct altos_error *error)
{
	*error = altos_last_error;
}

PUBLIC int
altos_getchar(struct altos_file *file, int timeout)
{
	int	ret;
	while (file->in_read == file->in_used) {
		ret = altos_fill(file, timeout);
		if (ret)
			return ret;
	}
	return file->in_data[file->in_read++];
}

PUBLIC int
altos_putchar(struct altos_file *file, char c)
{
	int	ret;

	if (file->out_used == USB_BUF_SIZE) {
		ret = altos_flush(file);
		if (ret) {
			return ret;
		}
	}
	file->out_data[file->out_used++] = c;
	ret = 0;
	if (file->out_used == USB_BUF_SIZE)
		ret = altos_flush(file);
	return ret;
}

struct bt_vendor_map {
	char	vendor[10];
	int	port;
};

static const struct bt_vendor_map altos_bt_vendor_map[] = {
	{ .vendor = "00:12:6f:", 1 },	/* Rayson */
	{ .vendor = "8C:DE:52:", 6 },	/* ISSC */
	{ .vendor = "D8:80:39:", 6 },	/* Microchip */
};

#define NUM_BT_VENDOR_MAP	(sizeof altos_bt_vendor_map / sizeof altos_bt_vendor_map[0])
#define BT_PORT_DEFAULT		1

int altos_bt_port(struct altos_bt_device *device) {
	unsigned i;
	for (i = 0; i < NUM_BT_VENDOR_MAP; i++)
		if (strncmp (device->addr, altos_bt_vendor_map[i].vendor, strlen(altos_bt_vendor_map[i].vendor)) == 0)
			return altos_bt_vendor_map[i].port;
	return BT_PORT_DEFAULT;
}

PUBLIC void
altos_free(struct altos_file *file)
{
	altos_close(file);
	free(file);
}
