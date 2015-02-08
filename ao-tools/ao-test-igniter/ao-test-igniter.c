/*
 * Copyright © 2014 Keith Packard <keithp@keithp.com>
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

#include <err.h>
#include <fcntl.h>
#include <gelf.h>
#include <stdio.h>
#include <stdint.h>
#include <stdlib.h>
#include <sysexits.h>
#include <unistd.h>
#include <getopt.h>
#include <string.h>
#include <stdbool.h>
#include "ao-elf.h"
#include "ccdbg.h"
#include "cc-usb.h"
#include "cc.h"
#include "ao-verbose.h"

static const struct option options[] = {
	{ .name = "tty", .has_arg = 1, .val = 'T' },
	{ .name = "device", .has_arg = 1, .val = 'D' },
	{ .name = "raw", .has_arg = 0, .val = 'r' },
	{ .name = "verbose", .has_arg = 1, .val = 'v' },
	{ 0, 0, 0, 0},
};

static void usage(char *program)
{
	fprintf(stderr, "usage: %s [--verbose=<verbose>] [--device=<device>] [-tty=<tty>] main|drogue\n", program);
	exit(1);
}

void
done(struct cc_usb *cc, int code)
{
/*	cc_usb_printf(cc, "a\n"); */
	cc_usb_close(cc);
	exit (code);
}

static int
ends_with(char *whole, char *suffix)
{
	int whole_len = strlen(whole);
	int suffix_len = strlen(suffix);

	if (suffix_len > whole_len)
		return 0;
	return strcmp(whole + whole_len - suffix_len, suffix) == 0;
}

static int
starts_with(char *whole, char *prefix)
{
	int whole_len = strlen(whole);
	int prefix_len = strlen(prefix);

	if (prefix_len > whole_len)
		return 0;
	return strncmp(whole, prefix, prefix_len) == 0;
}

struct igniter {
	struct igniter	*next;
	char		name[512];
	char		status[512];
};

static struct igniter *
igniters(struct cc_usb *usb)
{
	struct igniter	*head = NULL, **tail = &head;
	cc_usb_printf(usb, "t\nv\n");
	for (;;) {
		char	line[512];
		char	name[512];
		char	status[512];

		cc_usb_getline(usb, line, sizeof (line));
		if (strstr(line, "software-version"))
			break;
		if (sscanf(line, "Igniter: %s Status: %s", &name, &status) == 2) {
			struct igniter	*i = malloc (sizeof (struct igniter));
			strcpy(i->name, name);
			strcpy(i->status, status);
			i->next = NULL;
			*tail = i;
			tail = &i->next;
		}
	}
	return head;
}

static void
free_igniters(struct igniter *i) {
	struct igniter *n;

	while (i) {
		n = i->next;
		free(i);
		i = n;
	}
}

static struct igniter *
find_igniter(struct igniter *i, char *name)
{
	for (; i; i = i->next)
		if (strcmp(i->name, name) == 0)
			return i;
}

static int
do_igniter(struct cc_usb *usb, char *name)
{
	struct igniter	*all = igniters(usb);
	struct igniter	*this = find_igniter(all, name);
	if (!this) {
		struct igniter	*i;
		printf("no igniter %s found in");
		for (i = all; i; i = i->next)
			printf(" %s", i->name);
		printf("\n");
		free_igniters(all);
		return 0;
	}
	if (strcmp(this->status, "ready") != 0) {
		printf("igniter %s status is %s\n", this->name, this->status);
		free_igniters(all);
		return 0;
	}
	cc_usb_printf(usb, "i DoIt %s\n", this->name);
	cc_usb_sync(usb);
	free_igniters(all);
	usleep(200000);
	return 1;
}

int
main (int argc, char **argv)
{
	char			*device = NULL;
	char			*filename;
	Elf			*e;
	unsigned int		s;
	int			i;
	int			c;
	int			tries;
	struct cc_usb		*cc = NULL;
	char			*tty = NULL;
	int			success;
	int			verbose = 0;
	int			ret = 0;

	while ((c = getopt_long(argc, argv, "rT:D:c:s:v:", options, NULL)) != -1) {
		switch (c) {
		case 'T':
			tty = optarg;
			break;
		case 'D':
			device = optarg;
			break;
		case 'v':
			verbose++;
			break;
		default:
			usage(argv[0]);
			break;
		}
	}

	ao_verbose = verbose;

	if (verbose > 1)
		ccdbg_add_debug(CC_DEBUG_BITBANG);

	if (!tty)
		tty = cc_usbdevs_find_by_arg(device, "TeleMega-v1.0");
	if (!tty)
		tty = cc_usbdevs_find_by_arg(device, "TeleMetrum-v2.0");
	if (!tty)
		tty = cc_usbdevs_find_by_arg(device, "TeleMini-v2.0");
	if (!tty)
		tty = cc_usbdevs_find_by_arg(device, "EasyMega-v1.0");
	if (!tty)
		tty = cc_usbdevs_find_by_arg(device, "EasyMetrum-v1.0");
	if (!tty)
		tty = cc_usbdevs_find_by_arg(device, "EasyMini-v1.0");
	if (!tty)
		tty = getenv("ALTOS_TTY");
	if (!tty)
		tty="/dev/ttyACM0";

	cc = cc_usb_open(tty);

	if (!cc)
		exit(1);

	for (i = optind; i < argc; i++) {
		char	*name = argv[i];

		if (!do_igniter(cc, name))
			ret++;
	}
	done(cc, ret);
}
