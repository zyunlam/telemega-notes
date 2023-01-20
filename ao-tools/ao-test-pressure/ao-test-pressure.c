/*
 * Copyright © 2022 Bdale Garbee <bdale@gag.com>
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

static void
done(struct cc_usb *cc, int code)
{
/*	cc_usb_printf(cc, "a\n"); */
	cc_usb_close(cc);
	exit (code);
}

static char **
tok(char *line) {
	char	**strs = malloc (sizeof (char *)), *str;
	int	n = 0;

	while ((str = strtok(line, " \t"))) {
		line = NULL;
		strs = realloc(strs, (n + 2) * sizeof (char *));
		strs[n] = strdup(str);
		n++;
	}
	strs[n] = '\0';
	return strs;
}

static void
free_strs(char **strs) {
	char	*str;
	int	i;

	for (i = 0; (str = strs[i]) != NULL; i++)
		free(str);
	free(strs);
}

struct pressure {
	struct pressure	*next;
	char		**strs;
};

static struct pressure *
pressure(struct cc_usb *usb)
{
	struct pressure	*head = NULL, **tail = &head;
	cc_usb_printf(usb, "a\n");
	for (;;) {
		char	line[512];
		struct pressure	*b;

		cc_usb_getline(usb, line, sizeof (line));
		b = malloc (sizeof (struct pressure));
		b->strs = tok(line);
		b->next = NULL;
		*tail = b;
		tail = &b->next;
		if (strstr(line, "tick:"))
			break;
	}
	return head;
}

static void
free_pressure(struct pressure *b) {
	struct pressure *n;

	while (b) {
		n = b->next;
		free_strs(b->strs);
		free(b);
		b = n;
	}
}

static int
do_pressure(struct cc_usb *usb) {
	struct pressure *b = pressure(usb);
	char **alt = b->strs;

	if (!alt) {
		printf("no response\n");
		free_pressure(b);
		return 0;
	}

	double pressure = strtod(alt[5], NULL);

	if (pressure < 15000 || pressure > 16000) {
		printf ("weird pressure %f\n", pressure);
		free_pressure(b);
		return 0;
	}

	printf ("pressure %f\n", pressure);

	return 1;
}

int
main (int argc, char **argv)
{
	char			*device = NULL;
	int			c;
	struct cc_usb		*cc = NULL;
	char			*tty = NULL;
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
		tty = cc_usbdevs_find_by_arg(device, "AltosFlash");
	if (!tty)
		tty = cc_usbdevs_find_by_arg(device, "EasyMotor");
	if (!tty)
		tty = getenv("ALTOS_TTY");
	if (!tty)
		tty="/dev/ttyACM0";

	cc = cc_usb_open(tty);

	if (!cc)
		exit(1);

	if (!do_pressure(cc))
		ret = 1;
	done(cc, ret);
}
