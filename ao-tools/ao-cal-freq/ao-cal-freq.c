/*
 * Copyright © 2014 Keith Packard <keithp@keithp.com>
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
#include <termios.h>
#include <math.h>
#include "ao-elf.h"
#include "ccdbg.h"
#include "cc-usb.h"
#include "cc.h"
#include "ao-verbose.h"

static const struct option options[] = {
	{ .name = "tty", .has_arg = 1, .val = 'T' },
	{ .name = "device", .has_arg = 1, .val = 'D' },
	{ .name = "raw", .has_arg = 0, .val = 'r' },
	{ .name = "verbose", .has_arg = 0, .val = 'v' },
	{ 0, 0, 0, 0},
};

static void usage(char *program)
{
	fprintf(stderr, "usage: %s [--verbose] [--device=<device>] [-tty=<tty>]\n", program);
	exit(1);
}

void
done(struct cc_usb *cc, int code)
{
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

struct flash {
	struct flash	*next;
	char		line[512];
	char		**strs;
};

static struct flash *
flash(struct cc_usb *usb)
{
	struct flash	*head = NULL, **tail = &head;
	cc_usb_printf(usb, "c s\nv\n");
	for (;;) {
		char	line[512];
		struct flash	*b;

		cc_usb_getline(usb, line, sizeof (line));
		b = malloc (sizeof (struct flash));
		strcpy(b->line, line);
		b->strs = tok(line);
		b->next = NULL;
		*tail = b;
		tail = &b->next;
		if (strstr(line, "software-version"))
			break;
	}
	return head;
}

static void
free_flash(struct flash *b) {
	struct flash *n;

	while (b) {
		n = b->next;
		free_strs(b->strs);
		free(b);
		b = n;
	}
}

char **
find_flash(struct flash *b, char *word0) {
	int i;
	for (;b; b = b->next) {
		if (strstr(b->line, word0))
			return b->strs;
	}
	return NULL;
}

void
await_key(void)
{
	struct termios	termios, termios_save;
	char	buf[512];

	tcgetattr(0, &termios);
	termios_save = termios;
	cfmakeraw(&termios);
	tcsetattr(0, TCSAFLUSH, &termios);
	read(0, buf, sizeof (buf));
	tcsetattr(0, TCSAFLUSH, &termios_save);
}

int
do_cal(struct cc_usb *usb) {
	struct flash	*b;
	char	line[1024];
	double	measured_freq;
	char	**cur_freq_words;
	char	**cur_cal_words;
	char	*line_end;
	int	cur_freq;
	int	cur_cal;
	int	new_cal;

	cc_usb_printf(usb, "E 0\n");

	for(;;) {
		cc_usb_printf(usb, "C 1\n");
		cc_usb_sync(usb);

		printf("Generating RF carrier. Please enter measured frequency [enter for done]: ");
		fflush(stdout);
		fgets(line, sizeof (line) - 1, stdin);
		cc_usb_printf(usb, "C 0\n");
		cc_usb_sync(usb);

		measured_freq = strtod(line, &line_end);
		if (line_end == line)
			break;

		b = flash(usb);

		cur_cal_words = find_flash(b, "Radio cal:");
		cur_freq_words = find_flash(b, "Frequency:");

		if (!cur_cal_words || !cur_freq_words) {
			printf("no response\n");
			return 0;
		}

		cur_cal = atoi(cur_cal_words[2]);
		cur_freq = atoi(cur_freq_words[1]);

		printf ("Current radio calibration %d\n", cur_cal);
		printf ("Current radio frequency: %d\n", cur_freq);


		new_cal = floor ((((double) cur_freq / 1000.0) / measured_freq) * cur_cal + 0.5);

		printf ("Programming flash with cal value %d\n", new_cal);

		cc_usb_printf (usb, "c f %d\nc w\n", new_cal);
		cc_usb_sync(usb);
	}
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
	int			expected_size;

	while ((c = getopt_long(argc, argv, "vrT:D:c:s:", options, NULL)) != -1) {
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
		tty = cc_usbdevs_find_by_arg(device, "TeleMega");
	if (!tty)
		tty = getenv("ALTOS_TTY");
	if (!tty)
		tty="/dev/ttyACM0";

	cc = cc_usb_open(tty);

	if (!cc)
		exit(1);

	if (!do_cal(cc))
		ret = 1;
	done(cc, ret);
}
