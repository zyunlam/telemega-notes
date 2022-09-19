/*
 * Copyright © 2013 Keith Packard <keithp@keithp.com>
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

#include <getopt.h>
#include <stdlib.h>
#include <string.h>
#include <unistd.h>
#include "ao-hex.h"
#include "ao-elf.h"
#include "ao-verbose.h"

static const struct option options[] = {
	{ .name = "verbose", .has_arg = 1, .val = 'v' },
	{ .name = "output", .has_arg = 1, .val = 'o' },
	{ 0, 0, 0, 0},
};

static void usage(char *program)
{
	fprintf(stderr, "usage: %s [--verbose=<level>] [--output=<output.ihx>] <input.elf>\n", program);
	exit(1);
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

int
main (int argc, char **argv)
{
	char			*input = NULL;
	char			*output = NULL;
	struct ao_hex_image	*full_image = NULL;
	struct ao_sym		*file_symbols;
	int			num_file_symbols;
	FILE			*file;
	int			c;
	int			i;

	while ((c = getopt_long(argc, argv, "v:o:", options, NULL)) != -1) {
		switch (c) {
		case 'o':
			output = optarg;
			break;
		case 'v':
			ao_verbose = (int) strtol(optarg, NULL, 0);
			break;
		default:
			usage(argv[0]);
			break;
		}
	}

	if (optind >= argc)
		usage(argv[0]);

	for (i = optind; i < argc; i++) {
		struct ao_hex_image *image;

		input = argv[i];

		if (ends_with (input, ".ihx"))
			image = ao_hex_load(input, &file_symbols, &num_file_symbols);
		else
			image = ao_load_elf(input, &file_symbols, &num_file_symbols);

		if (!image) {
			fprintf(stderr, "Failed to load %s\n", input);
			usage(argv[0]);
		}

		if (full_image) {
			full_image = ao_hex_image_cat(full_image, image);
			if (!full_image) {
				fprintf(stderr, "Can't merge image %s\n", input);
				usage(argv[0]);
			}
		} else
			full_image = image;
	}

	if (!output)
		file = stdout;
	else {
		file = fopen(output, "w");
		if (!file) {
			perror(output);
			exit(1);
		}
	}

	if (!ao_hex_save(file, full_image, file_symbols, num_file_symbols)) {
		fprintf(stderr, "%s: failed to write hex file\n", output ? output : "<stdout>");
		if (output)
			unlink(output);
		exit(1);
	}
	exit(0);
}
