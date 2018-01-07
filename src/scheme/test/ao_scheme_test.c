/*
 * Copyright © 2016 Keith Packard <keithp@keithp.com>
 *
 * This program is free software; you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation, either version 2 of the License, or
 * (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful, but
 * WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
 * General Public License for more details.
 */

#include "ao_scheme.h"
#include <stdio.h>
#include <unistd.h>
#include <getopt.h>

static char save_file[] = "scheme.image";

int
ao_scheme_os_save(void)
{
	FILE	*save = fopen(save_file, "w");

	if (!save) {
		perror(save_file);
		return 0;
	}
	fwrite(ao_scheme_pool, 1, AO_SCHEME_POOL_TOTAL, save);
	fclose(save);
	return 1;
}

int
ao_scheme_os_restore_save(struct ao_scheme_os_save *save, int offset)
{
	FILE	*restore = fopen(save_file, "r");
	size_t	ret;

	if (!restore) {
		perror(save_file);
		return 0;
	}
	fseek(restore, offset, SEEK_SET);
	ret = fread(save, sizeof (struct ao_scheme_os_save), 1, restore);
	fclose(restore);
	if (ret != 1)
		return 0;
	return 1;
}

int
ao_scheme_os_restore(void)
{
	FILE	*restore = fopen(save_file, "r");
	size_t	ret;

	if (!restore) {
		perror(save_file);
		return 0;
	}
	ret = fread(ao_scheme_pool, 1, AO_SCHEME_POOL_TOTAL, restore);
	fclose(restore);
	if (ret != AO_SCHEME_POOL_TOTAL)
		return 0;
	return 1;
}

static const struct option options[] = {
	{ .name = "load", .has_arg = 1, .val = 'l' },
	{ 0, 0, 0, 0 },
};

static void usage(char *program)
{
	fprintf(stderr, "usage: %s [--load=<library> ...] <program ...>\n", program);
}

static void
check_exit(ao_poly v)
{
	if (ao_scheme_exception & AO_SCHEME_EXIT) {
		int	ret;

		if (v == _ao_scheme_bool_true)
			ret = 0;
		else {
			ret = 1;
			if (ao_scheme_is_integer(v))
				ret = ao_scheme_poly_integer(v);
		}
		exit(ret);
	}
}

static void
run_file(char *name)
{
	FILE	*in;
	int 	c;
	ao_poly	v;

	in = fopen(name, "r");
	if (!in) {
		perror(name);
		exit(1);
	}
	c = getc(in);
	if (c == '#') {
		do {
			c = getc(in);
		} while (c != EOF && c != '\n');
	} else {
		ungetc(c, in);
	}
	v = ao_scheme_read_eval_print(in, NULL, false);
	fclose(in);
	check_exit(v);
}

int
main (int argc, char **argv)
{
	int	o;

	while ((o = getopt_long(argc, argv, "?l:", options, NULL)) != -1) {
		switch (o) {
		case '?':
			usage(argv[0]);
			exit(0);
		case 'l':
			ao_scheme_set_argv(&argv[argc]);
			run_file(optarg);
			break;
		default:
			usage(argv[0]);
			exit(1);
		}
	}
	ao_scheme_set_argv(argv + optind);
	if (argv[optind]) {
		run_file(argv[optind]);
	} else {
		ao_poly v;
		v = ao_scheme_read_eval_print(stdin, stdout, true);
		check_exit(v);
		putchar('\n');
	}

#ifdef DBG_MEM_STATS
	printf ("collects: full: %lu incremental %lu\n",
		ao_scheme_collects[AO_SCHEME_COLLECT_FULL],
		ao_scheme_collects[AO_SCHEME_COLLECT_INCREMENTAL]);

	printf ("freed: full %lu incremental %lu\n",
		ao_scheme_freed[AO_SCHEME_COLLECT_FULL],
		ao_scheme_freed[AO_SCHEME_COLLECT_INCREMENTAL]);

	printf("loops: full %lu incremental %lu\n",
		ao_scheme_loops[AO_SCHEME_COLLECT_FULL],
		ao_scheme_loops[AO_SCHEME_COLLECT_INCREMENTAL]);

	printf("loops per collect: full %f incremental %f\n",
	       (double) ao_scheme_loops[AO_SCHEME_COLLECT_FULL] /
	       (double) ao_scheme_collects[AO_SCHEME_COLLECT_FULL],
	       (double) ao_scheme_loops[AO_SCHEME_COLLECT_INCREMENTAL] /
	       (double) ao_scheme_collects[AO_SCHEME_COLLECT_INCREMENTAL]);

	printf("freed per collect: full %f incremental %f\n",
	       (double) ao_scheme_freed[AO_SCHEME_COLLECT_FULL] /
	       (double) ao_scheme_collects[AO_SCHEME_COLLECT_FULL],
	       (double) ao_scheme_freed[AO_SCHEME_COLLECT_INCREMENTAL] /
	       (double) ao_scheme_collects[AO_SCHEME_COLLECT_INCREMENTAL]);

	printf("freed per loop: full %f incremental %f\n",
	       (double) ao_scheme_freed[AO_SCHEME_COLLECT_FULL] /
	       (double) ao_scheme_loops[AO_SCHEME_COLLECT_FULL],
	       (double) ao_scheme_freed[AO_SCHEME_COLLECT_INCREMENTAL] /
	       (double) ao_scheme_loops[AO_SCHEME_COLLECT_INCREMENTAL]);
#endif
	return 0;
}
