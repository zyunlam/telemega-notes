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

static FILE *ao_scheme_file;
static int newline = 1;

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

int
ao_scheme_getc(void)
{
	int c;

	if (ao_scheme_file)
		return getc(ao_scheme_file);

	if (newline) {
		if (ao_scheme_read_list)
			printf("+ ");
		else
			printf("> ");
		newline = 0;
	}
	c = getchar();
	if (c == '\n')
		newline = 1;
	return c;
}

int
main (int argc, char **argv)
{
	(void) argc;

	while (*++argv) {
		ao_scheme_file = fopen(*argv, "r");
		if (!ao_scheme_file) {
			perror(*argv);
			exit(1);
		}
		ao_scheme_read_eval_print();
		fclose(ao_scheme_file);
		ao_scheme_file = NULL;
	}
	ao_scheme_read_eval_print();

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
}
