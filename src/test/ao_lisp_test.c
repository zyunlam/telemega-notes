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

#include "ao_lisp.h"
#include <stdio.h>

static FILE *ao_lisp_file;
static int newline = 1;

static char save_file[] = "lisp.image";

int
ao_lisp_os_save(void)
{
	FILE	*save = fopen(save_file, "w");

	if (!save) {
		perror(save_file);
		return 0;
	}
	fwrite(ao_lisp_pool, 1, AO_LISP_POOL_TOTAL, save);
	fclose(save);
	return 1;
}

int
ao_lisp_os_restore_save(struct ao_lisp_os_save *save, int offset)
{
	FILE	*restore = fopen(save_file, "r");
	size_t	ret;

	if (!restore) {
		perror(save_file);
		return 0;
	}
	fseek(restore, offset, SEEK_SET);
	ret = fread(save, sizeof (struct ao_lisp_os_save), 1, restore);
	fclose(restore);
	if (ret != 1)
		return 0;
	return 1;
}

int
ao_lisp_os_restore(void)
{
	FILE	*restore = fopen(save_file, "r");
	size_t	ret;

	if (!restore) {
		perror(save_file);
		return 0;
	}
	ret = fread(ao_lisp_pool, 1, AO_LISP_POOL_TOTAL, restore);
	fclose(restore);
	if (ret != AO_LISP_POOL_TOTAL)
		return 0;
	return 1;
}

int
ao_lisp_getc(void)
{
	int c;

	if (ao_lisp_file)
		return getc(ao_lisp_file);

	if (newline) {
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
	while (*++argv) {
		ao_lisp_file = fopen(*argv, "r");
		if (!ao_lisp_file) {
			perror(*argv);
			exit(1);
		}
		ao_lisp_read_eval_print();
		fclose(ao_lisp_file);
		ao_lisp_file = NULL;
	}
	ao_lisp_read_eval_print();
	printf ("collects: full: %d incremental %d\n",
		ao_lisp_collects[AO_LISP_COLLECT_FULL],
		ao_lisp_collects[AO_LISP_COLLECT_INCREMENTAL]);
	printf ("freed: full %d incremental %d\n",
		ao_lisp_freed[AO_LISP_COLLECT_FULL],
		ao_lisp_freed[AO_LISP_COLLECT_INCREMENTAL]);
}
