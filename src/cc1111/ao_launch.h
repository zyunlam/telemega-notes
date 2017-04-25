/*
 * Copyright © 2017 Keith Packard <keithp@keithp.com>
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

#ifndef _AO_LAUNCH_H_
#define _AO_LAUNCH_H_
/* ao_launch.c */

struct ao_launch_command {
	uint16_t	tick;
	uint16_t	serial;
	uint8_t		cmd;
	uint8_t		channel;
	uint16_t	unused;
};

#define AO_LAUNCH_QUERY		1

struct ao_launch_query {
	uint16_t	tick;
	uint16_t	serial;
	uint8_t		channel;
	uint8_t		valid;
	uint8_t		arm_status;
	uint8_t		igniter_status;
};

#define AO_LAUNCH_ARM		2
#define AO_LAUNCH_FIRE		3

void
ao_launch_init(void);

#endif /* _AO_LAUNCH_H_ */
