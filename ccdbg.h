/*
 * Copyright © 2008 Keith Packard <keithp@keithp.com>
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

#ifndef _CCDBG_H_
#define _CCDBG_H_

#include <stdint.h>
#include <stdlib.h>
#include <unistd.h>
#include <string.h>
#include <stdio.h>
#include <stdint.h>
#include <assert.h>
#include <fcntl.h>
#include <sys/types.h>
#include <sys/ioctl.h>
#include <sys/stat.h>
#include <cp2101.h>

#define CC_DATA		CP2101_GPIO_MASK(0)
#define CC_CLOCK	CP2101_GPIO_MASK(1)
#define CC_RESET_N	CP2101_GPIO_MASK(2)

/* painfully slow for now */
#define CC_CLOCK_US	(1 * 1000)

struct ccdbg {
	int	fd;
	uint8_t	debug_data;
	int	clock;
};

#include "cccp.h"

#define CC_CHIP_ERASE		0x14

#define CC_WR_CONFIG		0x1d
#define CC_RD_CONFIG		0x24
# define CC_CONFIG_TIMERS_OFF		(1 << 3)
# define CC_CONFIG_DMA_PAUSE		(1 << 2)
# define CC_CONFIG_TIMER_SUSPEND	(1 << 1)
# define CC_SET_FLASH_INFO_PAGE		(1 << 0)

#define CC_GET_PC		0x28
#define CC_READ_STATUS		0x34
# define CC_STATUS_CHIP_ERASE_DONE	(1 << 7)
# define CC_STATUS_PCON_IDLE		(1 << 6)
# define CC_STATUS_CPU_HALTED		(1 << 5)
# define CC_STATUS_POWER_MODE_0		(1 << 4)
# define CC_STATUS_HALT_STATUS		(1 << 3)
# define CC_STATUS_DEBUG_LOCKED		(1 << 2)
# define CC_STATUS_OSCILLATOR_STABLE	(1 << 1)
# define CC_STATUS_STACK_OVERFLOW	(1 << 0)

#define CC_SET_HW_BRKPNT	0x3b
# define CC_HW_BRKPNT_N(n)	((n) << 3)
# define CC_HW_BRKPNT_N_MASK	(0x3 << 3)
# define CC_HW_BRKPNT_ENABLE	(1 << 2)

#define CC_HALT			0x44
#define CC_RESUME		0x4c
#define CC_DEBUG_INSTR(n)	(0x54|(n))
#define CC_STEP_INSTR		0x5c
#define CC_STEP_REPLACE		(0x64|(n))
#define CC_GET_CHIP_ID		0x68

/* ccdbg-command.c */
void
ccdbg_debug_mode(struct ccdbg *dbg);

void
ccdbg_reset(struct ccdbg *dbg);

uint8_t
ccdbg_read_status(struct ccdbg *dbg);

uint8_t
ccdbg_rd_config(struct ccdbg *dbg);

uint16_t
ccdbg_get_chip_id(struct ccdbg *dbg);

/* ccdbg-io.c */
void
ccdbg_quarter_clock(struct ccdbg *dbg);

void
ccdbg_half_clock(struct ccdbg *dbg);

struct ccdbg *
ccdbg_open(char *file);

void
ccdbg_close(struct ccdbg *dbg);

void
ccdbg_clock_1_0(struct ccdbg *dbg);

void
ccdbg_clock_0_1(struct ccdbg *dbg);

void
ccdbg_write_bit(struct ccdbg *dbg, uint8_t bit);

void
ccdbg_write_byte(struct ccdbg *dbg, uint8_t byte);

uint8_t
ccdbg_read_bit(struct ccdbg *dbg);

uint8_t
ccdbg_read_byte(struct ccdbg *dbg);

void
ccdbg_cmd_write(struct ccdbg *dbg, uint8_t cmd, uint8_t *data, int len);

uint8_t
ccdbg_cmd_write_read8(struct ccdbg *dbg, uint8_t cmd, uint8_t *data, int len);

uint16_t
ccdbg_cmd_write_read16(struct ccdbg *dbg, uint8_t cmd, uint8_t *data, int len);

#endif /* _CCDBG_H_ */
