/*
 * Copyright © 2019 Keith Packard <keithp@keithp.com>
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

#include <ao.h>
#include <ao_flash.h>
#include <stdio.h>

/* Erase rows are four pages */
static uint32_t
samd21_nvmctrl_row_size(void)
{
	return samd21_nvmctrl_page_size() * 4;
}

/* size of a lock region. That's just total flash size / 16 */
static uint32_t
samd21_nvmctrl_lock_region(void)
{
	return samd21_flash_size() >> 4;
}

/* Find the bit index of an address within the lock word */
static uint8_t
ao_flash_lock_region_bit(void *addr)
{
	uint32_t lock_region = samd21_nvmctrl_lock_region();
	uintptr_t a = (uintptr_t) addr;

	while (lock_region) {
		a >>= 1;
		lock_region >>= 1;
	}

	return (uint8_t) a;
}

static uint8_t
ao_flash_is_locked(void *addr)
{
	return (samd21_nvmctrl.lock >> ao_flash_lock_region_bit(addr)) & 1;
}

/* Execute a single flash operation, waiting for it to complete. This
 * bit of code must be in ram
 */
static void __attribute__ ((section(".sdata2.flash"), noinline))
_ao_flash_execute(uint16_t cmd)
{
	while ((samd21_nvmctrl.intflag & (1 << SAMD21_NVMCTRL_INTFLAG_READY)) == 0)
		;
	samd21_nvmctrl.ctrla = ((cmd << SAMD21_NVMCTRL_CTRLA_CMD) |
				(SAMD21_NVMCTRL_CTRLA_CMDEX_KEY << SAMD21_NVMCTRL_CTRLA_CMDEX));
	while ((samd21_nvmctrl.intflag & (1 << SAMD21_NVMCTRL_INTFLAG_READY)) == 0)
		;
	samd21_nvmctrl.intflag = ((1 << SAMD21_NVMCTRL_INTFLAG_READY) |
				  (1 << SAMD21_NVMCTRL_INTFLAG_ERROR));
}

/* Set the address of the next flash operation */
static void
_ao_flash_set_addr(void *addr)
{
	while ((samd21_nvmctrl.intflag & (1 << SAMD21_NVMCTRL_INTFLAG_READY)) == 0)
		;
	samd21_nvmctrl.addr = ((uint32_t) addr) >> 1;
	while ((samd21_nvmctrl.intflag & (1 << SAMD21_NVMCTRL_INTFLAG_READY)) == 0)
		;
}

/* Unlock a region of flash */
static void
_ao_flash_unlock(void *addr)
{
	if (!ao_flash_is_locked(addr))
		return;

	_ao_flash_set_addr(addr);
	_ao_flash_execute(SAMD21_NVMCTRL_CTRLA_CMD_UR);
}

/* Erase a row of flash */
static void
_ao_flash_erase_row(void *row)
{
	_ao_flash_unlock(row);
	_ao_flash_set_addr(row);
	_ao_flash_execute(SAMD21_NVMCTRL_CTRLA_CMD_ER);
}

void
ao_flash_erase_page(uint32_t *page)
{
	uint8_t *row = (uint8_t *) page;
	uint32_t row_size = samd21_nvmctrl_row_size();
	uint32_t rows = (row_size + 255) / 256;

	if ((uintptr_t) page & (row_size - 1))
		return;

	ao_arch_block_interrupts();

	if (((uintptr_t) row & (row_size - 1)) == 0) {
		while (rows--) {
			_ao_flash_erase_row(row);
			row += row_size;
		}
	}

	ao_arch_release_interrupts();
}

void
ao_flash_page(uint32_t *page, uint32_t *src)
{
	uint32_t	page_shift = samd21_nvmctrl_page_shift();
	uint32_t	pages = 256 >> page_shift;
	uint32_t	i;
	uint32_t	per_page = 1 << (page_shift - 2);

	ao_flash_erase_page(page);

	ao_arch_block_interrupts();

	while(pages--) {
		/* Clear write buffer */
		_ao_flash_execute(SAMD21_NVMCTRL_CTRLA_CMD_PBC);
		_ao_flash_set_addr(page);
		for (i = 0; i < per_page; i++)
			*page++ = *src++;
		_ao_flash_execute(SAMD21_NVMCTRL_CTRLA_CMD_WP);
	}

	ao_arch_release_interrupts();
}

