/*
 * Copyright © 2023 Keith Packard <keithp@keithp.com>
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

/* Note that the HSI clock must be running for this code to work.
 * Also, special care must be taken with the linker to ensure that the
 * functions marked 'ramtext' land in ram and not rom. An example of that
 * can be found in altos-loader.ld
 */

static uint8_t
ao_flash_is_locked(void)
{
	return (stm_flash.cr & (1 << STM_FLASH_CR_LOCK)) != 0;
}

static void
ao_flash_unlock(void)
{
	if (!ao_flash_is_locked())
		return;

	/* Unlock FLASH_CR register */
	stm_flash.keyr = STM_FLASH_KEYR_KEY1;
	stm_flash.keyr = STM_FLASH_KEYR_KEY2;
	if (ao_flash_is_locked())
		ao_panic(AO_PANIC_FLASH);
}

static void
ao_flash_lock(void)
{
	stm_flash.cr |= (1 << STM_FLASH_CR_LOCK);
}

#define ao_flash_wait_bsy() do { while (stm_flash.sr & (1 << STM_FLASH_SR_BSY)); } while (0)

static void __attribute__ ((section(".sdata2.flash"), noinline))
_ao_flash_erase_page(uint32_t *page)
{
	stm_flash.cr |= (1 << STM_FLASH_CR_PER);

	stm_flash.ar = (uintptr_t) page;

	stm_flash.cr |= (1 << STM_FLASH_CR_STRT);

	ao_flash_wait_bsy();

	stm_flash.cr &= ~(1UL << STM_FLASH_CR_PER);
}

static uint32_t
stm_flash_page_size(void)
{
	uint16_t	f_size = stm_flash_data.f_size;

	if (f_size <= 128) {
		/* low-density and medium-density devices */
		return 1024;
	} else {
		/*
		 * high-density devices, XL-density devices and
		 * Connectivity devices
		 */
		return 2048;
	}
}

void
ao_flash_erase_page(uint32_t *page)
{
	/* Erase the whole page at the start. This assumes we'll be flashing things
	 * in memory order
	 */

	if ((uintptr_t) page & (stm_flash_page_size() - 1))
		return;

	ao_arch_block_interrupts();
	ao_flash_unlock();

	_ao_flash_erase_page(page);

	ao_flash_lock();
	ao_arch_release_interrupts();
}

static void __attribute__ ((section(".sdata2.flash"), noinline))
_ao_flash_page(uint16_t *dst, uint16_t *src, unsigned int shorts)
{
	uint8_t		i;

	stm_flash.cr |= (1 << STM_FLASH_CR_PG);

	for (i = 0; i < shorts; i++) {
		*dst++ = *src++;
		ao_flash_wait_bsy();
	}

	stm_flash.cr &= ~(1UL << STM_FLASH_CR_PG);
}

void
ao_flash_page(uint32_t *page, uint32_t *src)
{
	ao_flash_erase_page(page);

	ao_arch_block_interrupts();
	ao_flash_unlock();

	_ao_flash_page((uint16_t *) page, (uint16_t *) src, 128);

	ao_flash_lock();
	ao_arch_release_interrupts();
}

/* Stores less than a full page while still smashing the full page */
void
ao_flash_bytes(void *page, void *src, size_t size)
{
	unsigned int shorts = (unsigned int) ((size + 1) >> 1);

	ao_flash_erase_page(page);

	ao_arch_block_interrupts();
	ao_flash_unlock();

	_ao_flash_page((uint16_t *) page, (uint16_t *) src, shorts);

	ao_flash_lock();
	ao_arch_release_interrupts();
}
