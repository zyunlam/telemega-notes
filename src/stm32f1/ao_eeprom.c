/*
 * Copyright © 2024 Keith Packard <keithp@keithp.com>
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
 *
 * You should have received a copy of the GNU General Public License along
 * with this program; if not, write to the Free Software Foundation, Inc.,
 * 59 Temple Place, Suite 330, Boston, MA 02111-1307 USA.
 */

#include <ao.h>
#include <ao_flash.h>

extern uint8_t	__eeprom_base[2048];

uint8_t
ao_eeprom_read(ao_pos_t pos, void *v, uint16_t len)
{
	memcpy(v, &__eeprom_base[pos], len);
	return 1;
}

uint8_t
ao_eeprom_write(ao_pos_t pos, void *v, uint16_t len)
{
	bool hsi_on = (stm_rcc.cr & (1UL << STM_RCC_CR_HSIRDY)) != 0;

	if (!hsi_on) {
		stm_rcc.cr |= (1UL << STM_RCC_CR_HSION);
		while (!(stm_rcc.cr & (1 << STM_RCC_CR_HSIRDY)))
			ao_arch_nop();

	}
	ao_flash_bytes(&__eeprom_base[pos], v, len);

	if (!hsi_on) {
		stm_rcc.cr &= ~(1UL << STM_RCC_CR_HSION);
	}

	return 1;
}

