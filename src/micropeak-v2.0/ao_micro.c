/*
 * Copyright © 2020 Keith Packard <keithp@keithp.com>
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
#include <ao_exti.h>
#include <ao_micropeak.h>
#include <ao_adc_stm32l0.h>


uint32_t pa;

static void
ao_msi_init(void)
{
	uint32_t icscr = stm_rcc.icscr;

	/* Set MSI clock to desired range */
	icscr &= ~(STM_RCC_ICSCR_MSIRANGE_MASK << STM_RCC_ICSCR_MSIRANGE);
	icscr |= (AO_MSI_RANGE << STM_RCC_ICSCR_MSIRANGE);
	stm_rcc.icscr = icscr;

	/* Set vcore to 1.2V */
	uint32_t cr = stm_pwr.cr;
	cr &= ~(STM_PWR_CR_VOS_MASK << STM_PWR_CR_VOS);
	cr |= (STM_PWR_CR_VOS_1_2 << STM_PWR_CR_VOS);
	stm_pwr.cr = cr;
}

static void
list_flights(void)
{
	printf("flight %d start %x end %x\n",
	       1, 0 >> 8, ao_storage_total >> 8);
}

const struct ao_cmds ao_micro_cmds[] = {
	{ list_flights,	"l\0List flights" },
	{}
};

void
ao_pa_get(void)
{
	static struct ao_ms5607_value	value;

	ao_ms5607_sample(&ao_ms5607_current);
	ao_ms5607_convert(&ao_ms5607_current, &value);
	pa = value.pres;
}

int
main(void)
{
	ao_msi_init();

	ao_led_init();
	ao_led_on(AO_LED_ORANGE);

	ao_timer_init();
	ao_spi_init();
	ao_ms5607_init();
	ao_ms5607_setup();
	ao_storage_init();

	uint16_t vref = ao_adc_read_vref();

	uint32_t vdda = 3 * stm_vrefint_cal.vrefint_cal * 1000 / vref;
	ao_led_off(AO_LED_ORANGE);

	/* Power supply > 3.25V means we're on USB power */
	if (vdda > 3250) {
		ao_serial_init();
		ao_cmd_init();
		ao_cmd_register(ao_micro_cmds);
		ao_cmd();
	} else {
		ao_microflight();
	}
}
