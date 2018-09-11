/*
 * Copyright © 2012 Keith Packard <keithp@keithp.com>
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

#include "ao.h"

#if LED_PER_LED
static const struct {
	struct stm_gpio	*port;
	uint16_t	pin;
} ao_leds[] = {
#ifdef LED_0_PORT
    [0] { LED_0_PORT, LED_0_PIN },
#endif
#ifdef LED_1_PORT
    [1] { LED_1_PORT, LED_1_PIN },
#endif
#ifdef LED_2_PORT
    [2] { LED_2_PORT, LED_2_PIN },
#endif
#ifdef LED_3_PORT
    [3] { LED_3_PORT, LED_3_PIN },
#endif
#ifdef LED_4_PORT
    [4] { LED_4_PORT, LED_4_PIN },
#endif
#ifdef LED_5_PORT
    [5] { LED_5_PORT, LED_5_PIN },
#endif
#ifdef LED_6_PORT
    [6] { LED_6_PORT, LED_6_PIN },
#endif
#ifdef LED_7_PORT
    [7] { LED_7_PORT, LED_7_PIN },
#endif
#ifdef LED_8_PORT
    [8] { LED_8_PORT, LED_8_PIN },
#endif
#ifdef LED_9_PORT
    [9] { LED_9_PORT, LED_9_PIN },
#endif
#ifdef LED_10_PORT
    [10] { LED_10_PORT, LED_10_PIN },
#endif
#ifdef LED_11_PORT
    [11] { LED_11_PORT, LED_11_PIN },
#endif
#ifdef LED_12_PORT
    [12] { LED_12_PORT, LED_12_PIN },
#endif
#ifdef LED_13_PORT
    [13] { LED_13_PORT, LED_13_PIN },
#endif
#ifdef LED_14_PORT
    [14] { LED_14_PORT, LED_14_PIN },
#endif
#ifdef LED_15_PORT
    [15] { LED_15_PORT, LED_15_PIN },
#endif
};
#define N_LED	(sizeof (ao_leds)/sizeof(ao_leds[0]))
#endif
static AO_LED_TYPE ao_led_enable;

void
ao_led_on(AO_LED_TYPE colors)
{
#ifdef LED_PER_LED
	AO_LED_TYPE i;
	for (i = 0; i < N_LED; i++)
		if (colors & (1 << i))
			ao_gpio_set(ao_leds[i].port, ao_leds[i].pin, 1);
#else
#ifdef LED_PORT
	LED_PORT->bsrr = (colors & ao_led_enable);
#else
#ifdef LED_PORT_0
	LED_PORT_0->bsrr = ((colors & ao_led_enable) & LED_PORT_0_MASK) << LED_PORT_0_SHIFT;
#endif
#ifdef LED_PORT_1
	LED_PORT_1->bsrr = ((colors & ao_led_enable) & LED_PORT_1_MASK) << LED_PORT_1_SHIFT;
#endif
#endif
#endif
}

void
ao_led_off(AO_LED_TYPE colors)
{
#ifdef LED_PER_LED
	AO_LED_TYPE i;
	for (i = 0; i < N_LED; i++)
		if (colors & (1 << i))
			ao_gpio_set(ao_leds[i].port, ao_leds[i].pin, 0);
#else
#ifdef LED_PORT
	LED_PORT->bsrr = (uint32_t) (colors & ao_led_enable) << 16;
#else
#ifdef LED_PORT_0
	LED_PORT_0->bsrr = ((uint32_t) (colors & ao_led_enable) & LED_PORT_0_MASK) << (LED_PORT_0_SHIFT + 16);
#endif
#ifdef LED_PORT_1
	LED_PORT_1->bsrr = ((uint32_t) (colors & ao_led_enable) & LED_PORT_1_MASK) << (LED_PORT_1_SHIFT + 16);
#endif
#endif
#endif
}

void
ao_led_set(AO_LED_TYPE colors)
{
	AO_LED_TYPE	on = colors & ao_led_enable;
	AO_LED_TYPE	off = ~colors & ao_led_enable;

	ao_led_off(off);
	ao_led_on(on);
}

void
ao_led_toggle(AO_LED_TYPE colors)
{
#ifdef LED_PER_LED
	AO_LED_TYPE i;
	for (i = 0; i < N_LED; i++)
		if (colors & (1 << i))
			ao_gpio_set(ao_leds[i].port, ao_leds[i].pin, ~ao_gpio_get(ao_leds[i].port, ao_leds[i].pin));
#else
#ifdef LED_PORT
	LED_PORT->odr ^= (colors & ao_led_enable);
#else
#ifdef LED_PORT_0
	LED_PORT_0->odr ^= ((colors & ao_led_enable) & LED_PORT_0_MASK) << LED_PORT_0_SHIFT;
#endif
#ifdef LED_PORT_1
	LED_PORT_1->odr ^= ((colors & ao_led_enable) & LED_PORT_1_MASK) << LED_PORT_1_SHIFT;
#endif
#endif
#endif
}

void
ao_led_for(AO_LED_TYPE colors, AO_LED_TYPE ticks) 
{
	ao_led_on(colors);
	ao_delay(ticks);
	ao_led_off(colors);
}

#define init_led_pin(port, bit) do { \
		stm_moder_set(port, bit, STM_MODER_OUTPUT);		\
		stm_otyper_set(port, bit, STM_OTYPER_PUSH_PULL);	\
	} while (0)

void
ao_led_init(AO_LED_TYPE enable)
{
	AO_LED_TYPE	bit;

	ao_led_enable = enable;
#if LED_PER_LED
	for (bit = 0; bit < N_LED; bit++)
		ao_enable_output(ao_leds[bit].port, ao_leds[bit].pin, 0);
#else
#ifdef LED_PORT
	stm_rcc.ahbenr |= (1 << LED_PORT_ENABLE);
	LED_PORT->odr &= ~enable;
#else
#ifdef LED_PORT_0
	stm_rcc.ahbenr |= (1 << LED_PORT_0_ENABLE);
	LED_PORT_0->odr &= ~((enable & ao_led_enable) & LED_PORT_0_MASK) << LED_PORT_0_SHIFT;
#endif
#ifdef LED_PORT_1
	stm_rcc.ahbenr |= (1 << LED_PORT_1_ENABLE);
	LED_PORT_1->odr &= ~((enable & ao_led_enable) & LED_PORT_1_MASK) << LED_PORT_1_SHIFT;
#endif
#ifdef LED_PORT_2
	stm_rcc.ahbenr |= (1 << LED_PORT_1_ENABLE);
	LED_PORT_1->odr &= ~((enable & ao_led_enable) & LED_PORT_1_MASK) << LED_PORT_1_SHIFT;
#endif
#endif
	for (bit = 0; bit < 16; bit++) {
		if (enable & (1 << bit)) {
#ifdef LED_PORT
			init_led_pin(LED_PORT, bit);
#else
#ifdef LED_PORT_0
			if (LED_PORT_0_MASK & (1 << bit))
				init_led_pin(LED_PORT_0, bit + LED_PORT_0_SHIFT);
#endif
#ifdef LED_PORT_1
			if (LED_PORT_1_MASK & (1 << bit))
				init_led_pin(LED_PORT_1, bit + LED_PORT_1_SHIFT);
#endif
#ifdef LED_PORT_2
			if (LED_PORT_2_MASK & (1 << bit))
				init_led_pin(LED_PORT_2, bit + LED_PORT_2_SHIFT);
#endif
#endif
		}
	}
#endif
}
