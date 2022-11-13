/*
 * Copyright © 2022 Keith Packard <keithp@keithp.com>
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

struct ao_samd21_exti {
	void			(*callback)(void);
	uint8_t			pmux;
	uint8_t			pincfg;
};

static struct ao_samd21_exti ao_samd21_exti[SAMD21_NUM_EIC];

static uint8_t
pin_id(struct samd21_port *port, uint8_t pin)
{
	/* Why, atmel, why? */
	if (port == &samd21_port_a) {
		switch (pin) {
		case 24:
		case 25:
		case 27:
			return pin - 12;
		case 28:
		case 30:
		case 31:
			return pin - 20;
		default:
			break;
		}
	}

	/* most pins use exti mapped to their pin number directly (mod 16) */
	return pin & 0xf;
}


void
samd21_eic_isr(void)
{
	uint32_t	intflag = samd21_eic.intflag;
	uint8_t		id;

	for (id = 0; id < SAMD21_NUM_EIC; id++) {
		uint32_t mask = (1 << id);

		if (intflag & mask) {
			samd21_eic.intflag = mask;
			if (ao_samd21_exti[id].callback)
				(*ao_samd21_exti[id].callback)();
		}
	}
}

static void
_ao_exti_set_sense(uint8_t id, uint8_t mode)
{
	uint8_t		sense = mode & (SAMD21_EIC_CONFIG_SENSE_RISE | SAMD21_EIC_CONFIG_SENSE_FALL);
	uint8_t		n = SAMD21_EIC_CONFIG_N(id);
	uint32_t	config;

	config = samd21_eic.config[n];
	config &= ~(SAMD21_EIC_CONFIG_SENSE_MASK << SAMD21_EIC_CONFIG_SENSE(id));
	config |= (sense << SAMD21_EIC_CONFIG_SENSE(id));
	samd21_eic.config[n] = config;
}

void
ao_exti_setup (struct samd21_port *port, uint8_t pin, uint8_t mode, void (*callback)(void))
{
	uint8_t			id = pin_id(port,pin);
	struct ao_samd21_exti	*exti = &ao_samd21_exti[id];

	if (exti->callback)
		ao_panic(AO_PANIC_EXTI);

	if (mode & AO_EXTI_PIN_NOCONFIGURE) {
		ao_enable_port(port);
		samd21_port_dir_set(port, pin, SAMD21_PORT_DIR_IN);
		samd21_port_pincfg_set(port, pin,
				       (1 << SAMD21_PORT_PINCFG_INEN),
				       (1 << SAMD21_PORT_PINCFG_INEN));
	} else {
		ao_enable_input(port, pin, mode);
	}

	ao_arch_block_interrupts();

	exti->callback = callback;

	/* Set edge triggered */
	_ao_exti_set_sense(id, mode);

	ao_arch_release_interrupts();
}

void
ao_exti_set_mode(struct samd21_port *port, uint8_t pin, uint8_t mode)
{
	uint8_t			id = pin_id(port,pin);

	ao_arch_block_interrupts();
	_ao_exti_set_sense(id, mode);
	ao_arch_release_interrupts();
}

void
ao_exti_set_callback(struct samd21_port *port, uint8_t pin, void (*callback)(void))
{
	uint8_t		id = pin_id(port,pin);

	ao_arch_block_interrupts();
	ao_samd21_exti[id].callback = callback;
	ao_arch_release_interrupts();
}

void
ao_exti_enable(struct samd21_port *port, uint8_t pin)
{
	uint8_t		id = pin_id(port,pin);

	ao_arch_block_interrupts();
	/* configure gpio to interrupt routing */
	if ((samd21_eic.intenset & (1 << id)) == 0) {
		ao_samd21_exti[id].pmux = samd21_port_pmux_get(port, pin);
		ao_samd21_exti[id].pincfg = samd21_port_pincfg_get(port, pin);
	}
	samd21_port_pmux_set(port, pin, SAMD21_PORT_PMUX_FUNC_A);
	samd21_eic.intenset = 1 << id;
	ao_arch_release_interrupts();
}

void
ao_exti_disable(struct samd21_port *port, uint8_t pin)
{
	uint8_t		id = pin_id(port,pin);

	ao_arch_block_interrupts();
	samd21_eic.intenclr = 1 << id;
	/* restore gpio config */
	if (ao_samd21_exti[id].pincfg & (1 << SAMD21_PORT_PINCFG_PMUXEN))
		samd21_port_pmux_set(port, pin, ao_samd21_exti[id].pmux);
	else
		samd21_port_pmux_clr(port, pin);
	ao_arch_release_interrupts();
}

void
ao_exti_init(void)
{
	samd21_gclk_clkctrl(0, SAMD21_GCLK_CLKCTRL_ID_EIC);

	/* Reset */
	samd21_eic.ctrl = (1 << SAMD21_EIC_CTRL_SWRST);

	while (samd21_eic.status & (1 << SAMD21_EIC_STATUS_SYNCBUSY))
		;

	/* Wire up interrupts */
	samd21_nvic_set_enable(SAMD21_NVIC_ISR_EIC_POS);
	samd21_nvic_set_priority(SAMD21_NVIC_ISR_EIC_POS, 3);

	/* Enable */
	samd21_eic.ctrl = (1 << SAMD21_EIC_CTRL_ENABLE);
}
