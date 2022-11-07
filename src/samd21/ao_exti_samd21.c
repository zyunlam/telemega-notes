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

struct samd21_exti {
	void	(*callback)(void);
	uint8_t	port;
	uint8_t pin;
};

#if 0
static struct samd21_exti ao_samd_exti[SAMD21_NUM_EVSYS];

static uint32_t ao_exti_inuse;
#endif

void
samd21_evsys_isr(void)
{
}

void
ao_exti_setup (struct samd21_port *port, uint8_t pin, uint8_t mode, void (*callback)(void))
{
	(void) port;
	(void) pin;
	(void) mode;
	(void) callback;
#if 0
	uint8_t		id = pin_id(port,pin);
	uint8_t	       	pint;
	uint8_t		mask;
	uint8_t		prio;

	for (pint = 0; pint < SAMD21_NUM_EVSYS; pint++)
		if ((ao_exti_inuse & (1 << pint)) == 0)
			break;

	if (pint == SAMD21_NUM_EVSYS)
		ao_panic(AO_PANIC_EXTI);

	if (!(mode & AO_EXTI_PIN_NOCONFIGURE))
		ao_enable_input(port, pin, mode);

	ao_arch_block_interrupts();
	mask = (1 << pint);
	ao_exti_inuse |= mask;
	ao_pint_enabled &= (uint8_t) ~mask;

	ao_pint_map[id] = pint;
	ao_exti_callback[pint] = callback;

	/* configure gpio to interrupt routing */
	lpc_scb.pintsel[pint] = id;

	/* Set edge triggered */
	lpc_gpio_pin.isel &= ~mask;

	ao_pint_enabled &= (uint8_t) ~mask;
	ao_pint_mode[pint] = mode;
	_ao_exti_set_enable(pint);

	/* Set interrupt mask and rising/falling mode */

	prio = AO_LPC_NVIC_MED_PRIORITY;
	if (mode & AO_EXTI_PRIORITY_LOW)
		prio = AO_LPC_NVIC_LOW_PRIORITY;
	else if (mode & AO_EXTI_PRIORITY_HIGH)
		prio = AO_LPC_NVIC_HIGH_PRIORITY;

	/* Set priority and enable */
	lpc_nvic_set_priority(LPC_ISR_PIN_INT0_POS + pint, prio);
	lpc_nvic_set_enable(LPC_ISR_PIN_INT0_POS + pint);
	ao_arch_release_interrupts();
#endif
}

void
ao_exti_set_mode(struct samd21_port *port, uint8_t pin, uint8_t mode)
{
	(void) port;
	(void) pin;
	(void) mode;
#if 0
	uint8_t		id = pin_id(port,pin);
	uint8_t		pint = ao_pint_map[id];

	ao_arch_block_interrupts();
	ao_pint_mode[pint] = mode;
	_ao_exti_set_enable(pint);
	ao_arch_release_interrupts();
#endif
}

void
ao_exti_set_callback(struct samd21_port *port, uint8_t pin, void (*callback)(void))
{
	(void) port;
	(void) pin;
	(void) callback;
#if 0
	uint8_t		id = pin_id(port,pin);
	uint8_t		pint = ao_pint_map[id];

	ao_exti_callback[pint] = callback;
#endif
}

void
ao_exti_enable(struct samd21_port *port, uint8_t pin)
{
	(void) port;
	(void) pin;
#if 0
	uint8_t		id = pin_id(port,pin);
	uint8_t		pint = ao_pint_map[id];
	uint8_t		mask = 1 << pint;

	ao_arch_block_interrupts();
	ao_pint_enabled |= mask;
	_ao_exti_set_enable(pint);
	ao_arch_release_interrupts();
#endif
}

void
ao_exti_disable(struct samd21_port *port, uint8_t pin)
{
	(void) port;
	(void) pin;
#if 0
	uint8_t		id = pin_id(port,pin);
	uint8_t		pint = ao_pint_map[id];
	uint8_t		mask = 1 << pint;

	ao_arch_block_interrupts();
	ao_pint_enabled &= (uint8_t) ~mask;
	_ao_exti_set_enable(pint);
	ao_arch_release_interrupts();
#endif
}

void
ao_exti_init(void)
{
}
