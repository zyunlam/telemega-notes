/*
 * Copyright © 2023 Keith Packard <keithp@keithp.com>
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

#ifndef _AO_ARCH_FUNCS_H_
#define _AO_ARCH_FUNCS_H_

static inline void
ao_enable_port(struct stm_gpio *port)
{
	if ((port) == &stm_gpioa)
		stm_rcc.apb2enr |= (1 << STM_RCC_APB2ENR_IOPAEN);
	else if ((port) == &stm_gpiob)
		stm_rcc.apb2enr |= (1 << STM_RCC_APB2ENR_IOPBEN);
	else if ((port) == &stm_gpioc)
		stm_rcc.apb2enr |= (1 << STM_RCC_APB2ENR_IOPCEN);
	else if ((port) == &stm_gpiod)
		stm_rcc.apb2enr |= (1 << STM_RCC_APB2ENR_IOPDEN);
	else if ((port) == &stm_gpioe)
		stm_rcc.apb2enr |= (1 << STM_RCC_APB2ENR_IOPEEN);
}

static inline void
ao_disable_port(struct stm_gpio *port)
{
	if ((port) == &stm_gpioa)
		stm_rcc.apb2enr &= ~(1UL << STM_RCC_APB2ENR_IOPAEN);
	else if ((port) == &stm_gpiob)
		stm_rcc.apb2enr &= ~(1UL << STM_RCC_APB2ENR_IOPBEN);
	else if ((port) == &stm_gpioc)
		stm_rcc.apb2enr &= ~(1UL << STM_RCC_APB2ENR_IOPCEN);
	else if ((port) == &stm_gpiod)
		stm_rcc.apb2enr &= ~(1UL << STM_RCC_APB2ENR_IOPDEN);
	else if ((port) == &stm_gpioe)
		stm_rcc.apb2enr &= ~(1UL << STM_RCC_APB2ENR_IOPEEN);
}

#define ao_gpio_set(port, bit, v) stm_gpio_set(port, bit, v)

#define ao_gpio_get(port, bit) stm_gpio_get(port, bit)

#define ao_gpio_set_bits(port, bits) stm_gpio_set_bits(port, bits)

#define ao_gpio_set_mask(port, bits, mask) stm_gpio_set_mask(port, bits, mask)

#define ao_gpio_clr_bits(port, bits) stm_gpio_clr_bits(port, bits);

#define ao_gpio_get_all(port) stm_gpio_get_all(port)

static inline void
ao_enable_output(struct stm_gpio *port, int bit, uint8_t v)
{
	ao_enable_port(port);
	ao_gpio_set(port, bit, v);
	stm_gpio_conf(port, bit,
		      STM_GPIO_CR_MODE_OUTPUT_10MHZ,
		      STM_GPIO_CR_CNF_OUTPUT_PUSH_PULL);
}

#endif /* _AO_ARCH_FUNCS_H_ */
