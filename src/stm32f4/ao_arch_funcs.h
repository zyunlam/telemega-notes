/*
 * Copyright © 2018 Keith Packard <keithp@keithp.com>
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

#ifndef _AO_ARCH_FUNCS_H_
#define _AO_ARCH_FUNCS_H_

/* GPIO functions */

#define ao_power_register(gpio)
#define ao_power_unregister(gpio)

static inline void ao_enable_port(struct stm_gpio *port)
{
	if ((port) == &stm_gpioa) {
		stm_rcc.ahb1enr |= (1 << STM_RCC_AHB1ENR_IOPAEN);
		ao_power_register(&ao_power_gpioa);
	} else if ((port) == &stm_gpiob) {
		stm_rcc.ahb1enr |= (1 << STM_RCC_AHB1ENR_IOPBEN);
		ao_power_register(&ao_power_gpiob);
	} else if ((port) == &stm_gpioc) {
		stm_rcc.ahb1enr |= (1 << STM_RCC_AHB1ENR_IOPCEN);
		ao_power_register(&ao_power_gpioc);
	} else if ((port) == &stm_gpiod) {
		stm_rcc.ahb1enr |= (1 << STM_RCC_AHB1ENR_IOPDEN);
		ao_power_register(&ao_power_gpiod);
	} else if ((port) == &stm_gpioe) {
		stm_rcc.ahb1enr |= (1 << STM_RCC_AHB1ENR_IOPEEN);
		ao_power_register(&ao_power_gpioe);
	} else if ((port) == &stm_gpiof) {
		stm_rcc.ahb1enr |= (1 << STM_RCC_AHB1ENR_IOPFEN);
		ao_power_register(&ao_power_gpiof);
	} else if ((port) == &stm_gpiog) {
		stm_rcc.ahb1enr |= (1 << STM_RCC_AHB1ENR_IOPGEN);
		ao_power_register(&ao_power_gpiog);
	} else if ((port) == &stm_gpioh) {
		stm_rcc.ahb1enr |= (1 << STM_RCC_AHB1ENR_IOPHEN);
		ao_power_register(&ao_power_gpioh);
	}
}

static inline void ao_disable_port(struct stm_gpio *port)
{
	if ((port) == &stm_gpioa) {
		stm_rcc.ahb1enr &= ~(1 << STM_RCC_AHB1ENR_IOPAEN);
		ao_power_unregister(&ao_power_gpioa);
	} else if ((port) == &stm_gpiob) {
		stm_rcc.ahb1enr &= ~(1 << STM_RCC_AHB1ENR_IOPBEN);
		ao_power_unregister(&ao_power_gpiob);
	} else if ((port) == &stm_gpioc) {
		stm_rcc.ahb1enr &= ~(1 << STM_RCC_AHB1ENR_IOPCEN);
		ao_power_unregister(&ao_power_gpioc);
	} else if ((port) == &stm_gpiod) {
		stm_rcc.ahb1enr &= ~(1 << STM_RCC_AHB1ENR_IOPDEN);
		ao_power_unregister(&ao_power_gpiod);
	} else if ((port) == &stm_gpioe) {
		stm_rcc.ahb1enr &= ~(1 << STM_RCC_AHB1ENR_IOPEEN);
		ao_power_unregister(&ao_power_gpioe);
	} else if ((port) == &stm_gpiof) {
		stm_rcc.ahb1enr &= ~(1 << STM_RCC_AHB1ENR_IOPFEN);
		ao_power_unregister(&ao_power_gpiof);
	} else if ((port) == &stm_gpiog) {
		stm_rcc.ahb1enr &= ~(1 << STM_RCC_AHB1ENR_IOPGEN);
		ao_power_unregister(&ao_power_gpiog);
	} else if ((port) == &stm_gpioh) {
		stm_rcc.ahb1enr &= ~(1 << STM_RCC_AHB1ENR_IOPHEN);
		ao_power_unregister(&ao_power_gpioh);
	}
}

#define ao_gpio_set(port, bit, v) stm_gpio_set(port, bit, v)

#define ao_gpio_get(port, bit) stm_gpio_get(port, bit)

#define ao_enable_output(port,bit,v) do {			\
		ao_enable_port(port);				\
		ao_gpio_set(port, bit, v);			\
		stm_moder_set(port, bit, STM_MODER_OUTPUT);\
	} while (0)

#define ao_gpio_set_mode(port,bit,mode) do {				\
		if (mode == AO_EXTI_MODE_PULL_UP)			\
			stm_pupdr_set(port, bit, STM_PUPDR_PULL_UP);	\
		else if (mode == AO_EXTI_MODE_PULL_DOWN)		\
			stm_pupdr_set(port, bit, STM_PUPDR_PULL_DOWN);	\
		else							\
			stm_pupdr_set(port, bit, STM_PUPDR_NONE);	\
	} while (0)

#define ao_enable_input(port,bit,mode) do {				\
		ao_enable_port(port);					\
		stm_moder_set(port, bit, STM_MODER_INPUT);		\
		ao_gpio_set_mode(port, bit, mode);			\
	} while (0)

#endif /* _AO_ARCH_FUNCS_H_ */
