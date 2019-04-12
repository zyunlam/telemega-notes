/*
 * Copyright © 2019 Keith Packard <keithp@keithp.com>
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

#include "ao.h"
#include "ao_max6691.h"

#define cat(a,b)	a ## b

#define AO_MAX6691_CCR		(AO_MAX6691_TIMER->cat(ccr, AO_MAX6691_CH))

#if 0
static uint16_t	ao_max6691_data[8];

void
ao_max6691_sample(void)
{
	/* Get the DMA engine ready */
	ao_dma_set_transfer(AO_MAX6691_DMA,
			    &AO_MAX6691_CCR,
			    &ao_max6691_data,
			    8,
			    (0 << STM_DMA_CCR_MEM2MEM) |
			    (STM_DMA_CCR_PL_MEDIUM << STM_DMA_CCR_PL) |
			    (STM_DMA_CCR_MSIZE_16 << STM_DMA_CCR_MSIZE) |
			    (STM_DMA_CCR_PSIZE_16 << STM_DMA_CCR_PSIZE) |
			    (1 << STM_DMA_CCR_MINC) |
			    (0 << STM_DMA_CCR_PINC) |
			    (0 << STM_DMA_CCR_CIRC) |
			    (STM_DMA_CCR_DIR_PER_TO_MEM << STM_DMA_CCR_DIR));
	ao_dma_start(AO_MAX6691_DMA);

	/* Prod the max6691 */
	ao_gpio_set(AO_MAX6691_PORT, AO_MAX6691_PIN, 0);
	ao_arch_nop();
	ao_gpio_set(AO_MAX6691_PORT, AO_MAX6691_PIN, 1);

	/* Switch the pin to timer input mode */
	stm_afr_set(AO_MAX6691_GPIO, AO_MAX6691_PIN, STM_AFR_AF1);

	ao_arch_block_interrupts();
	while (!ao_dma_done(AO_MAX6691_DMA))
		ao_sleep(&ao_dma_done(AO_MAX6691_DMA));
	ao_arch_release_interrupts();

	/* Switch pin back to output mode */
	stm_moder_set(AO_MAX6691_GPIO, AO_MAX6691_PIN, STM_MODER_OUTPUT);

	/* Mark DMA done */
	ao_dma_done_transfer(AO_MAX6691_DMA);
}
#endif

static void
ao_max6691_test(void)
{
	int i;

	printf("Testing MAX6691\n");
	/* Prod the max6691 */
	ao_set_output(AO_MAX6691_GPIO, AO_MAX6691_PIN, 0);
	for (i = 0; i < 1000; i++) {
		ao_led_on(AO_LED_ARMED);
		ao_gpio_set(AO_MAX6691_GPIO, AO_MAX6691_PIN, 0);
		ao_delay(AO_MS_TO_TICKS(250));
		ao_led_off(AO_LED_ARMED);
		ao_gpio_set(AO_MAX6691_GPIO, AO_MAX6691_PIN, 1);
		ao_delay(AO_MS_TO_TICKS(250));
	}
#if 0
	uint16_t	tick;
	uint16_t	i, j;
	uint8_t		p, v;

	for (i = 0; i < 100; i++)
		ao_arch_nop();
	ao_gpio_set(AO_MAX6691_GPIO, AO_MAX6691_PIN, 1);
	ao_arch_nop();
	ao_set_input(AO_MAX6691_GPIO, AO_MAX6691_PIN);
	i = 0;
	p = 1;
	for (tick = 0; i < 8 && tick < 10000; tick++)
	{
		v = ao_gpio_get(AO_MAX6691_GPIO, AO_MAX6691_PIN);
		if (v != p) {
			ao_max6691_data[i++] = tick;
			p = v;
		}
	}
	for (j = 0; j < i; i++)
		printf("%d: %5u\n", j, ao_max6691_data[j]);
#endif
	printf("Done\n");
}

static const struct ao_cmds ao_max6691_cmds[] = {
	{ ao_max6691_test, 	"q\0Thermistor test" },
	{ 0, NULL },
};


void
ao_max6691_init(void)
{
	ao_cmd_register(&ao_max6691_cmds[0]);

#if 0
	struct stm_tim234	*tim = &AO_MAX6691_TIMER;

	stm_rcc.apb1enr |= (1 << AO_MAX6691_TIMER_ENABLE);

	tim->cr1 = 0;
	tim->psc = 0;
	tim->cnt = 0;

	tim->ccmr1 = ((0 << STM_TIM234_CCMR1_IC2F) |
		      (


	tim->ccmr2 = ((0 << STM_TIM234_CCMR2_OC4CE) |
		      (STM_TIM234_CCMR2_OC4M_PWM_MODE_1 << STM_TIM234_CCMR2_OC4M) |
		      (0 << STM_TIM234_CCMR2_OC4PE) |
		      (0 << STM_TIM234_CCMR2_OC4FE) |
		      (STM_TIM234_CCMR2_CC4S_OUTPUT << STM_TIM234_CCMR2_CC4S) |

		      (0 << STM_TIM234_CCMR2_OC3CE) |
		      (STM_TIM234_CCMR2_OC3M_PWM_MODE_1 << STM_TIM234_CCMR2_OC3M) |
		      (0 << STM_TIM234_CCMR2_OC3PE) |
		      (0 << STM_TIM234_CCMR2_OC3FE) |
		      (STM_TIM234_CCMR2_CC3S_OUTPUT << STM_TIM234_CCMR2_CC3S));
	tim->ccer = ((1 << STM_TIM234_CCER_CC1E) |
		     (0 << STM_TIM234_CCER_CC1P) |
		     (1 << STM_TIM234_CCER_CC2E) |
		     (0 << STM_TIM234_CCER_CC2P) |
		     (1 << STM_TIM234_CCER_CC3E) |
		     (0 << STM_TIM234_CCER_CC3P) |
		     (1 << STM_TIM234_CCER_CC4E) |
		     (0 << STM_TIM234_CCER_CC4P));

	tim->egr = 0;

	tim->sr = 0;
	tim->dier = 0;
	tim->smcr = 0;
	tim->cr2 = ((0 << STM_TIM234_CR2_TI1S) |
		    (STM_TIM234_CR2_MMS_RESET<< STM_TIM234_CR2_MMS) |
		    (0 << STM_TIM234_CR2_CCDS));

	tim->cr1 = ((STM_TIM234_CR1_CKD_1 << STM_TIM234_CR1_CKD) |
		    (0 << STM_TIM234_CR1_ARPE) |
		    (STM_TIM234_CR1_CMS_EDGE << STM_TIM234_CR1_CMS) |
		    (STM_TIM234_CR1_DIR_UP << STM_TIM234_CR1_DIR) |
		    (0 << STM_TIM234_CR1_OPM) |
		    (0 << STM_TIM234_CR1_URS) |
		    (0 << STM_TIM234_CR1_UDIS) |
		    (1 << STM_TIM234_CR1_CEN));

	tim->cr2 = ((0 << STM_TIM234_CR2_TI1S) |
		    (STM_TIM234_CR2_MMS_RESET << STM_TIM234_CR2_MMS) |
		    (0 << STM_TIM234_CR2_CCDS));

	tim->dier = ((0 << STM_TIM234_DIER_TDE) |
		     (0 << STM_TIM234_DIER_CC4DE) |
		     (0 << STM_TIM234_DIER_CC3DE) |
		     (1 << STM_TIM234_DIER_CC2DE) |
		     (0 << STM_TIM234_DIER_CC1DE) |
		     (0 << STM_TIM234_DIER_TIE) |
		     (0 << STM_TIM234_DIER_CC4IE) |
		     (0 << STM_TIM234_DIER_CC3IE) |
		     (0 << STM_TIM234_DIER_CC2IE) |
		     (0 << STM_TIM234_DIER_CC1IE) |
		     (0 << STM_TIM234_DIER_UIE));

	tim->egr = ((0 << STM_TIM234_EGR_TG) |
		    (0 << STM_TIM234_EGR_CC4G) |
		    (0 << STM_TIM234_EGR_CC3G) |
		    (1 << STM_TIM234_EGR_CC2G) |
		    (0 << STM_TIM234_EGR_CC1G) |
		    (0 << STM_TIM234_EGR_UG));

	tim->ccmr1 = ((0 << STM_TIM234_CCMR_IC2F) |
		      (0 << STM_TIM234_CCMR_IC2PSC) |
		      (2 << STM_TIM234_CCMR_CC2S) |
		      (

	stm_ospeedr_set(AO_MAX6691_GPIO, AO_MAX6691_PIN, STM_OSPEEDR_40MHz);
#endif
	ao_enable_output(AO_MAX6691_GPIO, AO_MAX6691_PIN, 1);
}
