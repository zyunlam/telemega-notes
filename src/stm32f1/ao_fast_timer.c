/*
 * Copyright © 2024 Keith Packard <keithp@keithp.com>
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
#include <ao_fast_timer.h>

static void (*ao_fast_timer_callback[AO_FAST_TIMER_MAX])(void);
static uint8_t ao_fast_timer_count;
static uint8_t ao_fast_timer_users;

#if AO_FAST_TIMER == 1

# define AO_FAST_TIMER_TYPE 18
# define stm_tim		stm_tim1
# define stm_tim_isr		stm_tim1_up_isr
# define STM_ISR_TIM_POS	STM_ISR_TIM1_UP_POS
# define STM_RCC_APBENR_TIMEN	STM_RCC_APB2ENR_TIM1EN

#elif AO_FAST_TIMER == 2

# define AO_FAST_TIMER_TYPE 234
# define stm_tim		stm_tim2
# define stm_tim_isr		stm_tim2_isr
# define STM_ISR_TIM_POS	STM_ISR_TIM2_POS
# define STM_RCC_APBENR_TIMEN	STM_RCC_APB1ENR_TIM2EN

#elif AO_FAST_TIMER == 3

# define AO_FAST_TIMER_TYPE 234
# define stm_tim		stm_tim3
# define stm_tim_isr		stm_tim3_isr
# define STM_ISR_TIM_POS	STM_ISR_TIM3_POS
# define STM_RCC_APBENR_TIMEN	STM_RCC_APB1ENR_TIM3EN

#elif AO_FAST_TIMER == 4

# define AO_FAST_TIMER_TYPE 234
# define stm_tim		stm_tim4
# define stm_tim_isr		stm_tim4_isr
# define STM_ISR_TIM_POS	STM_ISR_TIM4_POS
# define STM_RCC_APBENR_TIMEN	STM_RCC_APB1ENR_TIM4EN

#else
#error AO_FAST_TIMER
#endif

#if AO_FAST_TIMER_TYPE == 18

#define STM_TIM_CR1(cen) ((0 << STM_TIM18_CR1_CKD) |	\
			  (0 << STM_TIM18_CR1_ARPE) |	\
			  (0 << STM_TIM18_CR1_CMS) |	\
			  (0 << STM_TIM18_CR1_DIR) |	\
			  (0 << STM_TIM18_CR1_OPM) |	\
			  (1 << STM_TIM18_CR1_URS) |	\
			  (0 << STM_TIM18_CR1_UDIS) |	\
			  ((cen) << STM_TIM18_CR1_CEN))
#define STM_TIM_SR_UIF		STM_TIM18_SR_UIF
#define STM_TIM_DIER_UIE	STM_TIM18_DIER_UIE
#define STM_TIM_EGR_UG		STM_TIM18_EGR_UG
#define STM_TIM_CR2_MMS		STM_TIM18_CR2_MMS
#define STM_TIM_CR2_MMS_RESET	STM_TIM18_CR2_MMS_RESET

#define AO_TIM_PCLK	AO_PCLK2

/*
 * According to the STM clock-configuration, timers 18 run
 * twice as fast as the APB2 clock *if* the APB2 prescaler
 * is greater than 1.
 */

#if AO_APB2_PRESCALER > 1
#define AO_TIM_SCALER 2
#else
#define AO_TIM_SCALER 1
#endif

#define STM_RCC_APB_TIM	stm_rcc.apb2enr

#elif AO_FAST_TIMER_TYPE == 234

#define STM_TIM_CR1(cen) ((STM_TIM234_CR1_CKD_1 << STM_TIM234_CR1_CKD) | \
			  (0 << STM_TIM234_CR1_ARPE) |			\
			  (STM_TIM234_CR1_CMS_EDGE << STM_TIM234_CR1_CMS) | \
			  (0 << STM_TIM234_CR1_DIR) |			\
			  (0 << STM_TIM234_CR1_OPM) |			\
			  (0 << STM_TIM234_CR1_URS) |			\
			  (0 << STM_TIM234_CR1_UDIS) |			\
			  ((cen) << STM_TIM234_CR1_CEN))		\

#define AO_TIM_PCLK	AO_PCLK1

/*
 * According to the STM clock-configuration, timers 234 run
 * twice as fast as the APB1 clock *if* the APB1 prescaler
 * is greater than 1.
 */

#if AO_APB1_PRESCALER > 1
#define AO_TIM_SCALER 2
#else
#define AO_TIM_SCALER 1
#endif

#define STM_TIM_SR_UIF		STM_TIM234_SR_UIF
#define STM_TIM_DIER_UIE	STM_TIM234_DIER_UIE
#define STM_TIM_EGR_UG		STM_TIM234_EGR_UG
#define STM_TIM_CR2_MMS		STM_TIM234_CR2_MMS
#define STM_TIM_CR2_MMS_RESET	STM_TIM234_CR2_MMS_RESET

#define STM_RCC_APB_TIM	stm_rcc.apb1enr

#endif

static void
ao_fast_timer_enable(void)
{
	stm_tim.cr1 = STM_TIM_CR1(1);
}

static void
ao_fast_timer_disable(void)
{
	stm_tim.cr1 = STM_TIM_CR1(0);
}

void
ao_fast_timer_on(void (*callback)(void))
{
	ao_fast_timer_callback[ao_fast_timer_count] = callback;
	if (!ao_fast_timer_count++)
		ao_fast_timer_enable();
}

void
ao_fast_timer_off(void (*callback)(void))
{
	uint8_t	n;

	for (n = 0; n < ao_fast_timer_count; n++)
		if (ao_fast_timer_callback[n] == callback) {
			for (; n < ao_fast_timer_count-1; n++) {
				ao_fast_timer_callback[n] = ao_fast_timer_callback[n+1];
			}
			if (!--ao_fast_timer_count)
				ao_fast_timer_disable();
			break;
		}
}

void stm_tim_isr(void)
{
	uint8_t	i;
	if (stm_tim.sr & (1 << STM_TIM_SR_UIF)) {
		stm_tim.sr = 0;

		for (i = 0; i < ao_fast_timer_count; i++)
			(*ao_fast_timer_callback[i])();
	}
}

#ifndef FAST_TIMER_FREQ
#define FAST_TIMER_FREQ	10000
#endif

#define TIMER_FAST	((AO_TIM_PCLK * AO_TIM_SCALER) / FAST_TIMER_FREQ)

void
ao_fast_timer_init(void)
{
	if (!ao_fast_timer_users) {
		stm_nvic_set_enable(STM_ISR_TIM_POS);
		stm_nvic_set_priority(STM_ISR_TIM_POS, AO_STM_NVIC_CLOCK_PRIORITY);

		/* Turn on timer 1 */
		STM_RCC_APB_TIM |= (1 << STM_RCC_APBENR_TIMEN);

		stm_tim.psc = TIMER_FAST;
		stm_tim.arr = 9;
		stm_tim.cnt = 0;

		/* Enable update interrupt */
		stm_tim.dier = (1 << STM_TIM_DIER_UIE);

		/* Poke timer to reload values */
		stm_tim.egr |= (1 << STM_TIM_EGR_UG);

		stm_tim.cr2 = (STM_TIM_CR2_MMS_RESET << STM_TIM_CR2_MMS);
		ao_fast_timer_disable();
	}
	if (ao_fast_timer_users == AO_FAST_TIMER_MAX)
		ao_panic(AO_PANIC_FAST_TIMER);
	ao_fast_timer_users++;
}

