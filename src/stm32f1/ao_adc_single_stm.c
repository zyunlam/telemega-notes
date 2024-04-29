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
#include <ao_data.h>
#include <ao_adc_single.h>

static uint8_t			ao_adc_ready;

#define AO_ADC_CR2_VAL(start)	((HAS_ADC_TEMP << STM_ADC_CR2_TSVREFE) |\
				 ((start) << STM_ADC_CR2_SWSTART) |	\
				 (0 << STM_ADC_CR2_JWSTART) |		\
				 (0 << STM_ADC_CR2_EXTTRIG) |		\
				 (STM_ADC_CR2_EXTSEL_SWSTART << STM_ADC_CR2_EXTSEL) | \
				 (0 << STM_ADC_CR2_JEXTTRIG) |		\
				 (0 << STM_ADC_CR2_JEXTSEL) |		\
				 (0 << STM_ADC_CR2_ALIGN) |		\
				 (1 << STM_ADC_CR2_DMA) |		\
				 (0 << STM_ADC_CR2_CONT) |		\
				 (1 << STM_ADC_CR2_ADON))

/*
 * Callback from DMA ISR
 *
 * Shut down DMA engine, signal anyone waiting
 */
static void ao_adc_done(int index)
{
	(void) index;
	ao_dma_done_transfer(STM_DMA_INDEX(STM_DMA_CHANNEL_ADC1));
	ao_adc_ready = 1;
	/* Turn the ADC back off */
	stm_adc1.cr2 = 0;
	ao_wakeup((void *) &ao_adc_ready);
}

/*
 * Start the ADC sequence using the DMA engine
 */
static void
ao_adc_poll(struct ao_adc *packet)
{
	ao_adc_ready = 0;
	stm_adc1.sr = 0;
	ao_dma_set_transfer(STM_DMA_INDEX(STM_DMA_CHANNEL_ADC1),
			    &stm_adc1.dr,
			    (void *) packet,
			    AO_NUM_ADC,
			    (0 << STM_DMA_CCR_MEM2MEM) |
			    (STM_DMA_CCR_PL_HIGH << STM_DMA_CCR_PL) |
			    (STM_DMA_CCR_MSIZE_16 << STM_DMA_CCR_MSIZE) |
			    (STM_DMA_CCR_PSIZE_16 << STM_DMA_CCR_PSIZE) |
			    (1 << STM_DMA_CCR_MINC) |
			    (0 << STM_DMA_CCR_PINC) |
			    (0 << STM_DMA_CCR_CIRC) |
			    (STM_DMA_CCR_DIR_PER_TO_MEM << STM_DMA_CCR_DIR));
	ao_dma_set_isr(STM_DMA_INDEX(STM_DMA_CHANNEL_ADC1), ao_adc_done);
	ao_dma_start(STM_DMA_INDEX(STM_DMA_CHANNEL_ADC1));

	stm_adc1.cr2 = AO_ADC_CR2_VAL(0);
	ao_delay(AO_MS_TO_TICKS(10));
	stm_adc1.cr2 = AO_ADC_CR2_VAL(0);
	ao_delay(AO_MS_TO_TICKS(10));
	stm_adc1.cr2 = AO_ADC_CR2_VAL(1);
}

/*
 * Fetch a copy of the most recent ADC data
 */
void
ao_adc_single_get(struct ao_adc *packet)
{
	ao_adc_poll(packet);
	ao_arch_block_interrupts();
	while (!ao_adc_ready)
		ao_sleep(&ao_adc_ready);
	ao_arch_release_interrupts();
}

static void
ao_adc_dump(void)
{
	struct ao_adc	packet;
	ao_adc_single_get(&packet);
	AO_ADC_DUMP(&packet);
}

const struct ao_cmds ao_adc_cmds[] = {
	{ ao_adc_dump,	"a\0Display current ADC values" },
	{ 0, NULL },
};

static inline void
adc_pin_set(struct stm_gpio *gpio, int pin)
{
	ao_enable_port(gpio);
	stm_gpio_conf(gpio, pin,
		      STM_GPIO_CR_MODE_INPUT,
		      STM_GPIO_CR_CNF_INPUT_ANALOG);
}

void
ao_adc_single_init(void)
{
#ifdef AO_ADC_PIN0_PORT
	adc_pin_set(AO_ADC_PIN0_PORT, AO_ADC_PIN0_PIN);
#endif
#ifdef AO_ADC_PIN1_PORT
	adc_pin_set(AO_ADC_PIN1_PORT, AO_ADC_PIN1_PIN);
#endif
#ifdef AO_ADC_PIN2_PORT
	adc_pin_set(AO_ADC_PIN2_PORT, AO_ADC_PIN2_PIN);
#endif
#ifdef AO_ADC_PIN3_PORT
	adc_pin_set(AO_ADC_PIN3_PORT, AO_ADC_PIN3_PIN);
#endif
#ifdef AO_ADC_PIN4_PORT
	adc_pin_set(AO_ADC_PIN4_PORT, AO_ADC_PIN4_PIN);
#endif
#ifdef AO_ADC_PIN5_PORT
	adc_pin_set(AO_ADC_PIN5_PORT, AO_ADC_PIN5_PIN);
#endif
#ifdef AO_ADC_PIN6_PORT
	adc_pin_set(AO_ADC_PIN6_PORT, AO_ADC_PIN6_PIN);
#endif
#ifdef AO_ADC_PIN7_PORT
	adc_pin_set(AO_ADC_PIN7_PORT, AO_ADC_PIN7_PIN);
#endif
#ifdef AO_ADC_PIN8_PORT
	adc_pin_set(AO_ADC_PIN8_PORT, AO_ADC_PIN8_PIN);
#endif
#ifdef AO_ADC_PIN9_PORT
	adc_pin_set(AO_ADC_PIN9_PORT, AO_ADC_PIN9_PIN);
#endif
#ifdef AO_ADC_PIN10_PORT
	adc_pin_set(AO_ADC_PIN10_PORT, AO_ADC_PIN10_PIN);
#endif
#ifdef AO_ADC_PIN11_PORT
	adc_pin_set(AO_ADC_PIN11_PORT, AO_ADC_PIN11_PIN);
#endif
#ifdef AO_ADC_PIN12_PORT
	adc_pin_set(AO_ADC_PIN12_PORT, AO_ADC_PIN12_PIN);
#endif
#ifdef AO_ADC_PIN13_PORT
	adc_pin_set(AO_ADC_PIN13_PORT, AO_ADC_PIN13_PIN);
#endif
#ifdef AO_ADC_PIN14_PORT
	adc_pin_set(AO_ADC_PIN14_PORT, AO_ADC_PIN14_PIN);
#endif
#ifdef AO_ADC_PIN15_PORT
	adc_pin_set(AO_ADC_PIN15_PORT, AO_ADC_PIN15_PIN);
#endif
#ifdef AO_ADC_PIN16_PORT
	adc_pin_set(AO_ADC_PIN16_PORT, AO_ADC_PIN16_PIN);
#endif
#ifdef AO_ADC_PIN17_PORT
	adc_pin_set(AO_ADC_PIN17_PORT, AO_ADC_PIN17_PIN);
#endif
#ifdef AO_ADC_PIN18_PORT
	adc_pin_set(AO_ADC_PIN18_PORT, AO_ADC_PIN18_PIN);
#endif
#ifdef AO_ADC_PIN19_PORT
	adc_pin_set(AO_ADC_PIN19_PORT, AO_ADC_PIN19_PIN);
#endif
#ifdef AO_ADC_PIN20_PORT
	adc_pin_set(AO_ADC_PIN20_PORT, AO_ADC_PIN20_PIN);
#endif
#ifdef AO_ADC_PIN21_PORT
	adc_pin_set(AO_ADC_PIN21_PORT, AO_ADC_PIN21_PIN);
#endif
#ifdef AO_ADC_PIN22_PORT
	adc_pin_set(AO_ADC_PIN22_PORT, AO_ADC_PIN22_PIN);
#endif
#ifdef AO_ADC_PIN23_PORT
	adc_pin_set(AO_ADC_PIN23_PORT, AO_ADC_PIN23_PIN);
#endif
#ifdef AO_ADC_PIN24_PORT
	#error "Too many ADC ports"
#endif

	stm_rcc.apb2enr |= (1 << STM_RCC_APB2ENR_ADC1EN);

	/* Turn off ADC during configuration */
	stm_adc1.cr2 = 0;

	stm_adc1.cr1 = ((0 << STM_ADC_CR1_AWDEN ) |
		       (0 << STM_ADC_CR1_JAWDEN ) |
		       (STM_ADC_CR1_DUALMOD_INDEPENDENT << STM_ADC_CR1_DUALMOD ) |
		       (0 << STM_ADC_CR1_DISCNUM ) |
		       (0 << STM_ADC_CR1_JDISCEN ) |
		       (0 << STM_ADC_CR1_DISCEN ) |
		       (0 << STM_ADC_CR1_JAUTO ) |
		       (0 << STM_ADC_CR1_AWDSGL ) |
		       (1 << STM_ADC_CR1_SCAN ) |
		       (0 << STM_ADC_CR1_JEOCIE ) |
		       (0 << STM_ADC_CR1_AWDIE ) |
		       (0 << STM_ADC_CR1_EOCIE ) |
		       (0 << STM_ADC_CR1_AWDCH ));

	/* 384 cycle sample time for everyone */
	stm_adc1.smpr1 = 0x00ffffff;
	stm_adc1.smpr2 = 0x3fffffff;

	stm_adc1.sqr1 = ((AO_NUM_ADC - 1) << 20);
#if AO_NUM_ADC > 0
	stm_adc1.sqr3 |= (AO_ADC_SQ1 << 0);
#endif
#if AO_NUM_ADC > 1
	stm_adc1.sqr3 |= (AO_ADC_SQ2 << 5);
#endif
#if AO_NUM_ADC > 2
	stm_adc1.sqr3 |= (AO_ADC_SQ3 << 10);
#endif
#if AO_NUM_ADC > 3
	stm_adc1.sqr3 |= (AO_ADC_SQ4 << 15);
#endif
#if AO_NUM_ADC > 4
	stm_adc1.sqr3 |= (AO_ADC_SQ5 << 20);
#endif
#if AO_NUM_ADC > 5
	stm_adc1.sqr3 |= (AO_ADC_SQ6 << 25);
#endif
#if AO_NUM_ADC > 6
	stm_adc1.sqr2 |= (AO_ADC_SQ7 << 0);
#endif
#if AO_NUM_ADC > 7
	stm_adc1.sqr2 |= (AO_ADC_SQ8 << 5);
#endif
#if AO_NUM_ADC > 8
	stm_adc1.sqr2 |= (AO_ADC_SQ9 << 10);
#endif
#if AO_NUM_ADC > 9
	stm_adc1.sqr2 |= (AO_ADC_SQ10 << 15);
#endif
#if AO_NUM_ADC > 10
	stm_adc1.sqr2 |= (AO_ADC_SQ11 << 20);
#endif
#if AO_NUM_ADC > 11
	stm_adc1.sqr2 |= (AO_ADC_SQ12 << 25);
#endif
#if AO_NUM_ADC > 12
	stm_adc1.sqr1 |= (AO_ADC_SQ13 << 0);
#endif
#if AO_NUM_ADC > 13
	stm_adc1.sqr1 |= (AO_ADC_SQ14 << 5);
#endif
#if AO_NUM_ADC > 14
	stm_adc1.sqr1 |= (AO_ADC_SQ15 << 10);
#endif
#if AO_NUM_ADC > 15
	stm_adc1.sqr1 |= (AO_ADC_SQ16 << 15);
#endif
#if AO_NUM_ADC > 15
#error "too many ADC channels"
#endif

	/* Clear any stale status bits */
	stm_adc1.sr = 0;

	ao_dma_alloc(STM_DMA_INDEX(STM_DMA_CHANNEL_ADC1));

	ao_cmd_register(&ao_adc_cmds[0]);
}
