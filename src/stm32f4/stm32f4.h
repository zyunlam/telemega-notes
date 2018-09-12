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

#ifndef _STM32F4_H_
#define _STM32F4_H_

#include <stdint.h>

typedef volatile uint32_t	vuint32_t;
typedef volatile void *		vvoid_t;
typedef volatile uint16_t	vuint16_t;
typedef volatile uint8_t	vuint8_t;

struct stm_pwr {
	vuint32_t	cr;
	vuint32_t	csr;
};

extern struct stm_pwr stm_pwr;

#define stm_pwr	(*((struct stm_pwr *) 0x40007000))

#define STM_PWR_CR_FISSR	21
#define STM_PWR_CR_FMSSR	20
#define STM_PWR_CR_VOS		14
#define  STM_PWR_CR_VOS_SCALE_MODE_3	1
#define  STM_PWR_CR_VOS_SCALE_MODE_2	2
#define  STM_PWR_CR_VOS_SCALE_MODE_1	3
#define  STM_PWR_CR_VOS_SCALE_MODE_MASK	3
#define STM_PWR_CR_ADCDC1	13
#define STM_PWR_CR_MRLVDS	11
#define STM_PWR_CR_LPLVDS	10
#define STM_PWR_CR_FPDS		9
#define STM_PWR_CR_DBP		8
#define STM_PWR_CR_PLS		5
#define STM_PWR_CR_PVDE		4
#define STM_PWR_CR_CSBF		3
#define STM_PWR_CR_CWUF		2
#define STM_PWR_CR_PDDS		1
#define STM_PWR_CR_LPDS		0

struct stm_rcc {
	vuint32_t	cr;
	vuint32_t	pllcfgr;
	vuint32_t	cfgr;
	vuint32_t	cir;

	vuint32_t	ahb1rstr;
	vuint32_t	ahb2rstr;
	vuint32_t	ahb3rstr;
	uint32_t	pad_1c;

	vuint32_t	apb1rstr;
	vuint32_t	apb2rstr;
	vuint32_t	pad_28;
	vuint32_t	pad_2c;

	vuint32_t	ahb1enr;
	vuint32_t	ahb2enr;
	vuint32_t	ahbdnr;
	vuint32_t	pad_3c;

	vuint32_t	apb1enr;
	vuint32_t	apb2enr;
	vuint32_t	pad_48;
	vuint32_t	pad_4c;

	vuint32_t	ahb1lpenr;
	vuint32_t	ahb2lpenr;
	vuint32_t	ahb3lpenr;
	vuint32_t	pad_5c;

	vuint32_t	apb1lpenr;
	vuint32_t	apb2lpenr;
	vuint32_t	pad_68;
	vuint32_t	pad_6c;

	vuint32_t	bdcr;
	vuint32_t	csr;
	vuint32_t	pad_78;
	vuint32_t	pad_7c;

	vuint32_t	sscgr;
	vuint32_t	plli2scfgr;
	vuint32_t	pad_88;
	vuint32_t	dckcfgr;

	vuint32_t	ckgatenr;
	vuint32_t	dckcfgr2;
};

extern struct stm_rcc stm_rcc;

#define stm_rcc	(*((struct stm_rcc *) 0x40023800))

/* Internal HSI is 16MHz */
#define STM_HSI_FREQ		16000000

#define STM_RCC_CR_PLLI2SRDY	(27)
#define STM_RCC_CR_PLLI2SON	(26)
#define STM_RCC_CR_PLLRDY	(25)
#define STM_RCC_CR_PLLON	(24)
#define STM_RCC_CR_CSSON	(19)
#define STM_RCC_CR_HSEBYP	(18)
#define STM_RCC_CR_HSERDY	(17)
#define STM_RCC_CR_HSEON	(16)
#define STM_RCC_CR_HSICAL	(8)
#define STM_RCC_CR_HSITRIM	(3)
#define STM_RCC_CR_HSIRDY	(1)
#define STM_RCC_CR_HSION	(0)

#define STM_RCC_PLLCFGR_PLLM	0
#define  STM_RCC_PLLCFGR_PLLM_MASK	0x3f
#define STM_RCC_PLLCFGR_PLLN	6
#define  STM_RCC_PLLCFGR_PLLN_MASK	0x1ff
#define STM_RCC_PLLCFGR_PLLP	16
#define  STM_RCC_PLLCFGR_PLLP_DIV_2	0
#define  STM_RCC_PLLCFGR_PLLP_DIV_4	1
#define  STM_RCC_PLLCFGR_PLLP_DIV_6	2
#define  STM_RCC_PLLCFGR_PLLP_DIV_8	3
#define  STM_RCC_PLLCFGR_PLLP_MASK	0x3
#define STM_RCC_PLLCFGR_PLLSRC	22
#define  STM_RCC_PLLCFGR_PLLSRC_HSI	0
#define  STM_RCC_PLLCFGR_PLLSRC_HSE	1
#define STM_RCC_PLLCFGR_PLLQ	24
#define  STM_RCC_PLLCFGR_PLLQ_MASK	0xf
#define STM_RCC_PLLCFGR_PLLR	28
#define  STM_RCC_PLLCFGR_PLLR_MASK	0x7

#define STM_RCC_CFGR_MCO2	(30)
#define STM_RCC_CFGR_MCO2PRE	(27)
#define STM_RCC_CFGR_MCO1PRE	(24)
#define STM_RCC_CFGR_MCO1	(21)
#define STM_RCC_CFGR_RTCPRE	(16)

#define STM_RCC_CFGR_PPRE2	(13)
#define  STM_RCC_CFGR_PPRE2_DIV_1	0
#define  STM_RCC_CFGR_PPRE2_DIV_2	4
#define  STM_RCC_CFGR_PPRE2_DIV_4	5
#define  STM_RCC_CFGR_PPRE2_DIV_8	6
#define  STM_RCC_CFGR_PPRE2_DIV_16	7
#define  STM_RCC_CFGR_PPRE2_MASK	7

#define STM_RCC_CFGR_PPRE1	(10)
#define  STM_RCC_CFGR_PPRE1_DIV_1	0
#define  STM_RCC_CFGR_PPRE1_DIV_2	4
#define  STM_RCC_CFGR_PPRE1_DIV_4	5
#define  STM_RCC_CFGR_PPRE1_DIV_8	6
#define  STM_RCC_CFGR_PPRE1_DIV_16	7
#define  STM_RCC_CFGR_PPRE1_MASK	7

#define STM_RCC_CFGR_HPRE	(4)
#define  STM_RCC_CFGR_HPRE_DIV_1	0x0
#define  STM_RCC_CFGR_HPRE_DIV_2	0x8
#define  STM_RCC_CFGR_HPRE_DIV_4	0x9
#define  STM_RCC_CFGR_HPRE_DIV_8	0xa
#define  STM_RCC_CFGR_HPRE_DIV_16	0xb
#define  STM_RCC_CFGR_HPRE_DIV_64	0xc
#define  STM_RCC_CFGR_HPRE_DIV_128	0xd
#define  STM_RCC_CFGR_HPRE_DIV_256	0xe
#define  STM_RCC_CFGR_HPRE_DIV_512	0xf
#define  STM_RCC_CFGR_HPRE_MASK		0xf

#define STM_RCC_CFGR_SWS	(2)
#define  STM_RCC_CFGR_SWS_HSI		0
#define  STM_RCC_CFGR_SWS_HSE		1
#define  STM_RCC_CFGR_SWS_PLL		2
#define  STM_RCC_CFGR_SWS_MASK		3

#define STM_RCC_CFGR_SW		(0)
#define  STM_RCC_CFGR_SW_HSI		0
#define  STM_RCC_CFGR_SW_HSE		1
#define  STM_RCC_CFGR_SW_PLL		2
#define  STM_RCC_CFGR_SW_MASK		3

#define STM_RCC_AHB1ENR_IOPAEN	0
#define STM_RCC_AHB1ENR_IOPBEN	1
#define STM_RCC_AHB1ENR_IOPCEN	2
#define STM_RCC_AHB1ENR_IOPDEN	3
#define STM_RCC_AHB1ENR_IOPEEN	4
#define STM_RCC_AHB1ENR_IOPFEN	5
#define STM_RCC_AHB1ENR_IOPGEN	6
#define STM_RCC_AHB1ENR_IOPHEN	7

#define STM_RCC_APB1ENR_UART8EN		31
#define STM_RCC_APB1ENR_UART7EN		30
#define STM_RCC_APB1ENR_DACEN		29
#define STM_RCC_APB1ENR_PWREN		28
#define STM_RCC_APB1ENR_CAN3EN		27
#define STM_RCC_APB1ENR_CAN2EN		26
#define STM_RCC_APB1ENR_CAN1EN		25
#define STM_RCC_APB1ENR_I2CFMP1EN	24
#define STM_RCC_APB1ENR_I2C3EN		23
#define STM_RCC_APB1ENR_I2C2EN		22
#define STM_RCC_APB1ENR_I2C1EN		21
#define STM_RCC_APB1ENR_UART5EN		20
#define STM_RCC_APB1ENR_UART4EN		19
#define STM_RCC_APB1ENR_USART3EN	18
#define STM_RCC_APB1ENR_USART2EN	17
#define STM_RCC_APB1ENR_SPI3EN		15
#define STM_RCC_APB1ENR_SPI2EN		14
#define STM_RCC_APB1ENR_WWDGEN		11
#define STM_RCC_APB1ENR_RTCAPBEN	10
#define STM_RCC_APB1ENR_LPTIMER1EN	9
#define STM_RCC_APB1ENR_TIM14EN		8
#define STM_RCC_APB1ENR_TIM13EN		7
#define STM_RCC_APB1ENR_TIM12EN		6
#define STM_RCC_APB1ENR_TIM7EN		5
#define STM_RCC_APB1ENR_TIM6EN		4
#define STM_RCC_APB1ENR_TIM5EN		3
#define STM_RCC_APB1ENR_TIM4EN		2
#define STM_RCC_APB1ENR_TIM3EN		1
#define STM_RCC_APB1ENR_TIM2EN		0

#define STM_RCC_APB2ENR_DFSDM2EN	25
#define STM_RCC_APB2ENR_DFSDM1EN	24
#define STM_RCC_APB2ENR_SAI1EN		22
#define STM_RCC_APB2ENR_SPI5EN		20
#define STM_RCC_APB2ENR_TIM11EN		18
#define STM_RCC_APB2ENR_TIM10EN		17
#define STM_RCC_APB2ENR_TIM9EN		16
#define STM_RCC_APB2ENR_EXITEN		15
#define STM_RCC_APB2ENR_SYSCFGEN	14
#define STM_RCC_APB2ENR_SPI4EN		13
#define STM_RCC_APB2ENR_SPI1EN		12
#define STM_RCC_APB2ENR_SDIOEN		11
#define STM_RCC_APB2ENR_ADC1EN		8
#define STM_RCC_APB2ENR_UART10EN	7
#define STM_RCC_APB2ENR_UART9EN		5
#define STM_RCC_APB2ENR_USART6EN	5
#define STM_RCC_APB2ENR_USART1EN	4
#define STM_RCC_APB2ENR_TIM8EN		1
#define STM_RCC_APB2ENR_TIM1EN		0

#define STM_RCC_CSR_RMVF		24

struct stm_ictr {
	vuint32_t	ictr;
};

extern struct stm_ictr stm_ictr;

#define stm_ictr (*((struct stm_ictr *) 0xe000e004))

#define STM_ICTR_ICTR_INTLINESNUM	0
#define  STM_ICTR_ICTR_INTLINESNUM_MASK		0xf

struct stm_nvic {
	vuint32_t	iser[8];	/* 0x000 0xe000e100 Set Enable Register */

	uint8_t		_unused020[0x080 - 0x020];

	vuint32_t	icer[8];	/* 0x080 0xe000e180 Clear Enable Register */

	uint8_t		_unused0a0[0x100 - 0x0a0];

	vuint32_t	ispr[8];	/* 0x100 0xe000e200 Set Pending Register */

	uint8_t		_unused120[0x180 - 0x120];

	vuint32_t	icpr[8];	/* 0x180 0xe000e280 Clear Pending Register */

	uint8_t		_unused1a0[0x200 - 0x1a0];

	vuint32_t	iabr[8];	/* 0x200 0xe000e300 Active Bit Register */

	uint8_t		_unused220[0x300 - 0x220];

	vuint32_t	ipr[60];	/* 0x300 0xe000e400 Priority Register */
};

extern struct stm_nvic stm_nvic;

#define stm_nvic (*((struct stm_nvic *) 0xe000e100))

#define IRQ_REG(irq)	((irq) >> 5)
#define IRQ_BIT(irq)	((irq) & 0x1f)
#define IRQ_MASK(irq)	(1 << IRQ_BIT(irq))
#define IRQ_BOOL(v,irq)	(((v) >> IRQ_BIT(irq)) & 1)

static inline void
stm_nvic_set_enable(int irq) {
	stm_nvic.iser[IRQ_REG(irq)] = IRQ_MASK(irq);
}

static inline void
stm_nvic_clear_enable(int irq) {
	stm_nvic.icer[IRQ_REG(irq)] = IRQ_MASK(irq);
}

static inline int
stm_nvic_enabled(int irq) {
	return IRQ_BOOL(stm_nvic.iser[IRQ_REG(irq)], irq);
}

static inline void
stm_nvic_set_pending(int irq) {
	stm_nvic.ispr[IRQ_REG(irq)] = IRQ_MASK(irq);
}

static inline void
stm_nvic_clear_pending(int irq) {
	stm_nvic.icpr[IRQ_REG(irq)] = IRQ_MASK(irq);
}

static inline int
stm_nvic_pending(int irq) {
	return IRQ_BOOL(stm_nvic.ispr[IRQ_REG(irq)], irq);
}

static inline int
stm_nvic_active(int irq) {
	return IRQ_BOOL(stm_nvic.iabr[IRQ_REG(irq)], irq);
}

#define IRQ_PRIO_REG(irq)	((irq) >> 2)
#define IRQ_PRIO_BIT(irq)	(((irq) & 3) << 3)
#define IRQ_PRIO_MASK(irq)	(0xff << IRQ_PRIO_BIT(irq))

static inline void
stm_nvic_set_priority(int irq, uint8_t prio) {
	int		n = IRQ_PRIO_REG(irq);
	uint32_t	v;

	v = stm_nvic.ipr[n];
	v &= ~IRQ_PRIO_MASK(irq);
	v |= (prio) << IRQ_PRIO_BIT(irq);
	stm_nvic.ipr[n] = v;
}

static inline uint8_t
stm_nvic_get_priority(int irq) {
	return (stm_nvic.ipr[IRQ_PRIO_REG(irq)] >> IRQ_PRIO_BIT(irq)) & IRQ_PRIO_MASK(0);
}

#define isr(name) void stm_ ## name ## _isr(void)

isr(nmi);
isr(hardfault);
isr(memmanage);
isr(busfault);
isr(usagefault);
isr(svc);
isr(debugmon);
isr(pendsv);
isr(systick);
isr(wwdg);
isr(pvd);
isr(tamper_stamp);
isr(rtc_wkup);
isr(flash);
isr(rcc);
isr(exti0);
isr(exti1);
isr(exti2);
isr(exti3);
isr(exti4);
isr(dma1_stream0);
isr(dma1_stream1);
isr(dma1_stream2);
isr(dma1_stream3);
isr(dma1_stream4);
isr(dma1_stream5);
isr(dma1_stream6);
isr(adc);
isr(can1_tx);
isr(can1_rx0);
isr(can1_rx1);
isr(can1_sce);
isr(exti9_5);
isr(tim1_brk_tim9);
isr(tim1_up_tim10);
isr(tim_trg_com_tim11);
isr(tim1_cc);
isr(tim2);
isr(tim3);
isr(tim4);
isr(i2c1_evt);
isr(i2c1_err);
isr(i2c2_evt);
isr(i2c2_err);
isr(spi1);
isr(spi2);
isr(usart1);
isr(usart2);
isr(usart3);
isr(exti15_10);
isr(rtc_alarm);
isr(otg_fs_wkup);
isr(tim8_brk_tim12);
isr(tim8_up_tim13);
isr(tim8_trg_com_tim14);
isr(tim8_cc);
isr(dma1_stream7);
isr(fsmc);
isr(sdio);
isr(tim5);
isr(spi3);
isr(uart4);
isr(uart5);
isr(tim6_glb_it);
isr(tim7);
isr(dma2_stream0);
isr(dma2_stream1);
isr(dma2_stream2);
isr(dma2_stream3);
isr(dma2_stream4);
isr(dfsdm1_flt0);
isr(dfsdm1_flt1);
isr(can2_tx);
isr(can2_rx0);
isr(can2_rx1);
isr(can2_sce);
isr(otg_fs);
isr(dma2_stream5);
isr(dma2_stream6);
isr(dma2_stream7);
isr(usart6);
isr(i2c3_ev);
isr(i2c3_er);
isr(can3_tx);
isr(can3_rx0);
isr(can3_rx1);
isr(can3_sce);
isr(crypto);
isr(rng);
isr(fpu);
isr(uart7);
isr(uart8);
isr(spi4);
isr(spi5);
isr(sai1);
isr(uart9);
isr(uart10);
isr(quad_spi);
isr(i2cfmp1_ev);
isr(i2cfmp1_er);
isr(exti23);
isr(dfsdm2_flt0);
isr(dfsdm2_flt1);
isr(dfsdm2_flt2);
isr(dfsdm2_flt3);

#undef isr

#define STM_ISR_WWDG_POS		0
#define STM_ISR_PVD_POS			1
#define STM_ISR_TAMPER_STAMP_POS	2
#define STM_ISR_RTC_WKUP_POS		3
#define STM_ISR_FLASH_POS		4
#define STM_ISR_RCC_POS			5
#define STM_ISR_EXTI0_POS		6
#define STM_ISR_EXTI1_POS		7
#define STM_ISR_EXTI2_POS		8
#define STM_ISR_EXTI3_POS		9
#define STM_ISR_EXTI4_POS		10
#define STM_ISR_DMA1_STREAM0_POS	11
#define STM_ISR_DMA1_STREAM1_POS	12
#define STM_ISR_DMA1_STREAM2_POS	13
#define STM_ISR_DMA1_STREAM3_POS	14
#define STM_ISR_DMA1_STREAM4_POS	15
#define STM_ISR_DMA1_STREAM5_POS	16
#define STM_ISR_DMA1_STREAM6_POS	17
#define STM_ISR_ADC_POS			18
#define STM_ISR_CAN1_TX_POS		19
#define STM_ISR_CAN1_RX0_POS		20
#define STM_ISR_CAN1_RX1_POS		21
#define STM_ISR_CAN1_SCE_POS		22
#define STM_ISR_EXTI9_5_POS		23
#define STM_ISR_USART1_POS		37
#define STM_ISR_USART2_POS		38
#define STM_ISR_USART3_POS		39
#define STM_ISR_UART4_POS		52
#define STM_ISR_UART5_POS		53
#define STM_ISR_USART6_POS		71
#define STM_ISR_UART7_POS		82
#define STM_ISR_UART9_POS		88
#define STM_ISR_UART10_POS		89

#define STM_ISR_EXTI15_10_POS		40

struct stm_flash {
	vuint32_t	acr;
	vuint32_t	keyr;
	vuint32_t	optkeyr;
	vuint32_t	sr;

	vuint32_t	cr;
	vuint32_t	optcr;
	vuint32_t	wrpr;
};

extern struct stm_flash stm_flash;

#define stm_flash (*((struct stm_flash *) 0x40023c00))

#define STM_FLASH_ACR_DCRST	12
#define STM_FLASH_ACR_ICRST	11
#define STM_FLASH_ACR_DCEN	10
#define STM_FLASH_ACR_ICEN	9
#define STM_FLASH_ACR_PRFTEN	8
#define STM_FLASH_ACR_LATENCY	0

struct stm_flash_size {
	vuint16_t	f_size;
};

extern struct stm_flash_size stm_flash_size;

#define stm_flash_size	(*((struct stm_flash_size *) 0x1fff7a22))

struct stm_gpio {
	vuint32_t	moder;
	vuint32_t	otyper;
	vuint32_t	ospeedr;
	vuint32_t	pupdr;

	vuint32_t	idr;
	vuint32_t	odr;
	vuint32_t	bsrr;
	vuint32_t	lckr;

	vuint32_t	afrl;
	vuint32_t	afrh;
};

#define STM_MODER_SHIFT(pin)		((pin) << 1)
#define STM_MODER_MASK			3
#define STM_MODER_INPUT			0
#define STM_MODER_OUTPUT		1
#define STM_MODER_ALTERNATE		2
#define STM_MODER_ANALOG		3

static inline void
stm_moder_set(struct stm_gpio *gpio, int pin, vuint32_t value) {
	gpio->moder = ((gpio->moder &
			~(STM_MODER_MASK << STM_MODER_SHIFT(pin))) |
		       value << STM_MODER_SHIFT(pin));
}

static inline uint32_t
stm_moder_get(struct stm_gpio *gpio, int pin) {
	return (gpio->moder >> STM_MODER_SHIFT(pin)) & STM_MODER_MASK;
}

#define STM_OTYPER_SHIFT(pin)		(pin)
#define STM_OTYPER_MASK			1
#define STM_OTYPER_PUSH_PULL		0
#define STM_OTYPER_OPEN_DRAIN		1

static inline void
stm_otyper_set(struct stm_gpio *gpio, int pin, vuint32_t value) {
	gpio->otyper = ((gpio->otyper &
			 ~(STM_OTYPER_MASK << STM_OTYPER_SHIFT(pin))) |
			value << STM_OTYPER_SHIFT(pin));
}

static inline uint32_t
stm_otyper_get(struct stm_gpio *gpio, int pin) {
	return (gpio->otyper >> STM_OTYPER_SHIFT(pin)) & STM_OTYPER_MASK;
}

#define STM_OSPEEDR_SHIFT(pin)		((pin) << 1)
#define STM_OSPEEDR_MASK		3
#define STM_OSPEEDR_LOW			0	/* 2-8MHz */
#define STM_OSPEEDR_MEDIUM		1	/* 12.5-50MHz */
#define STM_OSPEEDR_FAST		2	/* 25-100MHz */
#define STM_OSPEEDR_HIGH		3	/* 50-100MHz */

static inline void
stm_ospeedr_set(struct stm_gpio *gpio, int pin, vuint32_t value) {
	gpio->ospeedr = ((gpio->ospeedr &
			~(STM_OSPEEDR_MASK << STM_OSPEEDR_SHIFT(pin))) |
		       value << STM_OSPEEDR_SHIFT(pin));
}

static inline uint32_t
stm_ospeedr_get(struct stm_gpio *gpio, int pin) {
	return (gpio->ospeedr >> STM_OSPEEDR_SHIFT(pin)) & STM_OSPEEDR_MASK;
}

#define STM_PUPDR_SHIFT(pin)		((pin) << 1)
#define STM_PUPDR_MASK			3
#define STM_PUPDR_NONE			0
#define STM_PUPDR_PULL_UP		1
#define STM_PUPDR_PULL_DOWN		2
#define STM_PUPDR_RESERVED		3

static inline void
stm_pupdr_set(struct stm_gpio *gpio, int pin, uint32_t value) {
	gpio->pupdr = ((gpio->pupdr &
			~(STM_PUPDR_MASK << STM_PUPDR_SHIFT(pin))) |
		       value << STM_PUPDR_SHIFT(pin));
}

static inline uint32_t
stm_pupdr_get(struct stm_gpio *gpio, int pin) {
	return (gpio->pupdr >> STM_PUPDR_SHIFT(pin)) & STM_PUPDR_MASK;
}

#define STM_AFR_SHIFT(pin)		((pin) << 2)
#define STM_AFR_MASK			0xf
#define STM_AFR_NONE			0
#define STM_AFR_AF0			0x0
#define STM_AFR_AF1			0x1
#define STM_AFR_AF2			0x2
#define STM_AFR_AF3			0x3
#define STM_AFR_AF4			0x4
#define STM_AFR_AF5			0x5
#define STM_AFR_AF6			0x6
#define STM_AFR_AF7			0x7
#define STM_AFR_AF8			0x8
#define STM_AFR_AF9			0x9
#define STM_AFR_AF10			0xa
#define STM_AFR_AF11			0xb
#define STM_AFR_AF12			0xc
#define STM_AFR_AF13			0xd
#define STM_AFR_AF14			0xe
#define STM_AFR_AF15			0xf

static inline void
stm_afr_set(struct stm_gpio *gpio, int pin, uint32_t value) {
	/*
	 * Set alternate pin mode too
	 */
	stm_moder_set(gpio, pin, STM_MODER_ALTERNATE);
	if (pin < 8)
		gpio->afrl = ((gpio->afrl &
			       ~(STM_AFR_MASK << STM_AFR_SHIFT(pin))) |
			      value << STM_AFR_SHIFT(pin));
	else {
		pin -= 8;
		gpio->afrh = ((gpio->afrh &
			       ~(STM_AFR_MASK << STM_AFR_SHIFT(pin))) |
			      value << STM_AFR_SHIFT(pin));
	}
}

static inline uint32_t
stm_afr_get(struct stm_gpio *gpio, int pin) {
	if (pin < 8)
		return (gpio->afrl >> STM_AFR_SHIFT(pin)) & STM_AFR_MASK;
	else {
		pin -= 8;
		return (gpio->afrh >> STM_AFR_SHIFT(pin)) & STM_AFR_MASK;
	}
}

static inline void
stm_gpio_set(struct stm_gpio *gpio, int pin, uint8_t value) {
	/* Use the bit set/reset register to do this atomically */
	gpio->bsrr = ((uint32_t) (value ^ 1) << (pin + 16)) | ((uint32_t) value << pin);
}

static inline uint8_t
stm_gpio_get(struct stm_gpio *gpio, int pin) {
	return (gpio->idr >> pin) & 1;
}

static inline uint16_t
stm_gpio_get_all(struct stm_gpio *gpio) {
	return gpio->idr;
}

/*
 * We can't define these in registers.ld or our fancy
 * ao_enable_gpio macro will expand into a huge pile of code
 * as the compiler won't do correct constant folding and
 * dead-code elimination
 */

extern struct stm_gpio stm_gpioa;
extern struct stm_gpio stm_gpiob;
extern struct stm_gpio stm_gpioc;
extern struct stm_gpio stm_gpiod;
extern struct stm_gpio stm_gpioe;
extern struct stm_gpio stm_gpiof;
extern struct stm_gpio stm_gpiog;
extern struct stm_gpio stm_gpioh;

#define stm_gpioa  (*((struct stm_gpio *) 0x40020000))
#define stm_gpiob  (*((struct stm_gpio *) 0x40020400))
#define stm_gpioc  (*((struct stm_gpio *) 0x40020800))
#define stm_gpiod  (*((struct stm_gpio *) 0x40020c00))
#define stm_gpioe  (*((struct stm_gpio *) 0x40021000))
#define stm_gpiof  (*((struct stm_gpio *) 0x40021400))
#define stm_gpiog  (*((struct stm_gpio *) 0x40021800))
#define stm_gpioh  (*((struct stm_gpio *) 0x40021c00))

struct stm_scb {
	vuint32_t	cpuid;
	vuint32_t	icsr;
	vuint32_t	vtor;
	vuint32_t	aircr;

	vuint32_t	scr;
	vuint32_t	ccr;
	vuint32_t	shpr1;
	vuint32_t	shpr2;

	vuint32_t	shpr3;
	vuint32_t	shcsr;
	vuint32_t	cfsr;
	vuint32_t	hfsr;

	vuint32_t	dfsr;
	vuint32_t	mmcar;
	vuint32_t	bcar;
	vuint32_t	afsr;

	vuint32_t	id_pfr0;
	vuint32_t	id_pfr1;
	vuint32_t	id_dfr0;
	vuint32_t	id_afr0;

	vuint32_t	id_mmfr0;
	vuint32_t	id_mmfr1;
	vuint32_t	id_mmfr2;
	vuint32_t	id_mmfr3;

	vuint32_t	id_isar0;
	vuint32_t	id_isar1;
	vuint32_t	id_isar2;
	vuint32_t	id_isar3;

	vuint32_t	id_isar4;
	vuint32_t	pad_d74;
	vuint32_t	pad_d78;
	vuint32_t	pad_d7c;

	vuint32_t	pad_d80;
	vuint32_t	pad_d84;
	vuint32_t	cpacr;
	vuint32_t	pad_d8c;

	vuint8_t	pad_d90[0xf00 - 0xd90];

	vuint32_t	stir;
};

extern struct stm_scb stm_scb;

#define stm_scb (*((struct stm_scb *) 0xe000ed00))

#define STM_SCB_CPACR_CP(n)	((n) <<1)
#define  STM_SCB_CPACR_DENIED		0
#define  STM_SCB_CPACR_PRIVILEGED	1
#define  STM_SCB_CPACR_RESERVED		2
#define  STM_SCB_CPACR_FULL		3
#define STM_SCB_CPACR_FP0	STM_SCB_CPACR_CP(10)
#define STM_SCB_CPACR_FP1	STM_SCB_CPACR_CP(11)

#define STM_SCB_AIRCR_VECTKEY		16
#define  STM_SCB_AIRCR_VECTKEY_KEY		0x05fa
#define STM_SCB_AIRCR_PRIGROUP		8
#define STM_SCB_AIRCR_SYSRESETREQ	2
#define STM_SCB_AIRCR_VECTCLRACTIVE	1
#define STM_SCB_AIRCR_VECTRESET		0

/* The SYSTICK starts at 0xe000e010 */

struct stm_systick {
	vuint32_t	csr;
	vuint32_t	rvr;
	vuint32_t	cvr;
	vuint32_t	calib;
};

extern struct stm_systick stm_systick;

#define stm_systick	(*((struct stm_systick *) 0xe000e010))

#define STM_SYSTICK_CSR_ENABLE		0
#define STM_SYSTICK_CSR_TICKINT		1
#define STM_SYSTICK_CSR_CLKSOURCE	2
#define  STM_SYSTICK_CSR_CLKSOURCE_AHB_8		0
#define  STM_SYSTICK_CSR_CLKSOURCE_AHB			1
#define STM_SYSTICK_CSR_COUNTFLAG	16

#define STM_SYSCFG_EXTICR_PA		0
#define STM_SYSCFG_EXTICR_PB		1
#define STM_SYSCFG_EXTICR_PC		2
#define STM_SYSCFG_EXTICR_PD		3
#define STM_SYSCFG_EXTICR_PE		4
#define STM_SYSCFG_EXTICR_PF		5
#define STM_SYSCFG_EXTICR_PG		6
#define STM_SYSCFG_EXTICR_PH		7

struct stm_syscfg {
	vuint32_t	memrmp;
	vuint32_t	pmc;
	vuint32_t	exticr[4];
};

extern struct stm_syscfg stm_syscfg;

#define stm_syscfg (*((struct stm_syscfg *) 0x40013800))

#define STM_SYSCFG_MEMRMP_MEM_MODE	0
#define  STM_SYSCFG_MEMRMP_MEM_MODE_MAIN_FLASH		0
#define  STM_SYSCFG_MEMRMP_MEM_MODE_SYSTEM_FLASH	1
#define  STM_SYSCFG_MEMRMP_MEM_MODE_SRAM		3
#define  STM_SYSCFG_MEMRMP_MEM_MODE_MASK		3

#define STM_SYSCFG_PMC_ADC1DC2		0

static inline void
stm_exticr_set(struct stm_gpio *gpio, int pin) {
	uint8_t	reg = pin >> 2;
	uint8_t	shift = (pin & 3) << 2;
	uint8_t	val = 0;

	/* Enable SYSCFG */
	stm_rcc.apb2enr |= (1 << STM_RCC_APB2ENR_SYSCFGEN);

	if (gpio == &stm_gpioa)
		val = STM_SYSCFG_EXTICR_PA;
	else if (gpio == &stm_gpiob)
		val = STM_SYSCFG_EXTICR_PB;
	else if (gpio == &stm_gpioc)
		val = STM_SYSCFG_EXTICR_PC;
	else if (gpio == &stm_gpiod)
		val = STM_SYSCFG_EXTICR_PD;
	else if (gpio == &stm_gpioe)
		val = STM_SYSCFG_EXTICR_PE;
	else if (gpio == &stm_gpiof)
		val = STM_SYSCFG_EXTICR_PF;
	else if (gpio == &stm_gpiog)
		val = STM_SYSCFG_EXTICR_PG;
	else if (gpio == &stm_gpioh)
		val = STM_SYSCFG_EXTICR_PH;

	stm_syscfg.exticr[reg] = (stm_syscfg.exticr[reg] & ~(0xf << shift)) | val << shift;
}

struct stm_exti {
	vuint32_t	imr;
	vuint32_t	emr;
	vuint32_t	rtsr;
	vuint32_t	ftsr;

	vuint32_t	swier;
	vuint32_t	pr;
};

extern struct stm_exti stm_exti;

#define stm_exti	(*((struct stm_exti *) 0x40013c00))

struct stm_usart {
	vuint32_t	sr;	/* status register */
	vuint32_t	dr;	/* data register */
	vuint32_t	brr;	/* baud rate register */
	vuint32_t	cr1;	/* control register 1 */

	vuint32_t	cr2;	/* control register 2 */
	vuint32_t	cr3;	/* control register 3 */
	vuint32_t	gtpr;	/* guard time and prescaler */
};

extern struct stm_usart	stm_usart6;

#define stm_usart6	(*((struct stm_usart *) 0x40011400))

#define STM_USART_SR_CTS	(9)	/* CTS flag */
#define STM_USART_SR_LBD	(8)	/* LIN break detection flag */
#define STM_USART_SR_TXE	(7)	/* Transmit data register empty */
#define STM_USART_SR_TC		(6)	/* Transmission complete */
#define STM_USART_SR_RXNE	(5)	/* Read data register not empty */
#define STM_USART_SR_IDLE	(4)	/* IDLE line detected */
#define STM_USART_SR_ORE	(3)	/* Overrun error */
#define STM_USART_SR_NF		(2)	/* Noise detected flag */
#define STM_USART_SR_FE		(1)	/* Framing error */
#define STM_USART_SR_PE		(0)	/* Parity error */

#define STM_USART_CR1_OVER8	(15)	/* Oversampling mode */
#define STM_USART_CR1_UE	(13)	/* USART enable */
#define STM_USART_CR1_M		(12)	/* Word length */
#define STM_USART_CR1_WAKE	(11)	/* Wakeup method */
#define STM_USART_CR1_PCE	(10)	/* Parity control enable */
#define STM_USART_CR1_PS	(9)	/* Parity selection */
#define STM_USART_CR1_PEIE	(8)	/* PE interrupt enable */
#define STM_USART_CR1_TXEIE	(7)	/* TXE interrupt enable */
#define STM_USART_CR1_TCIE	(6)	/* Transmission complete interrupt enable */
#define STM_USART_CR1_RXNEIE	(5)	/* RXNE interrupt enable */
#define STM_USART_CR1_IDLEIE	(4)	/* IDLE interrupt enable */
#define STM_USART_CR1_TE	(3)	/* Transmitter enable */
#define STM_USART_CR1_RE	(2)	/* Receiver enable */
#define STM_USART_CR1_RWU	(1)	/* Receiver wakeup */
#define STM_USART_CR1_SBK	(0)	/* Send break */

#define STM_USART_CR2_LINEN	(14)	/* LIN mode enable */
#define STM_USART_CR2_STOP	(12)	/* STOP bits */
#define STM_USART_CR2_STOP_MASK	3
#define STM_USART_CR2_STOP_1	0
#define STM_USART_CR2_STOP_0_5	1
#define STM_USART_CR2_STOP_2	2
#define STM_USART_CR2_STOP_1_5	3

#define STM_USART_CR2_CLKEN	(11)	/* Clock enable */
#define STM_USART_CR2_CPOL	(10)	/* Clock polarity */
#define STM_USART_CR2_CPHA	(9)	/* Clock phase */
#define STM_USART_CR2_LBCL	(8)	/* Last bit clock pulse */
#define STM_USART_CR2_LBDIE	(6)	/* LIN break detection interrupt enable */
#define STM_USART_CR2_LBDL	(5)	/* lin break detection length */
#define STM_USART_CR2_ADD	(0)
#define STM_USART_CR2_ADD_MASK	0xf

#define STM_USART_CR3_ONEBIT	(11)	/* One sample bit method enable */
#define STM_USART_CR3_CTSIE	(10)	/* CTS interrupt enable */
#define STM_USART_CR3_CTSE	(9)	/* CTS enable */
#define STM_USART_CR3_RTSE	(8)	/* RTS enable */
#define STM_USART_CR3_DMAT	(7)	/* DMA enable transmitter */
#define STM_USART_CR3_DMAR	(6)	/* DMA enable receiver */
#define STM_USART_CR3_SCEN	(5)	/* Smartcard mode enable */
#define STM_USART_CR3_NACK	(4)	/* Smartcard NACK enable */
#define STM_USART_CR3_HDSEL	(3)	/* Half-duplex selection */
#define STM_USART_CR3_IRLP	(2)	/* IrDA low-power */
#define STM_USART_CR3_IREN	(1)	/* IrDA mode enable */
#define STM_USART_CR3_EIE	(0)	/* Error interrupt enable */

/* Errata 2.1.5

   Delay after an RCC peripheral clock enabling

   Description

   A delay between an RCC peripheral clock enable and the effective
   peripheral enabling should be taken into account in order to manage
   the peripheral read/write to registers.

   This delay depends on the peripheral’s mapping:

   • If the peripheral is mapped on AHB: the delay should be equal to
     2 AHB cycles.

   • If the peripheral is mapped on APB: the delay should be equal to
     1 + (AHB/APB prescaler) cycles.

   Workarounds

   1. Use the DSB instruction to stall the Cortex-M4 CPU pipeline
      until the instruction is completed.

   2. Insert “n” NOPs between the RCC enable bit write and the
      peripheral register writes
*/

static inline void
stm32f4_set_rcc(uint32_t *rcc, uint32_t value)
{
	*rcc = value;
	asm("dsb");
}

/* Errata 2.1.8

   In some specific cases, DMA2 data corruption occurs when managing
   AHB and APB2 peripherals in a concurrent way

   Description

   When the DMA2 is managing concurrent requests of AHB and APB2
   peripherals, the transfer on the AHB could be performed several
   times.

   Impacted peripheral are:

   • Quad-SPI: indirect mode read and write transfers

   • FSMC: read and write operation with external device having FIFO

   • GPIO: DMA2 transfers to GPIO registers (in memory-to-peripheral
     transfer mode).The transfers from GPIOs register are not
     impacted.


   The data corruption is due to multiple DMA2 accesses over AHB
   peripheral port impacting peripherals embedding a FIFO.

   For transfer to the internal SRAM through the DMA2 AHB peripheral
   port the accesses could be performed several times but without data
   corruptions in cases of concurrent requests.

   Workaround

   • The DMA2 AHB memory port must be used when reading/writing
     from/to Quad-SPI and FSMC instead of DMA2 AHB default peripheral
     port.

   • The DMA2 AHB memory port must be used when writing to GPIOs
     instead of DMA2 AHB default peripheral port.

   Refer to application note AN4031 section “Take benefits of DMA2
   controller and system architecture flexibility” for more details
   about DMA controller feature.

*/



#endif /* _STM32F4_H_ */
