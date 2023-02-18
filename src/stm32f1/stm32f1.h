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

#ifndef _STM32F1_H_
#define _STM32F1_H_

#include <stdint.h>

typedef volatile uint32_t	vuint32_t;
typedef volatile uint16_t	vuint16_t;
typedef volatile void *		vvoid_t;

struct stm_rcc {
	vuint32_t	cr;
	vuint32_t	cfgr;
	vuint32_t	cir;
	vuint32_t	apb2rstr;

	vuint32_t	apb1rstr;
	vuint32_t	ahbenr;
	vuint32_t	apb2enr;
	vuint32_t	apb1enr;

	vuint32_t	bdcr;
	vuint32_t	csr;
	vuint32_t	ahbstr;
	vuint32_t	cfgr2;
};

extern struct stm_rcc stm_rcc;

//#define stm_rcc    (*((struct stm_rcc *) 0x40021000))

#define STM_RCC_CR_RTCPRE	(29)
#define  STM_RCC_CR_RTCPRE_HSE_DIV_2	0
#define  STM_RCC_CR_RTCPRE_HSE_DIV_4	1
#define  STM_RCC_CR_RTCPRE_HSE_DIV_8	2
#define  STM_RCC_CR_RTCPRE_HSE_DIV_16	3
#define  STM_RCC_CR_RTCPRE_HSE_MASK	3UL

#define STM_RCC_CR_PLL3RDY	(29)
#define STM_RCC_CR_PLL3ON	(28)
#define STM_RCC_CR_PLL2RDY	(27)
#define STM_RCC_CR_PLL2ON	(26)
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

#define STM_RCC_CFGR_MCOPRE	(28)
#define  STM_RCC_CFGR_MCOPRE_DIV_1	0
#define  STM_RCC_CFGR_MCOPRE_DIV_2	1
#define  STM_RCC_CFGR_MCOPRE_DIV_4	2
#define  STM_RCC_CFGR_MCOPRE_DIV_8	3
#define  STM_RCC_CFGR_MCOPRE_DIV_16	4
#define  STM_RCC_CFGR_MCOPRE_MASK	7UL

#define STM_RCC_CFGR_MCO	(24)
#define  STM_RCC_CFGR_MCO_DISABLE	0
#define  STM_RCC_CFGR_MCO_SYSCLK	4
#define  STM_RCC_CFGR_MCO_HSI	5
#define  STM_RCC_CFGR_MCO_HSE	6
#define  STM_RCC_CFGR_MCO_PLL_2	7
#define  STM_RCC_CFGR_MCO_MASK	7UL

#define STM_RCC_CFGR_USBPRE	(22)
#define  STM_RCC_CFGR_USBPRE_1_5	0
#define  STM_RCC_CFGR_USBPRE_1		1

#define STM_RCC_CFGR_PLLMUL	(18)
#define  STM_RCC_CFGR_PLLMUL_2		0
#define  STM_RCC_CFGR_PLLMUL_3		1
#define  STM_RCC_CFGR_PLLMUL_4		2
#define  STM_RCC_CFGR_PLLMUL_5		3
#define  STM_RCC_CFGR_PLLMUL_6		4
#define  STM_RCC_CFGR_PLLMUL_7		5
#define  STM_RCC_CFGR_PLLMUL_8		6
#define  STM_RCC_CFGR_PLLMUL_9		7
#define  STM_RCC_CFGR_PLLMUL_10		8
#define  STM_RCC_CFGR_PLLMUL_11		9
#define  STM_RCC_CFGR_PLLMUL_12		10
#define  STM_RCC_CFGR_PLLMUL_13		11
#define  STM_RCC_CFGR_PLLMUL_14		12
#define  STM_RCC_CFGR_PLLMUL_15		13
#define  STM_RCC_CFGR_PLLMUL_16		14
#define  STM_RCC_CFGR_PLLMUL_MASK	0xfUL

#define STM_RCC_CFGR_PLLXTPRE	(17)
#define  STM_RCC_CFGR_PLLXTPRE_1	0
#define  STM_RCC_CFGR_PLLXTPRE_2	1

#define STM_RCC_CFGR_PLLSRC	(16)
#define  STM_RCC_CFGR_PLLSRC_HSI_2	0
#define  STM_RCC_CFGR_PLLSRC_HSE	1

#define STM_RCC_CFGR_ADCPRE	(14)
#define  STM_RCC_CFGR_ADCPRE_2		0
#define  STM_RCC_CFGR_ADCPRE_4		1
#define  STM_RCC_CFGR_ADCPRE_6		2
#define  STM_RCC_CFGR_ADCPRE_8		3

#define STM_RCC_CFGR_PPRE2	(11)
#define  STM_RCC_CFGR_PPRE2_DIV_1	0
#define  STM_RCC_CFGR_PPRE2_DIV_2	4
#define  STM_RCC_CFGR_PPRE2_DIV_4	5
#define  STM_RCC_CFGR_PPRE2_DIV_8	6
#define  STM_RCC_CFGR_PPRE2_DIV_16	7
#define  STM_RCC_CFGR_PPRE2_MASK	7UL

#define STM_RCC_CFGR_PPRE1	(8)
#define  STM_RCC_CFGR_PPRE1_DIV_1	0
#define  STM_RCC_CFGR_PPRE1_DIV_2	4
#define  STM_RCC_CFGR_PPRE1_DIV_4	5
#define  STM_RCC_CFGR_PPRE1_DIV_8	6
#define  STM_RCC_CFGR_PPRE1_DIV_16	7
#define  STM_RCC_CFGR_PPRE1_MASK	7UL

#define STM_RCC_CFGR_HPRE	(4)
#define  STM_RCC_CFGR_HPRE_DIV_1	0
#define  STM_RCC_CFGR_HPRE_DIV_2	8
#define  STM_RCC_CFGR_HPRE_DIV_4	9
#define  STM_RCC_CFGR_HPRE_DIV_8	0xa
#define  STM_RCC_CFGR_HPRE_DIV_16	0xb
#define  STM_RCC_CFGR_HPRE_DIV_64	0xc
#define  STM_RCC_CFGR_HPRE_DIV_128	0xd
#define  STM_RCC_CFGR_HPRE_DIV_256	0xe
#define  STM_RCC_CFGR_HPRE_DIV_512	0xf
#define  STM_RCC_CFGR_HPRE_MASK		0xfUL

#define STM_RCC_CFGR_SWS	(2)
#define  STM_RCC_CFGR_SWS_HSI		0
#define  STM_RCC_CFGR_SWS_HSE		1
#define  STM_RCC_CFGR_SWS_PLL		2
#define  STM_RCC_CFGR_SWS_MASK		3UL

#define STM_RCC_CFGR_SW		(0)
#define  STM_RCC_CFGR_SW_HSI		0
#define  STM_RCC_CFGR_SW_HSE		1
#define  STM_RCC_CFGR_SW_PLL		2
#define  STM_RCC_CFGR_SW_MASK		3UL

#define STM_RCC_AHBENR_CRCEN	6
#define STM_RCC_AHBENR_FLITFEN	4
#define STM_RCC_AHBENR_SRAMEN	2
#define STM_RCC_AHBENR_DMA2EN	1
#define STM_RCC_AHBENR_DMA1EN	0


#define STM_RCC_APB2ENR_USART1EN	14
#define STM_RCC_APB2ENR_SPI1EN		12
#define STM_RCC_APB2ENR_TIM1EN		11
#define STM_RCC_APB2ENR_ADC2EN		10
#define STM_RCC_APB2ENR_ADC1EN		9
#define STM_RCC_APB2ENR_IOPEEN		6
#define STM_RCC_APB2ENR_IOPDEN		5
#define STM_RCC_APB2ENR_IOPCEN		4
#define STM_RCC_APB2ENR_IOPBEN		3
#define STM_RCC_APB2ENR_IOPAEN		2
#define STM_RCC_APB2ENR_AFIOEN		0

#define STM_RCC_APB1ENR_DACEN		29
#define STM_RCC_APB1ENR_PWREN		28
#define STM_RCC_APB1ENR_BKPEN		27
#define STM_RCC_APB1ENR_CAN2EN		26
#define STM_RCC_APB1ENR_CAN1EN		25
#define STM_RCC_APB1ENR_I2C2EN		22
#define STM_RCC_APB1ENR_I2C1EN		21
#define STM_RCC_APB1ENR_UART5EN		20
#define STM_RCC_APB1ENR_UART4EN		19
#define STM_RCC_APB1ENR_USART3EN	18
#define STM_RCC_APB1ENR_USART2EN	17
#define STM_RCC_APB1ENR_SPI3EN		15
#define STM_RCC_APB1ENR_SPI2EN		14
#define STM_RCC_APB1ENR_WWDGEN		11
#define STM_RCC_APB1ENR_TIM7EN		5
#define STM_RCC_APB1ENR_TIM6EN		4
#define STM_RCC_APB1ENR_TIM5EN		3
#define STM_RCC_APB1ENR_TIM4EN		2
#define STM_RCC_APB1ENR_TIM3EN		1
#define STM_RCC_APB1ENR_TIM2EN		0

#define STM_RCC_CSR_LPWRRSTF		(31)
#define STM_RCC_CSR_WWDGRSTF		(30)
#define STM_RCC_CSR_IWDGRSTF		(29)
#define STM_RCC_CSR_SFTRSTF		(28)
#define STM_RCC_CSR_PORRSTF		(27)
#define STM_RCC_CSR_PINRSTF		(26)
#define STM_RCC_CSR_RMVF		(24)
#define STM_RCC_CSR_LSIRDY		(1)
#define STM_RCC_CSR_LSION		(0)

struct stm_systick {
	vuint32_t	ctrl;
	vuint32_t	load;
	vuint32_t	val;
	vuint32_t	calib;
};

extern struct stm_systick stm_systick;

#define stm_systick	(*((struct stm_systick *) 0xe000e010))

#define STM_SYSTICK_CTRL_ENABLE		0
#define STM_SYSTICK_CTRL_TICKINT	1
#define STM_SYSTICK_CTRL_CLKSOURCE	2
#define  STM_SYSTICK_CTRL_CLKSOURCE_HCLK_8		0
#define  STM_SYSTICK_CTRL_CLKSOURCE_HCLK		1
#define STM_SYSTICK_CTRL_COUNTFLAG	16

/* The NVIC starts at 0xe000e100, so add that to the offsets to find the absolute address */

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

	uint8_t		_unused3f0[0xc00 - 0x3f0];

	vuint32_t	cpuid_base;	/* 0xc00 0xe000ed00 CPUID Base Register */
	vuint32_t	ics;		/* 0xc04 0xe000ed04 Interrupt Control State Register */
	vuint32_t	vto;		/* 0xc08 0xe000ed08 Vector Table Offset Register */
	vuint32_t	ai_rc;		/* 0xc0c 0xe000ed0c Application Interrupt/Reset Control Register */
	vuint32_t	sc;		/* 0xc10 0xe000ed10 System Control Register */
	vuint32_t	cc;		/* 0xc14 0xe000ed14 Configuration Control Register */

	vuint32_t	shpr7_4;	/* 0xc18 0xe000ed18 System Hander Priority Registers */
	vuint32_t	shpr11_8;	/* 0xc1c */
	vuint32_t	shpr15_12;	/* 0xc20 */

	uint8_t		_unusedc18[0xe00 - 0xc24];

	vuint32_t	stir;		/* 0xe00 */
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
	v &= (uint32_t) ~IRQ_PRIO_MASK(irq);
	v |= (prio) << IRQ_PRIO_BIT(irq);
	stm_nvic.ipr[n] = v;
}

static inline uint8_t
stm_nvic_get_priority(int irq) {
	return (stm_nvic.ipr[IRQ_PRIO_REG(irq)] >> IRQ_PRIO_BIT(irq)) & IRQ_PRIO_MASK(0);
}

struct stm_flash_data {
	vuint16_t	f_size;
	vuint16_t	unused02;
	vuint32_t	unused04;
	vuint32_t	device_id[3];
};

extern struct stm_flash_data	stm_flash_data;

static inline uint32_t stm_flash_size(void) { return (uint32_t) stm_flash_data.f_size * 1024; }

#define stm_flash_data	(*((struct stm_flash_data *) 0x1ffff7e0))

struct stm_gpio {
	vuint32_t	cr[2];
	vuint32_t	idr;
	vuint32_t	odr;

	vuint32_t	bsrr;
	vuint32_t	brr;
	vuint32_t	lckr;
};

#define STM_GPIO_CR(y)		((uint8_t) (y) >> 3)
#define STM_GPIO_CR_CNF(y)	((((uint8_t) (y) & 7) << 2) + 2)
#define  STM_GPIO_CR_CNF_INPUT_ANALOG		0
#define  STM_GPIO_CR_CNF_INPUT_FLOATING		1
#define  STM_GPIO_CR_CNF_INPUT_PULL		2
#define  STM_GPIO_CR_CNF_OUTPUT_PUSH_PULL	0
#define	 STM_GPIO_CR_CNF_OUTPUT_OPEN_DRAIN	1
#define	 STM_GPIO_CR_CNF_OUTPUT_AF_PUSH_PULL	2
#define	 STM_GPIO_CR_CNF_OUTPUT_AF_OPEN_DRAIN	3
#define  STM_GPIO_CR_CNF_MASK			3U
#define STM_GPIO_CR_MODE(y)	((((y) & 7) << 2))
#define  STM_GPIO_CR_MODE_INPUT			0
#define  STM_GPIO_CR_MODE_OUTPUT_10MHZ		1
#define  STM_GPIO_CR_MODE_OUTPUT_2MHZ		2
#define  STM_GPIO_CR_MODE_OUTPUT_50MHZ		3
#define  STM_GPIO_CR_MODE_MASK			3U

static inline void
stm_gpio_conf(struct stm_gpio *gpio, int pin, uint8_t mode, uint8_t cnf)
{
	uint8_t		cr = STM_GPIO_CR(pin);
	uint32_t	v = gpio->cr[cr];

	v &= ~((STM_GPIO_CR_CNF_MASK << STM_GPIO_CR_CNF(pin)) |
	       (STM_GPIO_CR_MODE_MASK << STM_GPIO_CR_MODE(pin)));
	v |= (mode << STM_GPIO_CR_MODE(pin)) | (cnf << STM_GPIO_CR_CNF(pin));
	gpio->cr[cr] = v;
}

static inline void
stm_gpio_set(struct stm_gpio *gpio, int pin, uint8_t value) {
	/* Use the bit set/reset register to do this atomically */
	gpio->bsrr = ((uint32_t) (value ^ 1) << (pin + 16)) | ((uint32_t) value << pin);
}

static inline void
stm_gpio_set_mask(struct stm_gpio *gpio, uint16_t bits, uint16_t mask) {
	/* Use the bit set/reset register to do this atomically */
	gpio->bsrr = ((uint32_t) (~bits & mask) << 16) | ((uint32_t) (bits & mask));
}

static inline void
stm_gpio_set_bits(struct stm_gpio *gpio, uint16_t bits) {
	gpio->bsrr = bits;
}

static inline void
stm_gpio_clr_bits(struct stm_gpio *gpio, uint16_t bits) {
	gpio->bsrr = ((uint32_t) bits) << 16;
}

static inline uint8_t
stm_gpio_get(struct stm_gpio *gpio, int pin) {
	return (gpio->idr >> pin) & 1;
}

static inline uint16_t
stm_gpio_get_all(struct stm_gpio *gpio) {
	return (uint16_t) gpio->idr;
}

extern struct stm_gpio stm_gpioa;
extern struct stm_gpio stm_gpiob;
extern struct stm_gpio stm_gpioc;
extern struct stm_gpio stm_gpiod;
extern struct stm_gpio stm_gpioe;

#define stm_gpioe  (*((struct stm_gpio *) 0x40011800))
#define stm_gpiod  (*((struct stm_gpio *) 0x40011400))
#define stm_gpioc  (*((struct stm_gpio *) 0x40011000))
#define stm_gpiob  (*((struct stm_gpio *) 0x40010c00))
#define stm_gpioa  (*((struct stm_gpio *) 0x40010800))

struct stm_afio {
	vuint32_t	evcr;
	vuint32_t	mapr;
	vuint32_t	exticr[4];
	vuint32_t	mapr2;
};

extern struct stm_afio stm_afio;

#define stm_afio	(*((struct stm_afio *) 0x40010000))

#define isr_decl(name) void stm_ ## name ## _isr(void)

isr_decl(halt);
isr_decl(ignore);

isr_decl(nmi);
isr_decl(hardfault);
isr_decl(memmanage);
isr_decl(busfault);
isr_decl(usagefault);
isr_decl(svc);
isr_decl(debugmon);
isr_decl(pendsv);
isr_decl(systick);
isr_decl(wwdg);
isr_decl(pvd);
isr_decl(tamper_stamp);
isr_decl(rtc_wkup);
isr_decl(flash);
isr_decl(rcc);
isr_decl(exti0);
isr_decl(exti1);
isr_decl(exti2);
isr_decl(exti3);
isr_decl(exti4);
isr_decl(dma1_channel1);
isr_decl(dma1_channel2);
isr_decl(dma1_channel3);
isr_decl(dma1_channel4);
isr_decl(dma1_channel5);
isr_decl(dma1_channel6);
isr_decl(dma1_channel7);
isr_decl(adc1_2);
isr_decl(usb_hp);
isr_decl(usb_lp);
isr_decl(can_rx1);
isr_decl(can_sce);
isr_decl(exti9_5);
isr_decl(tim1_brk);
isr_decl(tim1_up);
isr_decl(tim1_trg_com);
isr_decl(tim1_cc);
isr_decl(tim2);
isr_decl(tim3);
isr_decl(tim4);
isr_decl(i2c1_ev);
isr_decl(i2c1_er);
isr_decl(i2c2_ev);
isr_decl(i2c2_er);
isr_decl(spi1);
isr_decl(spi2);
isr_decl(usart1);
isr_decl(usart2);
isr_decl(usart3);
isr_decl(exti15_10);
isr_decl(rtc_alarm);
isr_decl(usb_wakeup);
isr_decl(tim8_brk);
isr_decl(tim8_up);
isr_decl(tim8_trg_com);
isr_decl(tim8_cc);
isr_decl(adc3);
isr_decl(fsmc);
isr_decl(sdio);
isr_decl(tim5);
isr_decl(spi3);
isr_decl(uart4);
isr_decl(uart5);
isr_decl(tim6);
isr_decl(tim7);
isr_decl(dma2_channel1);
isr_decl(dma2_channel2);
isr_decl(dma2_channel3);
isr_decl(dma2_channel4_5);

#undef isr_decl

#endif
