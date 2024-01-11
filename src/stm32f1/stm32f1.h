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
#define  STM_RCC_CFGR_PLLXTPRE_MASK	1UL

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
#define STM_RCC_APB1ENR_CANEN		26
#define STM_RCC_APB1ENR_USBEN		23
#define STM_RCC_APB1ENR_I2C2EN		22
#define STM_RCC_APB1ENR_I2C1EN		21
#define STM_RCC_APB1ENR_UART5EN		20
#define STM_RCC_APB1ENR_UART4EN		19
#define STM_RCC_APB1ENR_USART3EN	18
#define STM_RCC_APB1ENR_USART2EN	17
#define STM_RCC_APB1ENR_SPI3EN		15
#define STM_RCC_APB1ENR_SPI2EN		14
#define STM_RCC_APB1ENR_WWDGEN		11
#define STM_RCC_APB1ENR_TIM14EN		8
#define STM_RCC_APB1ENR_TIM13EN		7
#define STM_RCC_APB1ENR_TIM12EN		6
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

//#define stm_systick	(*((struct stm_systick *) 0xe000e010))

#define STM_SYSTICK_CTRL_ENABLE		0
#define STM_SYSTICK_CTRL_TICKINT	1
#define STM_SYSTICK_CTRL_CLKSOURCE	2
#define  STM_SYSTICK_CTRL_CLKSOURCE_HCLK_8		0
#define  STM_SYSTICK_CTRL_CLKSOURCE_HCLK		1
#define STM_SYSTICK_CTRL_COUNTFLAG	16

/* The NVIC starts at 0xe000e100, so add that to the offsets to find the absolute address */

struct stm_nvic {
	vuint32_t	iser[3];	/* 0x000 0xe000e100 Set Enable Register */

	uint8_t		_unused00c[0x080 - 0x00c];

	vuint32_t	icer[3];	/* 0x080 0xe000e180 Clear Enable Register */

	uint8_t		_unused08c[0x100 - 0x08c];

	vuint32_t	ispr[3];	/* 0x100 0xe000e200 Set Pending Register */

	uint8_t		_unused10c[0x180 - 0x10c];

	vuint32_t	icpr[3];	/* 0x180 0xe000e280 Clear Pending Register */

	uint8_t		_unused18c[0x200 - 0x18c];

	vuint32_t	iabr[3];	/* 0x200 0xe000e300 Active Bit Register */

	uint8_t		_unused20c[0x300 - 0x20c];

	vuint32_t	ipr[31];	/* 0x300 0xe000e400 Priority Register */

	uint8_t		_unused37c[0xe00 - 0x37c];	/* covers SCB */

	vuint32_t	stir;		/* 0xe00 0xe000ee00 Software Trigger Interrupt Register */
};

extern struct stm_nvic stm_nvic;

//#define stm_nvic (*((struct stm_nvic *) 0xe000e100))

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
	vuint32_t	shcrs;
	vuint32_t	cfsr;
	vuint32_t	hfsr;

	uint32_t	unused_30;
	vuint32_t	mmar;
	vuint32_t	bfar;
};

extern struct stm_scb stm_scb;

#define STM_SCB_AIRCR_VECTKEY		16
#define  STM_SCB_AIRCR_VECTKEY_KEY		0x05fa
#define STM_SCB_AIRCR_PRIGROUP		8
#define STM_SCB_AIRCR_SYSRESETREQ	2
#define STM_SCB_AIRCR_VECTCLRACTIVE	1
#define STM_SCB_AIRCR_VECTRESET		0

struct stm_dbgmcu {
	uint32_t	idcode;
};

extern struct stm_dbgmcu	stm_dbgmcu;

static inline uint16_t
stm_dev_id(void) {
	return stm_dbgmcu.idcode & 0xfff;
}

struct stm_flash {
	vuint32_t	acr;
	vuint32_t	keyr;
	vuint32_t	optkeyr;
	vuint32_t	sr;

	vuint32_t	cr;
	vuint32_t	ar;
	uint32_t	_unused018;
	vuint32_t	obr;

	vuint32_t	wrpr;
};

extern struct stm_flash stm_flash;

//#define stm_flash (*((struct stm_flash *) 0x40022000))

#define STM_FLASH_ACR_PRFTBS	5
#define STM_FLASH_ACR_PRFTBE	4
#define STM_FLASH_ACR_HLFCYA	3
#define STM_FLASH_ACR_LATENCY	0
#define  STM_FLASH_ACR_LATENCY_0	0
#define  STM_FLASH_ACR_LATENCY_1	1
#define  STM_FLASH_ACR_LATENCY_2	2

#define STM_FLASH_SR_EOP	5
#define STM_FLASH_SR_WRPRTERR	4
#define STM_FLASH_SR_PGERR	2
#define STM_FLASH_SR_BSY	0

#define STM_FLASH_CR_EOPIE	12
#define STM_FLASH_CR_ERRIE	10
#define STM_FLASH_CR_OPTWRE	9
#define STM_FLASH_CR_LOCK	7
#define STM_FLASH_CR_STRT	6
#define STM_FLASH_CR_OPTER	5
#define STM_FLASH_CR_OPTPG	4
#define STM_FLASH_CR_MER	2
#define STM_FLASH_CR_PER	1
#define STM_FLASH_CR_PG		0

#define STM_FLASH_RDPRT_KEY	0x00A5
#define STM_FLASH_KEYR_KEY1	0x45670123
#define STM_FLASH_KEYR_KEY2	0xCDEF89AB


struct stm_flash_data {
	vuint16_t	f_size;
	vuint16_t	unused02;
	vuint32_t	unused04;
	vuint32_t	device_id[3];
};

extern struct stm_flash_data	stm_flash_data;

static inline uint32_t stm_flash_size(void) { return (uint32_t) stm_flash_data.f_size * 1024; }

//#define stm_flash_data	(*((struct stm_flash_data *) 0x1ffff7e0))

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

#define STM_AFIO_MAPR_ADC2_ETRGREG_REMAP	20
#define STM_AFIO_MAPR_ADC2_ETRGINJ_REMAP	19
#define STM_AFIO_MAPR_ADC1_ETRGREG_REMAP	18
#define STM_AFIO_MAPR_ADC1_ETRGINJ_REMAP	17
#define STM_AFIO_MAPR_TIM5CH4_IREMAP		16
#define STM_AFIO_MAPR_PD01_REMAP		15
#define STM_AFIO_MAPR_CAN_REMAP			13
#define  STM_AFIO_MAPR_CAN_REMAP_PA11_PA12		0
#define  STM_AFIO_MAPR_CAN_REMAP_PB8_PB9		2
#define  STM_AFIO_MAPR_CAN_REMAP_PD0_PD1		3
#define  STM_AFIO_MAPR_CAN_REMAP_MASK			3
#define STM_AFIO_MAPR_TIM4_REMAP		12
#define STM_AFIO_MAPR_TIM3_REMAP		10
#define  STM_AFIO_MAPR_TIM3_REMAP_PA6_PA7_PB0_PB1	0
#define  STM_AFIO_MAPR_TIM3_REMAP_PB4_PB5_PB0_PB1	2
#define  STM_AFIO_MAPR_TIM3_REMAP_PC6_PC7_PC8_PC9	3
#define  STM_AFIO_MAPR_TIM3_REMAP_MASK			3
#define STM_AFIO_MAPR_TIM2_REMAP		8
#define  STM_AFIO_MAPR_TIM2_REMAP_PA0_PA1_PA2_PA3	0
#define  STM_AFIO_MAPR_TIM2_REMAP_PA15_PB3_PA2_PA3	1
#define  STM_AFIO_MAPR_TIM2_REMAP_PA0_PA1_PB10_PB11	2
#define  STM_AFIO_MAPR_TIM2_REMAP_PA15_PB3_PB10_PB11	3
#define  STM_AFIO_MAPR_TIM2_REMAP_MASK			3
#define STM_AFIO_MAPR_TIM1_REMAP		6
#define  STM_AFIO_MAPR_TIM1_REMAP_PA12_PA8_PA9_PA10_PA11_PB12_PB13_PB14_PB15	0
#define  STM_AFIO_MAPR_TIM1_REMAP_PA12_PA8_PA9_PA10_PA11_PA6_PA7_PB0_PB1	1
#define  STM_AFIO_MAPR_TIM1_REMAP_PE7_PE9_PE11_PE13_PE14_PE15_PE8_PE10_PE12	3
#define  STM_AFIO_MAPR_TIM1_REMAP_MASK						3
#define STM_AFIO_MAPR_USART3_REMAP		4
#define  STM_AFIO_MAPR_USART3_REMAP_PB10_PB11_PB12_PB13_PB14	0
#define  STM_AFIO_MAPR_USART3_REMAP_PC10_PC11_PC12_PB13_PB14	1
#define  STM_AFIO_MAPR_USART3_REMAP_PD8_PD9_PD10_PD11_PD12	3
#define  STM_AFIO_MAPR_USART3_REMAP_MASK			3
#define STM_AFIO_MAPR_USART2_REMAP		3
#define  STM_AFIO_MAPR_USART2_REMAP_PA0_PA1_PA2_PA3_PA4	0
#define  STM_AFIO_MAPR_USART2_REMAP_PD3_PD4_PD5_PD6_PD7	1
#define  STM_AFIO_MAPR_USART2_REMAP_MASK		1
#define STM_AFIO_MAPR_USART1_REMAP		2
#define  STM_AFIO_MAPR_USART1_REMAP_PA9_PA10		0
#define  STM_AFIO_MAPR_USART1_REMAP_PB6_PB7		1
#define  STM_AFIO_MAPR_USART1_REMAP_MASK		1
#define STM_AFIO_MAPR_I2C1_REMAP		1
#define  STM_AFIO_MAPR_I2C1_REMAP_PB6_PB7		0
#define  STM_AFIO_MAPR_I2C1_REMAP_PB8_PB9		1
#define  STM_AFIO_MAPR_I2C1_REMAP_MASK			1
#define STM_AFIO_MAPR_SPI1_REMAP		0
#define  STM_AFIO_MAPR_SPI1_REMAP_PA4_PA5_PA6_PA7	0
#define  STM_AFIO_MAPR_SPI1_REMAP_PA15_PB3_PB4_PB5	1
#define  STM_AFIO_MAPR_SPI1_REMAP_MASK			1

#define STM_AFIO_EXTICR_PA		0
#define STM_AFIO_EXTICR_PB		1
#define STM_AFIO_EXTICR_PC		2
#define STM_AFIO_EXTICR_PD		3
#define STM_AFIO_EXTICR_PE		4
#define STM_AFIO_EXTICR_PF		5
#define STM_AFIO_EXTICR_PG		6

static inline void
stm_set_afio_mapr(uint8_t bit, uint32_t val, uint32_t mask) {
	uint32_t	mapr = stm_afio.mapr;

	mapr &= ~(mask << bit);
	mapr |= (val << bit);
	stm_afio.mapr = mapr;
}

struct stm_usart {
	vuint32_t	sr;	/* status register */
	vuint32_t	dr;	/* data register */
	vuint32_t	brr;	/* baud rate register */
	vuint32_t	cr1;	/* control register 1 */

	vuint32_t	cr2;	/* control register 2 */
	vuint32_t	cr3;	/* control register 3 */
	vuint32_t	gtpr;	/* guard time and prescaler */
};

extern struct stm_usart stm_usart1;
extern struct stm_usart stm_usart2;
extern struct stm_usart stm_usart3;

//#define stm_usart1	(*((struct stm_usart *) 0x40013800))
//#define stm_usart2	(*((struct stm_usart *) 0x40004800))
//#define stm_usart3	(*((struct stm_usart *) 0x40004400))

#define STM_USART_SR_CTS	(9)	/* CTS flag */
#define STM_USART_SR_LBD	(8)	/* LIN break detection flag */
#define STM_USART_SR_TXE	(7)	/* Transmit data register empty */
#define STM_USART_SR_TC		(6)	/* Transmission complete */
#define STM_USART_SR_RXNE	(5)	/* Read data register not empty */
#define STM_USART_SR_IDLE	(4)	/* IDLE line detected */
#define STM_USART_SR_ORE	(3)	/* Overrun error */
#define STM_USART_SR_NE		(2)	/* Noise detected flag */
#define STM_USART_SR_FE		(1)	/* Framing error */
#define STM_USART_SR_PE		(0)	/* Parity error */

#define STM_USART_BRR_DIV_MANTISSA	(4)
#define STM_USART_BRR_DIV_FRACTION	(0)

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
#define STM_USART_CR2_STOP_MASK	3UL
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
#define STM_USART_CR2_ADD_MASK	0xfUL

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

struct stm_usb {
	vuint32_t	epr[8];
	uint8_t		reserved_20[0x40 - 0x20];
	vuint32_t	cntr;
	vuint32_t	istr;
	vuint32_t	fnr;
	vuint32_t	daddr;
	vuint32_t	btable;
};

/*
 * USB DM: PA11
 * USB DP: PA12
 *
 * Need a pull-up on a separate GPIO
 */
#define STM_USB_EPR_CTR_RX	15
#define  STM_USB_EPR_CTR_RX_WRITE_INVARIANT		1
#define STM_USB_EPR_DTOG_RX	14
#define STM_USB_EPR_DTOG_RX_WRITE_INVARIANT		0
#define STM_USB_EPR_STAT_RX	12
#define  STM_USB_EPR_STAT_RX_DISABLED			0
#define  STM_USB_EPR_STAT_RX_STALL			1
#define  STM_USB_EPR_STAT_RX_NAK			2
#define  STM_USB_EPR_STAT_RX_VALID			3
#define  STM_USB_EPR_STAT_RX_MASK			3UL
#define  STM_USB_EPR_STAT_RX_WRITE_INVARIANT		0
#define STM_USB_EPR_SETUP	11
#define STM_USB_EPR_EP_TYPE	9
#define  STM_USB_EPR_EP_TYPE_BULK			0
#define  STM_USB_EPR_EP_TYPE_CONTROL			1
#define  STM_USB_EPR_EP_TYPE_ISO			2
#define  STM_USB_EPR_EP_TYPE_INTERRUPT			3
#define  STM_USB_EPR_EP_TYPE_MASK			3UL
#define STM_USB_EPR_EP_KIND	8
#define  STM_USB_EPR_EP_KIND_DBL_BUF			1	/* Bulk */
#define  STM_USB_EPR_EP_KIND_STATUS_OUT			1	/* Control */
#define STM_USB_EPR_CTR_TX	7
#define  STM_USB_CTR_TX_WRITE_INVARIANT			1
#define STM_USB_EPR_DTOG_TX	6
#define  STM_USB_EPR_DTOG_TX_WRITE_INVARIANT		0
#define STM_USB_EPR_STAT_TX	4
#define  STM_USB_EPR_STAT_TX_DISABLED			0
#define  STM_USB_EPR_STAT_TX_STALL			1
#define  STM_USB_EPR_STAT_TX_NAK			2
#define  STM_USB_EPR_STAT_TX_VALID			3
#define  STM_USB_EPR_STAT_TX_WRITE_INVARIANT		0
#define  STM_USB_EPR_STAT_TX_MASK			3UL
#define STM_USB_EPR_EA		0
#define  STM_USB_EPR_EA_MASK				0xfUL

#define STM_USB_CNTR_CTRM	15
#define STM_USB_CNTR_PMAOVRM	14
#define STM_USB_CNTR_ERRM	13
#define STM_USB_CNTR_WKUPM	12
#define STM_USB_CNTR_SUSPM	11
#define STM_USB_CNTR_RESETM	10
#define STM_USB_CNTR_SOFM	9
#define STM_USB_CNTR_ESOFM	8
#define STM_USB_CNTR_RESUME	4
#define STM_USB_CNTR_FSUSP	3
#define STM_USB_CNTR_LP_MODE	2
#define STM_USB_CNTR_PDWN	1
#define STM_USB_CNTR_FRES	0

#define STM_USB_ISTR_CTR	15
#define STM_USB_ISTR_PMAOVR	14
#define STM_USB_ISTR_ERR	13
#define STM_USB_ISTR_WKUP	12
#define STM_USB_ISTR_SUSP	11
#define STM_USB_ISTR_RESET	10
#define STM_USB_ISTR_SOF	9
#define STM_USB_ISTR_ESOF	8
#define STM_USB_ISTR_DIR	4
#define STM_USB_ISTR_EP_ID	0
#define  STM_USB_ISTR_EP_ID_MASK		0xfUL

#define STM_USB_FNR_RXDP	15
#define STM_USB_FNR_RXDM	14
#define STM_USB_FNR_LCK		13
#define STM_USB_FNR_LSOF	11
#define  STM_USB_FNR_LSOF_MASK			0x3UL
#define STM_USB_FNR_FN		0
#define  STM_USB_FNR_FN_MASK			0x7ffUL

#define STM_USB_DADDR_EF	7
#define STM_USB_DADDR_ADD	0
#define  STM_USB_DADDR_ADD_MASK			0x7fUL

extern struct stm_usb stm_usb;

#define stm_usb (*((struct stm_usb *) 0x40005c00))

union stm_usb_bdt {
	struct {
		vuint32_t	addr_tx;
		vuint32_t	count_tx;
		vuint32_t	addr_rx;
		vuint32_t	count_rx;
	} single;
	struct {
		vuint32_t	addr;
		vuint32_t	count;
	} double_tx[2];
	struct {
		vuint32_t	addr;
		vuint32_t	count;
	} double_rx[2];
};

#define STM_USB_BDT_COUNT_RX_BL_SIZE	15
#define STM_USB_BDT_COUNT_RX_NUM_BLOCK	10
#define  STM_USB_BDT_COUNT_RX_NUM_BLOCK_MASK	0x1fUL
#define STM_USB_BDT_COUNT_RX_COUNT_RX	0
#define  STM_USB_BDT_COUNT_RX_COUNT_RX_MASK	0x3ffUL

#define STM_USB_BDT_SIZE	8

extern uint8_t stm_usb_sram[] __attribute__ ((aligned(4)));

//#define stm_usb_sram ((uint8_t *)0x40006000)

struct stm_dma_channel {
	vuint32_t	ccr;
	vuint32_t	cndtr;
	vvoid_t		cpar;
	vvoid_t		cmar;
	vuint32_t	reserved;
};

#define STM_NUM_DMA	7

struct stm_dma {
	vuint32_t		isr;
	vuint32_t		ifcr;
	struct stm_dma_channel	channel[STM_NUM_DMA];
};

extern struct stm_dma stm_dma;

#define stm_dma	(*((struct stm_dma *) 0x40020000))

/* DMA channels go from 1 to 7, instead of 0 to 6 (sigh)
 */

#define STM_DMA_INDEX(channel)		((channel) - 1)

#define STM_DMA_ISR(index)		((index) << 2)
#define STM_DMA_ISR_MASK			0xfUL
#define STM_DMA_ISR_TEIF			3
#define STM_DMA_ISR_HTIF			2
#define STM_DMA_ISR_TCIF			1
#define STM_DMA_ISR_GIF				0

#define STM_DMA_IFCR(index)		((index) << 2)
#define STM_DMA_IFCR_MASK			0xfUL
#define STM_DMA_IFCR_CTEIF      		3
#define STM_DMA_IFCR_CHTIF			2
#define STM_DMA_IFCR_CTCIF			1
#define STM_DMA_IFCR_CGIF			0

#define STM_DMA_CCR_MEM2MEM		(14)

#define STM_DMA_CCR_PL			(12)
#define  STM_DMA_CCR_PL_LOW			(0)
#define  STM_DMA_CCR_PL_MEDIUM			(1)
#define  STM_DMA_CCR_PL_HIGH			(2)
#define  STM_DMA_CCR_PL_VERY_HIGH		(3)
#define  STM_DMA_CCR_PL_MASK			(3)

#define STM_DMA_CCR_MSIZE		(10)
#define  STM_DMA_CCR_MSIZE_8			(0)
#define  STM_DMA_CCR_MSIZE_16			(1)
#define  STM_DMA_CCR_MSIZE_32			(2)
#define  STM_DMA_CCR_MSIZE_MASK			(3)

#define STM_DMA_CCR_PSIZE		(8)
#define  STM_DMA_CCR_PSIZE_8			(0)
#define  STM_DMA_CCR_PSIZE_16			(1)
#define  STM_DMA_CCR_PSIZE_32			(2)
#define  STM_DMA_CCR_PSIZE_MASK			(3)

#define STM_DMA_CCR_MINC		(7)
#define STM_DMA_CCR_PINC		(6)
#define STM_DMA_CCR_CIRC		(5)
#define STM_DMA_CCR_DIR			(4)
#define  STM_DMA_CCR_DIR_PER_TO_MEM		0
#define  STM_DMA_CCR_DIR_MEM_TO_PER		1
#define STM_DMA_CCR_TEIE		(3)
#define STM_DMA_CCR_HTIE		(2)
#define STM_DMA_CCR_TCIE		(1)
#define STM_DMA_CCR_EN			(0)

#define STM_DMA_CHANNEL_ADC1		1
#define STM_DMA_CHANNEL_SPI1_RX		2
#define STM_DMA_CHANNEL_SPI1_TX		3
#define STM_DMA_CHANNEL_SPI2_RX		4
#define STM_DMA_CHANNEL_SPI2_TX		5
#define STM_DMA_CHANNEL_USART3_TX	2
#define STM_DMA_CHANNEL_USART3_RX	3
#define STM_DMA_CHANNEL_USART1_TX	4
#define STM_DMA_CHANNEL_USART1_RX	5
#define STM_DMA_CHANNEL_USART2_RX	6
#define STM_DMA_CHANNEL_USART2_TX	7
#define STM_DMA_CHANNEL_I2C2_TX		4
#define STM_DMA_CHANNEL_I2C2_RX		5
#define STM_DMA_CHANNEL_I2C1_TX		6
#define STM_DMA_CHANNEL_I2C1_RX		7
#define STM_DMA_CHANNEL_TIM1_CH1	2
#define STM_DMA_CHANNEL_TIM1_CH4	4
#define STM_DMA_CHANNEL_TIM1_TRIG	4
#define STM_DMA_CHANNEL_TIM1_COM	4
#define STM_DMA_CHANNEL_TIM1_UP		5
#define STM_DMA_CHANNEL_TIM1_CH3	6
#define STM_DMA_CHANNEL_TIM2_CH3	1
#define STM_DMA_CHANNEL_TIM2_UP		2
#define STM_DMA_CHANNEL_TIM2_CH1	5
#define STM_DMA_CHANNEL_TIM2_CH2	7
#define STM_DMA_CHANNEL_TIM2_CH4	7
#define STM_DMA_CHANNEL_TIM3_CH3	2
#define STM_DMA_CHANNEL_TIM3_CH4	3
#define STM_DMA_CHANNEL_TIM3_UP		3
#define STM_DMA_CHANNEL_TIM3_CH1	6
#define STM_DMA_CHANNEL_TIM3_TRIG	6
#define STM_DMA_CHANNEL_TIM4_CH1	1
#define STM_DMA_CHANNEL_TIM4_CH2	4
#define STM_DMA_CHANNEL_TIM4_CH3	5
#define STM_DMA_CHANNEL_TIM4_UP		7

/* high density, xl-density and connectivity devices also have dma2 */

#define STM_DMA2_CHANNEL_ADC3		5
#define STM_DMA2_CHANNEL_SPI3_RX	1
#define STM_DMA2_CHANNEL_SPI3_TX	2
#define STM_DMA2_CHANNEL_UART4_RX	3
#define STM_DMA2_CHANNEL_UART4_TX	5
#define STM_DMA2_CHANNEL_TIM5_CH4	1
#define STM_DMA2_CHANNEL_TIM5_TRIG	1
#define STM_DMA2_CHANNEL_TIM5_CH3	2
#define STM_DMA2_CHANNEL_TIM5_UP	2
#define STM_DMA2_CHANNEL_TIM5_CH2	4
#define STM_DMA2_CHANNEL_TIM5_CH1	5
#define STM_DMA2_CHANNEL_TIM6_UP	3
#define STM_DMA2_CHANNEL_DAC_CHANNEL1	3
#define STM_DMA2_CHANNEL_TIM7_UP	4
#define STM_DMA2_CHANNEL_DAC_CHANNEL2	4
#define STM_DMA2_CHANNEL_TIM8_CH3	1
#define STM_DMA2_CHANNEL_TIM8_UP	1
#define STM_DMA2_CHANNEL_TIM8_CH4	2
#define STM_DMA2_CHANNEL_TIM8_TRIG	2
#define STM_DMA2_CHANNEL_TIM8_COM	2
#define STM_DMA2_CHANNEL_TIM8_CH1	3
#define STM_DMA2_CHANNEL_TIM8_CH2	5

struct stm_spi {
	vuint32_t	cr1;
	vuint32_t	cr2;
	vuint32_t	sr;
	vuint32_t	dr;

	vuint32_t	crcpr;
	vuint32_t	rxcrcr;
	vuint32_t	txcrcr;
	vuint32_t	i2scfgr;

	vuint32_t	i2spr;
};

extern struct stm_spi stm_spi1, stm_spi2;

#define stm_spi1 (*((struct stm_spi *) 0x40013000))
#define stm_spi2 (*((struct stm_spi *) 0x40003800))

/* SPI channels go from 1 to 2, instead of 0 to 1 (sigh)
 */

#define STM_NUM_SPI	2

#define STM_SPI_INDEX(channel)		((channel) - 1)

#define STM_SPI_CR1_BIDIMODE		15
#define STM_SPI_CR1_BIDIOE		14
#define STM_SPI_CR1_CRCEN		13
#define STM_SPI_CR1_CRCNEXT		12
#define STM_SPI_CR1_DFF			11
#define STM_SPI_CR1_RXONLY		10
#define STM_SPI_CR1_SSM			9
#define STM_SPI_CR1_SSI			8
#define STM_SPI_CR1_LSBFIRST		7
#define STM_SPI_CR1_SPE			6
#define STM_SPI_CR1_BR			3
#define  STM_SPI_CR1_BR_PCLK_2			0
#define  STM_SPI_CR1_BR_PCLK_4			1
#define  STM_SPI_CR1_BR_PCLK_8			2
#define  STM_SPI_CR1_BR_PCLK_16			3
#define  STM_SPI_CR1_BR_PCLK_32			4
#define  STM_SPI_CR1_BR_PCLK_64			5
#define  STM_SPI_CR1_BR_PCLK_128		6
#define  STM_SPI_CR1_BR_PCLK_256		7
#define  STM_SPI_CR1_BR_MASK			7UL

#define STM_SPI_CR1_MSTR		2
#define STM_SPI_CR1_CPOL		1
#define STM_SPI_CR1_CPHA		0

#define STM_SPI_CR2_TXEIE	7
#define STM_SPI_CR2_RXNEIE	6
#define STM_SPI_CR2_ERRIE	5
#define STM_SPI_CR2_SSOE	2
#define STM_SPI_CR2_TXDMAEN	1
#define STM_SPI_CR2_RXDMAEN	0

#define STM_SPI_SR_FRE		8
#define STM_SPI_SR_BSY		7
#define STM_SPI_SR_OVR		6
#define STM_SPI_SR_MODF		5
#define STM_SPI_SR_CRCERR	4
#define STM_SPI_SR_UDR		3
#define STM_SPI_SR_CHSIDE	2
#define STM_SPI_SR_TXE		1
#define STM_SPI_SR_RXNE		0

#define STM_NUM_I2C	2

#define STM_I2C_INDEX(channel)	((channel) - 1)

struct stm_i2c {
	vuint32_t	cr1;
	vuint32_t	cr2;
	vuint32_t	oar1;
	vuint32_t	oar2;
	vuint32_t	dr;
	vuint32_t	sr1;
	vuint32_t	sr2;
	vuint32_t	ccr;
	vuint32_t	trise;
};

extern struct stm_i2c stm_i2c1, stm_i2c2;

#define stm_i2c1	(*((struct stm_i2c *) 0x40005400))
#define stm_i2c2	(*((struct stm_i2c *) 0x40005800))

#define STM_I2C_CR1_SWRST	15
#define STM_I2C_CR1_ALERT	13
#define STM_I2C_CR1_PEC		12
#define STM_I2C_CR1_POS		11
#define STM_I2C_CR1_ACK		10
#define STM_I2C_CR1_STOP	9
#define STM_I2C_CR1_START	8
#define STM_I2C_CR1_NOSTRETCH	7
#define STM_I2C_CR1_ENGC	6
#define STM_I2C_CR1_ENPEC	5
#define STM_I2C_CR1_ENARP	4
#define STM_I2C_CR1_SMBTYPE	3
#define STM_I2C_CR1_SMBUS	1
#define STM_I2C_CR1_PE		0

#define STM_I2C_CR2_LAST	12
#define STM_I2C_CR2_DMAEN	11
#define STM_I2C_CR2_ITBUFEN	10
#define STM_I2C_CR2_ITEVTEN	9
#define STM_I2C_CR2_ITERREN	8
#define STM_I2C_CR2_FREQ	0
#define  STM_I2C_CR2_FREQ_MASK		0x3fUL

#define STM_I2C_SR1_SMBALERT	15
#define STM_I2C_SR1_TIMEOUT	14
#define STM_I2C_SR1_PECERR	12
#define STM_I2C_SR1_OVR		11
#define STM_I2C_SR1_AF		10
#define STM_I2C_SR1_ARLO	9
#define STM_I2C_SR1_BERR	8
#define STM_I2C_SR1_TXE		7
#define STM_I2C_SR1_RXNE	6
#define STM_I2C_SR1_STOPF	4
#define STM_I2C_SR1_ADD10	3
#define STM_I2C_SR1_BTF		2
#define STM_I2C_SR1_ADDR	1
#define STM_I2C_SR1_SB		0

#define STM_I2C_SR2_PEC		8
#define  STM_I2C_SR2_PEC_MASK	0xff00UL
#define STM_I2C_SR2_DUALF	7
#define STM_I2C_SR2_SMBHOST	6
#define STM_I2C_SR2_SMBDEFAULT	5
#define STM_I2C_SR2_GENCALL	4
#define STM_I2C_SR2_TRA		2
#define STM_I2C_SR2_BUSY       	1
#define STM_I2C_SR2_MSL		0

#define STM_I2C_CCR_FS		15
#define STM_I2C_CCR_DUTY	14
#define STM_I2C_CCR_CCR		0
#define  STM_I2C_CCR_MASK	0x7ffUL

struct stm_adc {
	vuint32_t	sr;
	vuint32_t	cr1;
	vuint32_t	cr2;
	vuint32_t	smpr1;

	vuint32_t	smpr2;
	vuint32_t	jofr1;
	vuint32_t	jofr2;
	vuint32_t	jofr3;

	vuint32_t	jofr4;
	vuint32_t	htr;
	vuint32_t	ltr;
	vuint32_t	sqr1;

	vuint32_t	sqr2;
	vuint32_t	sqr3;
	vuint32_t	jsqr;
	vuint32_t	jdr1;

	vuint32_t	jdr2;
	vuint32_t	jdr3;
	vuint32_t	jdr4;
	vuint32_t	dr;
};

extern struct stm_adc stm_adc;

#define stm_adc (*((struct stm_adc *) 0x40012400))

#define STM_ADC_SQ_TEMP		16
#define STM_ADC_SQ_V_REF	17

#define STM_ADC_SR_STRT		4
#define STM_ADC_SR_JSTRT	3
#define STM_ADC_SR_JEOC		2
#define STM_ADC_SR_EOC		1
#define STM_ADC_SR_AWD		0

#define STM_ADC_CR1_AWDEN       23
#define STM_ADC_CR1_JAWDEN	22
#define STM_ADC_CR1_DUALMOD	16
# define STM_ADC_CR1_DUALMOD_INDEPENDENT		0
# define STM_ADC_CR1_DUALMOD_COMB_REG_SIM_INJ_SIM	1
# define STM_ADC_CR1_DUALMOD_COMB_REG_SIM_ALT_TRIG	2
# define STM_ADC_CR1_DUALMOD_COMB_INJ_SIM_FAST_INT	3
# define STM_ADC_CR1_DUALMOD_COMB_INJ_SIM_SLOW_INT	4
# define STM_ADC_CR1_DUALMOD_INJ_SIM			5
# define STM_ADC_CR1_DUALMOD_REG_SIM			6
# define STM_ADC_CR1_DUALMOD_FAST_INT			7
# define STM_ADC_CR1_DUALMOD_SLOW_INT			8
# define STM_ADC_CR1_DUALMOD_ALT_TRIG			9

#define STM_ADC_CR1_DISCNUM	13
#define  STM_ADC_CR1_DISCNUM_1		0
#define  STM_ADC_CR1_DISCNUM_2		1
#define  STM_ADC_CR1_DISCNUM_3		2
#define  STM_ADC_CR1_DISCNUM_4		3
#define  STM_ADC_CR1_DISCNUM_5		4
#define  STM_ADC_CR1_DISCNUM_6		5
#define  STM_ADC_CR1_DISCNUM_7		6
#define  STM_ADC_CR1_DISCNUM_8		7
#define  STM_ADC_CR1_DISCNUM_MASK	7UL
#define STM_ADC_CR1_JDISCEN	12
#define STM_ADC_CR1_DISCEN	11
#define STM_ADC_CR1_JAUTO	10
#define STM_ADC_CR1_AWDSGL	9
#define STM_ADC_CR1_SCAN	8
#define STM_ADC_CR1_JEOCIE	7
#define STM_ADC_CR1_AWDIE	6
#define STM_ADC_CR1_EOCIE	5
#define STM_ADC_CR1_AWDCH	0
#define  STM_ADC_CR1_AWDCH_MASK		0x1fUL

#define STM_ADC_CR2_TSVREF	23
#define STM_ADC_CR2_SWSTART	21
#define STM_ADC_CR2_JWSTART	21
#define STM_ADC_CR2_EXTTRIG	20
#define STM_ADC_CR2_EXTSEL	17
#define  STM_ADC_CR2_EXTSEL_TIM1_CC1	0
#define  STM_ADC_CR2_EXTSEL_TIM1_CC2	1
#define  STM_ADC_CR2_EXTSEL_TIM1_CC3	2
#define  STM_ADC_CR2_EXTSEL_TIM2_CC2	3
#define  STM_ADC_CR2_EXTSEL_TIM3_TRGO	4
#define  STM_ADC_CR2_EXTSEL_TIM4_CC4	5
#define  STM_ADC_CR2_EXTSEL_EXTI	6
#define  STM_ADC_CR2_EXTSEL_SWSTART	7
#define  STM_ADC_CR2_EXTSEL_MASK	7UL
#define STM_ADC_CR2_JEXTTRIG	15
#define STM_ADC_CR2_JEXTSEL	12
#define  STM_ADC_CR2_JEXTSEL_TIM1_TRGO	0
#define  STM_ADC_CR2_JEXTSEL_TIM1_CC4	1
#define  STM_ADC_CR2_JEXTSEL_TIM2_TRGO	2
#define  STM_ADC_CR2_JEXTSEL_TIM2_CC1	3
#define  STM_ADC_CR2_JEXTSEL_TIM3_CC4	4
#define  STM_ADC_CR2_JEXTSEL_TIM4_TRGO	5
#define  STM_ADC_CR2_JEXTSEL_EXTI_15	6
#define  STM_ADC_CR2_JEXTSEL_JSWSTART	7
#define  STM_ADC_CR2_JEXTSEL_MASK	7UL
#define STM_ADC_CR2_ALIGN	11
#define STM_ADC_CR2_DMA		8
#define STM_ADC_CR2_RSTCAL	3
#define STM_ADC_CR2_CAL		2
#define STM_ADC_CR2_CONT	1
#define STM_ADC_CR2_ADON	0

struct stm_exti {
	vuint32_t	imr;
	vuint32_t	emr;
	vuint32_t	rtsr;
	vuint32_t	ftsr;

	vuint32_t	swier;
	vuint32_t	pr;
};

extern struct stm_exti stm_exti;

#define stm_exti	(*((struct stm_exti *) 0x40010400))

static inline void
stm_exticr_set(struct stm_gpio *gpio, int pin) {
	uint8_t	reg = (uint8_t) (pin >> 2);
	uint8_t	shift = (pin & 3) << 2;
	uint8_t	val = 0;

	/* Enable AFIO */
	stm_rcc.apb2enr |= (1 << STM_RCC_APB2ENR_AFIOEN);

	if (gpio == &stm_gpioa)
		val = STM_AFIO_EXTICR_PA;
	else if (gpio == &stm_gpiob)
		val = STM_AFIO_EXTICR_PB;
	else if (gpio == &stm_gpioc)
		val = STM_AFIO_EXTICR_PC;
	else if (gpio == &stm_gpiod)
		val = STM_AFIO_EXTICR_PD;
	else if (gpio == &stm_gpioe)
		val = STM_AFIO_EXTICR_PE;

	stm_afio.exticr[reg] = (stm_afio.exticr[reg] & (uint32_t) ~(0xf << shift)) | val << shift;
}

struct stm_tim18 {
	vuint32_t	cr1;
	vuint32_t	cr2;
	vuint32_t	smcr;
	vuint32_t	dier;

	vuint32_t	sr;
	vuint32_t	egr;
	vuint32_t	ccmr1;
	vuint32_t	ccmr2;

	vuint32_t	ccer;
	vuint32_t	cnt;
	vuint32_t	psc;
	vuint32_t	arr;

	vuint32_t	rcr;
	vuint32_t	ccr1;
	vuint32_t	ccr2;
	vuint32_t	ccr3;

	vuint32_t	ccr4;
	uint32_t	bdtr;
	vuint32_t	dcr;
	vuint32_t	dmar;
};

extern struct stm_tim18 stm_tim1, stm_tim8;

#define stm_tim1	(*((struct stm_tim18 *) 0x40012c00))
#define stm_tim8	(*((struct stm_tim18 *) 0x40013400))

#define STM_TIM18_CR1_CKD	8
#define  STM_TIM18_CR1_CKD_1		0
#define  STM_TIM18_CR1_CKD_2		1
#define  STM_TIM18_CR1_CKD_4		2
#define  STM_TIM18_CR1_CKD_MASK	3UL
#define STM_TIM18_CR1_ARPE	7
#define STM_TIM18_CR1_CMS	5
#define  STM_TIM18_CR1_CMS_EDGE	0
#define  STM_TIM18_CR1_CMS_CENTER_1	1
#define  STM_TIM18_CR1_CMS_CENTER_2	2
#define  STM_TIM18_CR1_CMS_CENTER_3	3
#define  STM_TIM18_CR1_CMS_MASK	3UL
#define STM_TIM18_CR1_DIR	4
#define  STM_TIM18_CR1_DIR_UP		0
#define  STM_TIM18_CR1_DIR_DOWN	1
#define STM_TIM18_CR1_OPM	3
#define STM_TIM18_CR1_URS	2
#define STM_TIM18_CR1_UDIS	1
#define STM_TIM18_CR1_CEN	0

#define STM_TIM18_CR2_TI1S	7
#define STM_TIM18_CR2_MMS	4
#define  STM_TIM18_CR2_MMS_RESET		0
#define  STM_TIM18_CR2_MMS_ENABLE		1
#define  STM_TIM18_CR2_MMS_UPDATE		2
#define  STM_TIM18_CR2_MMS_COMPARE_PULSE	3
#define  STM_TIM18_CR2_MMS_COMPARE_OC1REF	4
#define  STM_TIM18_CR2_MMS_COMPARE_OC2REF	5
#define  STM_TIM18_CR2_MMS_COMPARE_OC3REF	6
#define  STM_TIM18_CR2_MMS_COMPARE_OC4REF	7
#define  STM_TIM18_CR2_MMS_MASK		7UL
#define STM_TIM18_CR2_CCDS	3

#define STM_TIM18_SMCR_ETP	15
#define STM_TIM18_SMCR_ECE	14
#define STM_TIM18_SMCR_ETPS	12
#define  STM_TIM18_SMCR_ETPS_OFF		0
#define  STM_TIM18_SMCR_ETPS_DIV_2	       	1
#define  STM_TIM18_SMCR_ETPS_DIV_4		2
#define  STM_TIM18_SMCR_ETPS_DIV_8		3
#define  STM_TIM18_SMCR_ETPS_MASK		3UL
#define STM_TIM18_SMCR_ETF	8
#define  STM_TIM18_SMCR_ETF_NONE		0
#define  STM_TIM18_SMCR_ETF_INT_N_2		1
#define  STM_TIM18_SMCR_ETF_INT_N_4		2
#define  STM_TIM18_SMCR_ETF_INT_N_8		3
#define  STM_TIM18_SMCR_ETF_DTS_2_N_6		4
#define  STM_TIM18_SMCR_ETF_DTS_2_N_8		5
#define  STM_TIM18_SMCR_ETF_DTS_4_N_6		6
#define  STM_TIM18_SMCR_ETF_DTS_4_N_8		7
#define  STM_TIM18_SMCR_ETF_DTS_8_N_6		8
#define  STM_TIM18_SMCR_ETF_DTS_8_N_8		9
#define  STM_TIM18_SMCR_ETF_DTS_16_N_5		10
#define  STM_TIM18_SMCR_ETF_DTS_16_N_6		11
#define  STM_TIM18_SMCR_ETF_DTS_16_N_8		12
#define  STM_TIM18_SMCR_ETF_DTS_32_N_5		13
#define  STM_TIM18_SMCR_ETF_DTS_32_N_6		14
#define  STM_TIM18_SMCR_ETF_DTS_32_N_8		15
#define  STM_TIM18_SMCR_ETF_MASK		15UL
#define STM_TIM18_SMCR_MSM	7
#define STM_TIM18_SMCR_TS	4
#define  STM_TIM18_SMCR_TS_ITR0		0
#define  STM_TIM18_SMCR_TS_ITR1		1
#define  STM_TIM18_SMCR_TS_ITR2		2
#define  STM_TIM18_SMCR_TS_ITR3		3
#define  STM_TIM18_SMCR_TS_TI1F_ED		4
#define  STM_TIM18_SMCR_TS_TI1FP1		5
#define  STM_TIM18_SMCR_TS_TI2FP2		6
#define  STM_TIM18_SMCR_TS_ETRF		7
#define  STM_TIM18_SMCR_TS_MASK		7UL
#define STM_TIM18_SMCR_SMS	0
#define  STM_TIM18_SMCR_SMS_DISABLE		0
#define  STM_TIM18_SMCR_SMS_ENCODER_MODE_1	1
#define  STM_TIM18_SMCR_SMS_ENCODER_MODE_2	2
#define  STM_TIM18_SMCR_SMS_ENCODER_MODE_3	3
#define  STM_TIM18_SMCR_SMS_RESET_MODE		4
#define  STM_TIM18_SMCR_SMS_GATED_MODE		5
#define  STM_TIM18_SMCR_SMS_TRIGGER_MODE	6
#define  STM_TIM18_SMCR_SMS_EXTERNAL_CLOCK	7
#define  STM_TIM18_SMCR_SMS_MASK		7UL

#define STM_TIM18_DIER_TDE		14
#define STM_TIM18_DIER_CC4DE		12
#define STM_TIM18_DIER_CC3DE		11
#define STM_TIM18_DIER_CC2DE		10
#define STM_TIM18_DIER_CC1DE		9
#define STM_TIM18_DIER_UDE		8

#define STM_TIM18_DIER_TIE		6
#define STM_TIM18_DIER_CC4IE		4
#define STM_TIM18_DIER_CC3IE		3
#define STM_TIM18_DIER_CC2IE		2
#define STM_TIM18_DIER_CC1IE		1
#define STM_TIM18_DIER_UIE		0

#define STM_TIM18_SR_CC4OF	12
#define STM_TIM18_SR_CC3OF	11
#define STM_TIM18_SR_CC2OF	10
#define STM_TIM18_SR_CC1OF	9
#define STM_TIM18_SR_TIF	6
#define STM_TIM18_SR_CC4IF	4
#define STM_TIM18_SR_CC3IF	3
#define STM_TIM18_SR_CC2IF	2
#define STM_TIM18_SR_CC1IF	1
#define STM_TIM18_SR_UIF	0

#define STM_TIM18_EGR_TG	6
#define STM_TIM18_EGR_CC4G	4
#define STM_TIM18_EGR_CC3G	3
#define STM_TIM18_EGR_CC2G	2
#define STM_TIM18_EGR_CC1G	1
#define STM_TIM18_EGR_UG	0

#define STM_TIM18_CCMR1_OC2CE	15
#define STM_TIM18_CCMR1_OC2M	12
#define  STM_TIM18_CCMR1_OC2M_FROZEN			0
#define  STM_TIM18_CCMR1_OC2M_SET_HIGH_ON_MATCH	1
#define  STM_TIM18_CCMR1_OC2M_SET_LOW_ON_MATCH		2
#define  STM_TIM18_CCMR1_OC2M_TOGGLE			3
#define  STM_TIM18_CCMR1_OC2M_FORCE_LOW		4
#define  STM_TIM18_CCMR1_OC2M_FORCE_HIGH		5
#define  STM_TIM18_CCMR1_OC2M_PWM_MODE_1		6
#define  STM_TIM18_CCMR1_OC2M_PWM_MODE_2		7
#define  STM_TIM18_CCMR1_OC2M_MASK			7UL
#define STM_TIM18_CCMR1_OC2PE	11
#define STM_TIM18_CCMR1_OC2FE	10
#define STM_TIM18_CCMR1_CC2S	8
#define  STM_TIM18_CCMR1_CC2S_OUTPUT			0
#define  STM_TIM18_CCMR1_CC2S_INPUT_TI2		1
#define  STM_TIM18_CCMR1_CC2S_INPUT_TI1		2
#define  STM_TIM18_CCMR1_CC2S_INPUT_TRC		3
#define  STM_TIM18_CCMR1_CC2S_MASK			3UL

#define STM_TIM18_CCMR1_OC1CE	7
#define STM_TIM18_CCMR1_OC1M	4
#define  STM_TIM18_CCMR1_OC1M_FROZEN			0
#define  STM_TIM18_CCMR1_OC1M_SET_HIGH_ON_MATCH	1
#define  STM_TIM18_CCMR1_OC1M_SET_LOW_ON_MATCH		2
#define  STM_TIM18_CCMR1_OC1M_TOGGLE			3
#define  STM_TIM18_CCMR1_OC1M_FORCE_LOW		4
#define  STM_TIM18_CCMR1_OC1M_FORCE_HIGH		5
#define  STM_TIM18_CCMR1_OC1M_PWM_MODE_1		6
#define  STM_TIM18_CCMR1_OC1M_PWM_MODE_2		7
#define  STM_TIM18_CCMR1_OC1M_MASK			7UL
#define STM_TIM18_CCMR1_OC1PE	3
#define STM_TIM18_CCMR1_OC1FE	2
#define STM_TIM18_CCMR1_CC1S	0
#define  STM_TIM18_CCMR1_CC1S_OUTPUT			0
#define  STM_TIM18_CCMR1_CC1S_INPUT_TI1		1
#define  STM_TIM18_CCMR1_CC1S_INPUT_TI2		2
#define  STM_TIM18_CCMR1_CC1S_INPUT_TRC		3
#define  STM_TIM18_CCMR1_CC1S_MASK			3UL

#define STM_TIM18_CCMR1_IC2F	12
#define  STM_TIM18_CCMR1_IC2F_NONE			0
#define  STM_TIM18_CCMR1_IC2F_CK_INT_N_2		1
#define  STM_TIM18_CCMR1_IC2F_CK_INT_N_4		2
#define  STM_TIM18_CCMR1_IC2F_CK_INT_N_8		3
#define  STM_TIM18_CCMR1_IC2F_DTS_2_N_6		4
#define  STM_TIM18_CCMR1_IC2F_DTS_2_N_8		5
#define  STM_TIM18_CCMR1_IC2F_DTS_4_N_6		6
#define  STM_TIM18_CCMR1_IC2F_DTS_4_N_8		7
#define  STM_TIM18_CCMR1_IC2F_DTS_8_N_6		8
#define  STM_TIM18_CCMR1_IC2F_DTS_8_N_8		9
#define  STM_TIM18_CCMR1_IC2F_DTS_16_N_5		10
#define  STM_TIM18_CCMR1_IC2F_DTS_16_N_6		11
#define  STM_TIM18_CCMR1_IC2F_DTS_16_N_8		12
#define  STM_TIM18_CCMR1_IC2F_DTS_32_N_5		13
#define  STM_TIM18_CCMR1_IC2F_DTS_32_N_6		14
#define  STM_TIM18_CCMR1_IC2F_DTS_32_N_8		15
#define STM_TIM18_CCMR1_IC2PSC	10
#define  STM_TIM18_CCMR1_IC2PSC_NONE			0
#define  STM_TIM18_CCMR1_IC2PSC_2			1
#define  STM_TIM18_CCMR1_IC2PSC_4			2
#define  STM_TIM18_CCMR1_IC2PSC_8			3
#define STM_TIM18_CCMR1_IC1F	4
#define  STM_TIM18_CCMR1_IC1F_NONE			0
#define  STM_TIM18_CCMR1_IC1F_CK_INT_N_2		1
#define  STM_TIM18_CCMR1_IC1F_CK_INT_N_4		2
#define  STM_TIM18_CCMR1_IC1F_CK_INT_N_8		3
#define  STM_TIM18_CCMR1_IC1F_DTS_2_N_6		4
#define  STM_TIM18_CCMR1_IC1F_DTS_2_N_8		5
#define  STM_TIM18_CCMR1_IC1F_DTS_4_N_6		6
#define  STM_TIM18_CCMR1_IC1F_DTS_4_N_8		7
#define  STM_TIM18_CCMR1_IC1F_DTS_8_N_6		8
#define  STM_TIM18_CCMR1_IC1F_DTS_8_N_8		9
#define  STM_TIM18_CCMR1_IC1F_DTS_16_N_5		10
#define  STM_TIM18_CCMR1_IC1F_DTS_16_N_6		11
#define  STM_TIM18_CCMR1_IC1F_DTS_16_N_8		12
#define  STM_TIM18_CCMR1_IC1F_DTS_32_N_5		13
#define  STM_TIM18_CCMR1_IC1F_DTS_32_N_6		14
#define  STM_TIM18_CCMR1_IC1F_DTS_32_N_8		15
#define STM_TIM18_CCMR1_IC1PSC	2
#define  STM_TIM18_CCMR1_IC1PSC_NONE			0
#define  STM_TIM18_CCMR1_IC1PSC_2			1
#define  STM_TIM18_CCMR1_IC1PSC_4			2
#define  STM_TIM18_CCMR1_IC1PSC_8			3

#define STM_TIM18_CCMR2_OC4CE	15
#define STM_TIM18_CCMR2_OC4M	12
#define  STM_TIM18_CCMR2_OC4M_FROZEN			0
#define  STM_TIM18_CCMR2_OC4M_SET_HIGH_ON_MATCH	1
#define  STM_TIM18_CCMR2_OC4M_SET_LOW_ON_MATCH		2
#define  STM_TIM18_CCMR2_OC4M_TOGGLE			3
#define  STM_TIM18_CCMR2_OC4M_FORCE_LOW		4
#define  STM_TIM18_CCMR2_OC4M_FORCE_HIGH		5
#define  STM_TIM18_CCMR2_OC4M_PWM_MODE_1		6
#define  STM_TIM18_CCMR2_OC4M_PWM_MODE_2		7
#define  STM_TIM18_CCMR2_OC4M_MASK			7UL
#define STM_TIM18_CCMR2_OC4PE	11
#define STM_TIM18_CCMR2_OC4FE	10
#define STM_TIM18_CCMR2_CC4S	8
#define  STM_TIM18_CCMR2_CC4S_OUTPUT			0
#define  STM_TIM18_CCMR2_CC4S_INPUT_TI4		1
#define  STM_TIM18_CCMR2_CC4S_INPUT_TI3		2
#define  STM_TIM18_CCMR2_CC4S_INPUT_TRC		3
#define  STM_TIM18_CCMR2_CC4S_MASK			3UL

#define STM_TIM18_CCMR2_OC3CE	7
#define STM_TIM18_CCMR2_OC3M	4
#define  STM_TIM18_CCMR2_OC3M_FROZEN			0
#define  STM_TIM18_CCMR2_OC3M_SET_HIGH_ON_MATCH	1
#define  STM_TIM18_CCMR2_OC3M_SET_LOW_ON_MATCH		2
#define  STM_TIM18_CCMR2_OC3M_TOGGLE			3
#define  STM_TIM18_CCMR2_OC3M_FORCE_LOW		4
#define  STM_TIM18_CCMR2_OC3M_FORCE_HIGH		5
#define  STM_TIM18_CCMR2_OC3M_PWM_MODE_1		6
#define  STM_TIM18_CCMR2_OC3M_PWM_MODE_2		7
#define  STM_TIM18_CCMR2_OC3M_MASK			7UL
#define STM_TIM18_CCMR2_OC3PE	3
#define STM_TIM18_CCMR2_OC3FE	2
#define STM_TIM18_CCMR2_CC3S	0
#define  STM_TIM18_CCMR2_CC3S_OUTPUT			0
#define  STM_TIM18_CCMR2_CC3S_INPUT_TI3		1
#define  STM_TIM18_CCMR2_CC3S_INPUT_TI4		2
#define  STM_TIM18_CCMR2_CC3S_INPUT_TRC		3
#define  STM_TIM18_CCMR2_CC3S_MASK			3UL

#define STM_TIM18_CCER_CC4NP	15
#define STM_TIM18_CCER_CC4P	13
#define  STM_TIM18_CCER_CC4P_ACTIVE_HIGH	0
#define  STM_TIM18_CCER_CC4P_ACTIVE_LOW	1
#define STM_TIM18_CCER_CC4E	12
#define STM_TIM18_CCER_CC3NP	11
#define STM_TIM18_CCER_CC3P	9
#define  STM_TIM18_CCER_CC3P_ACTIVE_HIGH	0
#define  STM_TIM18_CCER_CC3P_ACTIVE_LOW	1
#define STM_TIM18_CCER_CC3E	8
#define STM_TIM18_CCER_CC2NP	7
#define STM_TIM18_CCER_CC2P	5
#define  STM_TIM18_CCER_CC2P_ACTIVE_HIGH	0
#define  STM_TIM18_CCER_CC2P_ACTIVE_LOW	1
#define STM_TIM18_CCER_CC2E	4
#define STM_TIM18_CCER_CC1NP	3
#define STM_TIM18_CCER_CC1P	1
#define  STM_TIM18_CCER_CC1P_ACTIVE_HIGH	0
#define  STM_TIM18_CCER_CC1P_ACTIVE_LOW	1
#define STM_TIM18_CCER_CC1E	0

struct stm_tim234 {
	vuint32_t	cr1;
	vuint32_t	cr2;
	vuint32_t	smcr;
	vuint32_t	dier;

	vuint32_t	sr;
	vuint32_t	egr;
	vuint32_t	ccmr1;
	vuint32_t	ccmr2;

	vuint32_t	ccer;
	vuint32_t	cnt;
	vuint32_t	psc;
	vuint32_t	arr;

	uint32_t	reserved_30;
	vuint32_t	ccr1;
	vuint32_t	ccr2;
	vuint32_t	ccr3;

	vuint32_t	ccr4;
	uint32_t	reserved_44;
	vuint32_t	dcr;
	vuint32_t	dmar;
};

extern struct stm_tim234 stm_tim2, stm_tim3, stm_tim4;

#define stm_tim2	(*((struct stm_tim234 *) 0x40000000))
#define stm_tim3	(*((struct stm_tim234 *) 0x40000400))
#define stm_tim4	(*((struct stm_tim234 *) 0x40000800))

#define STM_TIM234_CR1_CKD	8
#define  STM_TIM234_CR1_CKD_1		0
#define  STM_TIM234_CR1_CKD_2		1
#define  STM_TIM234_CR1_CKD_4		2
#define  STM_TIM234_CR1_CKD_MASK	3UL
#define STM_TIM234_CR1_ARPE	7
#define STM_TIM234_CR1_CMS	5
#define  STM_TIM234_CR1_CMS_EDGE	0
#define  STM_TIM234_CR1_CMS_CENTER_1	1
#define  STM_TIM234_CR1_CMS_CENTER_2	2
#define  STM_TIM234_CR1_CMS_CENTER_3	3
#define  STM_TIM234_CR1_CMS_MASK	3UL
#define STM_TIM234_CR1_DIR	4
#define  STM_TIM234_CR1_DIR_UP		0
#define  STM_TIM234_CR1_DIR_DOWN	1
#define STM_TIM234_CR1_OPM	3
#define STM_TIM234_CR1_URS	2
#define STM_TIM234_CR1_UDIS	1
#define STM_TIM234_CR1_CEN	0

#define STM_TIM234_CR2_TI1S	7
#define STM_TIM234_CR2_MMS	4
#define  STM_TIM234_CR2_MMS_RESET		0
#define  STM_TIM234_CR2_MMS_ENABLE		1
#define  STM_TIM234_CR2_MMS_UPDATE		2
#define  STM_TIM234_CR2_MMS_COMPARE_PULSE	3
#define  STM_TIM234_CR2_MMS_COMPARE_OC1REF	4
#define  STM_TIM234_CR2_MMS_COMPARE_OC2REF	5
#define  STM_TIM234_CR2_MMS_COMPARE_OC3REF	6
#define  STM_TIM234_CR2_MMS_COMPARE_OC4REF	7
#define  STM_TIM234_CR2_MMS_MASK		7UL
#define STM_TIM234_CR2_CCDS	3

#define STM_TIM234_SMCR_ETP	15
#define STM_TIM234_SMCR_ECE	14
#define STM_TIM234_SMCR_ETPS	12
#define  STM_TIM234_SMCR_ETPS_OFF		0
#define  STM_TIM234_SMCR_ETPS_DIV_2	       	1
#define  STM_TIM234_SMCR_ETPS_DIV_4		2
#define  STM_TIM234_SMCR_ETPS_DIV_8		3
#define  STM_TIM234_SMCR_ETPS_MASK		3UL
#define STM_TIM234_SMCR_ETF	8
#define  STM_TIM234_SMCR_ETF_NONE		0
#define  STM_TIM234_SMCR_ETF_INT_N_2		1
#define  STM_TIM234_SMCR_ETF_INT_N_4		2
#define  STM_TIM234_SMCR_ETF_INT_N_8		3
#define  STM_TIM234_SMCR_ETF_DTS_2_N_6		4
#define  STM_TIM234_SMCR_ETF_DTS_2_N_8		5
#define  STM_TIM234_SMCR_ETF_DTS_4_N_6		6
#define  STM_TIM234_SMCR_ETF_DTS_4_N_8		7
#define  STM_TIM234_SMCR_ETF_DTS_8_N_6		8
#define  STM_TIM234_SMCR_ETF_DTS_8_N_8		9
#define  STM_TIM234_SMCR_ETF_DTS_16_N_5		10
#define  STM_TIM234_SMCR_ETF_DTS_16_N_6		11
#define  STM_TIM234_SMCR_ETF_DTS_16_N_8		12
#define  STM_TIM234_SMCR_ETF_DTS_32_N_5		13
#define  STM_TIM234_SMCR_ETF_DTS_32_N_6		14
#define  STM_TIM234_SMCR_ETF_DTS_32_N_8		15
#define  STM_TIM234_SMCR_ETF_MASK		15UL
#define STM_TIM234_SMCR_MSM	7
#define STM_TIM234_SMCR_TS	4
#define  STM_TIM234_SMCR_TS_ITR0		0
#define  STM_TIM234_SMCR_TS_ITR1		1
#define  STM_TIM234_SMCR_TS_ITR2		2
#define  STM_TIM234_SMCR_TS_ITR3		3
#define  STM_TIM234_SMCR_TS_TI1F_ED		4
#define  STM_TIM234_SMCR_TS_TI1FP1		5
#define  STM_TIM234_SMCR_TS_TI2FP2		6
#define  STM_TIM234_SMCR_TS_ETRF		7
#define  STM_TIM234_SMCR_TS_MASK		7UL
#define STM_TIM234_SMCR_SMS	0
#define  STM_TIM234_SMCR_SMS_DISABLE		0
#define  STM_TIM234_SMCR_SMS_ENCODER_MODE_1	1
#define  STM_TIM234_SMCR_SMS_ENCODER_MODE_2	2
#define  STM_TIM234_SMCR_SMS_ENCODER_MODE_3	3
#define  STM_TIM234_SMCR_SMS_RESET_MODE		4
#define  STM_TIM234_SMCR_SMS_GATED_MODE		5
#define  STM_TIM234_SMCR_SMS_TRIGGER_MODE	6
#define  STM_TIM234_SMCR_SMS_EXTERNAL_CLOCK	7
#define  STM_TIM234_SMCR_SMS_MASK		7UL

#define STM_TIM234_DIER_TDE		14
#define STM_TIM234_DIER_CC4DE		12
#define STM_TIM234_DIER_CC3DE		11
#define STM_TIM234_DIER_CC2DE		10
#define STM_TIM234_DIER_CC1DE		9
#define STM_TIM234_DIER_UDE		8

#define STM_TIM234_DIER_TIE		6
#define STM_TIM234_DIER_CC4IE		4
#define STM_TIM234_DIER_CC3IE		3
#define STM_TIM234_DIER_CC2IE		2
#define STM_TIM234_DIER_CC1IE		1
#define STM_TIM234_DIER_UIE		0

#define STM_TIM234_SR_CC4OF	12
#define STM_TIM234_SR_CC3OF	11
#define STM_TIM234_SR_CC2OF	10
#define STM_TIM234_SR_CC1OF	9
#define STM_TIM234_SR_TIF	6
#define STM_TIM234_SR_CC4IF	4
#define STM_TIM234_SR_CC3IF	3
#define STM_TIM234_SR_CC2IF	2
#define STM_TIM234_SR_CC1IF	1
#define STM_TIM234_SR_UIF	0

#define STM_TIM234_EGR_TG	6
#define STM_TIM234_EGR_CC4G	4
#define STM_TIM234_EGR_CC3G	3
#define STM_TIM234_EGR_CC2G	2
#define STM_TIM234_EGR_CC1G	1
#define STM_TIM234_EGR_UG	0

#define STM_TIM234_CCMR1_OC2CE	15
#define STM_TIM234_CCMR1_OC2M	12
#define  STM_TIM234_CCMR1_OC2M_FROZEN			0
#define  STM_TIM234_CCMR1_OC2M_SET_HIGH_ON_MATCH	1
#define  STM_TIM234_CCMR1_OC2M_SET_LOW_ON_MATCH		2
#define  STM_TIM234_CCMR1_OC2M_TOGGLE			3
#define  STM_TIM234_CCMR1_OC2M_FORCE_LOW		4
#define  STM_TIM234_CCMR1_OC2M_FORCE_HIGH		5
#define  STM_TIM234_CCMR1_OC2M_PWM_MODE_1		6
#define  STM_TIM234_CCMR1_OC2M_PWM_MODE_2		7
#define  STM_TIM234_CCMR1_OC2M_MASK			7UL
#define STM_TIM234_CCMR1_OC2PE	11
#define STM_TIM234_CCMR1_OC2FE	10
#define STM_TIM234_CCMR1_CC2S	8
#define  STM_TIM234_CCMR1_CC2S_OUTPUT			0
#define  STM_TIM234_CCMR1_CC2S_INPUT_TI2		1
#define  STM_TIM234_CCMR1_CC2S_INPUT_TI1		2
#define  STM_TIM234_CCMR1_CC2S_INPUT_TRC		3
#define  STM_TIM234_CCMR1_CC2S_MASK			3UL

#define STM_TIM234_CCMR1_OC1CE	7
#define STM_TIM234_CCMR1_OC1M	4
#define  STM_TIM234_CCMR1_OC1M_FROZEN			0
#define  STM_TIM234_CCMR1_OC1M_SET_HIGH_ON_MATCH	1
#define  STM_TIM234_CCMR1_OC1M_SET_LOW_ON_MATCH		2
#define  STM_TIM234_CCMR1_OC1M_TOGGLE			3
#define  STM_TIM234_CCMR1_OC1M_FORCE_LOW		4
#define  STM_TIM234_CCMR1_OC1M_FORCE_HIGH		5
#define  STM_TIM234_CCMR1_OC1M_PWM_MODE_1		6
#define  STM_TIM234_CCMR1_OC1M_PWM_MODE_2		7
#define  STM_TIM234_CCMR1_OC1M_MASK			7UL
#define STM_TIM234_CCMR1_OC1PE	3
#define STM_TIM234_CCMR1_OC1FE	2
#define STM_TIM234_CCMR1_CC1S	0
#define  STM_TIM234_CCMR1_CC1S_OUTPUT			0
#define  STM_TIM234_CCMR1_CC1S_INPUT_TI1		1
#define  STM_TIM234_CCMR1_CC1S_INPUT_TI2		2
#define  STM_TIM234_CCMR1_CC1S_INPUT_TRC		3
#define  STM_TIM234_CCMR1_CC1S_MASK			3UL

#define STM_TIM234_CCMR1_IC2F	12
#define  STM_TIM234_CCMR1_IC2F_NONE			0
#define  STM_TIM234_CCMR1_IC2F_CK_INT_N_2		1
#define  STM_TIM234_CCMR1_IC2F_CK_INT_N_4		2
#define  STM_TIM234_CCMR1_IC2F_CK_INT_N_8		3
#define  STM_TIM234_CCMR1_IC2F_DTS_2_N_6		4
#define  STM_TIM234_CCMR1_IC2F_DTS_2_N_8		5
#define  STM_TIM234_CCMR1_IC2F_DTS_4_N_6		6
#define  STM_TIM234_CCMR1_IC2F_DTS_4_N_8		7
#define  STM_TIM234_CCMR1_IC2F_DTS_8_N_6		8
#define  STM_TIM234_CCMR1_IC2F_DTS_8_N_8		9
#define  STM_TIM234_CCMR1_IC2F_DTS_16_N_5		10
#define  STM_TIM234_CCMR1_IC2F_DTS_16_N_6		11
#define  STM_TIM234_CCMR1_IC2F_DTS_16_N_8		12
#define  STM_TIM234_CCMR1_IC2F_DTS_32_N_5		13
#define  STM_TIM234_CCMR1_IC2F_DTS_32_N_6		14
#define  STM_TIM234_CCMR1_IC2F_DTS_32_N_8		15
#define STM_TIM234_CCMR1_IC2PSC	10
#define  STM_TIM234_CCMR1_IC2PSC_NONE			0
#define  STM_TIM234_CCMR1_IC2PSC_2			1
#define  STM_TIM234_CCMR1_IC2PSC_4			2
#define  STM_TIM234_CCMR1_IC2PSC_8			3
#define STM_TIM234_CCMR1_IC1F	4
#define  STM_TIM234_CCMR1_IC1F_NONE			0
#define  STM_TIM234_CCMR1_IC1F_CK_INT_N_2		1
#define  STM_TIM234_CCMR1_IC1F_CK_INT_N_4		2
#define  STM_TIM234_CCMR1_IC1F_CK_INT_N_8		3
#define  STM_TIM234_CCMR1_IC1F_DTS_2_N_6		4
#define  STM_TIM234_CCMR1_IC1F_DTS_2_N_8		5
#define  STM_TIM234_CCMR1_IC1F_DTS_4_N_6		6
#define  STM_TIM234_CCMR1_IC1F_DTS_4_N_8		7
#define  STM_TIM234_CCMR1_IC1F_DTS_8_N_6		8
#define  STM_TIM234_CCMR1_IC1F_DTS_8_N_8		9
#define  STM_TIM234_CCMR1_IC1F_DTS_16_N_5		10
#define  STM_TIM234_CCMR1_IC1F_DTS_16_N_6		11
#define  STM_TIM234_CCMR1_IC1F_DTS_16_N_8		12
#define  STM_TIM234_CCMR1_IC1F_DTS_32_N_5		13
#define  STM_TIM234_CCMR1_IC1F_DTS_32_N_6		14
#define  STM_TIM234_CCMR1_IC1F_DTS_32_N_8		15
#define STM_TIM234_CCMR1_IC1PSC	2
#define  STM_TIM234_CCMR1_IC1PSC_NONE			0
#define  STM_TIM234_CCMR1_IC1PSC_2			1
#define  STM_TIM234_CCMR1_IC1PSC_4			2
#define  STM_TIM234_CCMR1_IC1PSC_8			3

#define STM_TIM234_CCMR2_OC4CE	15
#define STM_TIM234_CCMR2_OC4M	12
#define  STM_TIM234_CCMR2_OC4M_FROZEN			0
#define  STM_TIM234_CCMR2_OC4M_SET_HIGH_ON_MATCH	1
#define  STM_TIM234_CCMR2_OC4M_SET_LOW_ON_MATCH		2
#define  STM_TIM234_CCMR2_OC4M_TOGGLE			3
#define  STM_TIM234_CCMR2_OC4M_FORCE_LOW		4
#define  STM_TIM234_CCMR2_OC4M_FORCE_HIGH		5
#define  STM_TIM234_CCMR2_OC4M_PWM_MODE_1		6
#define  STM_TIM234_CCMR2_OC4M_PWM_MODE_2		7
#define  STM_TIM234_CCMR2_OC4M_MASK			7UL
#define STM_TIM234_CCMR2_OC4PE	11
#define STM_TIM234_CCMR2_OC4FE	10
#define STM_TIM234_CCMR2_CC4S	8
#define  STM_TIM234_CCMR2_CC4S_OUTPUT			0
#define  STM_TIM234_CCMR2_CC4S_INPUT_TI4		1
#define  STM_TIM234_CCMR2_CC4S_INPUT_TI3		2
#define  STM_TIM234_CCMR2_CC4S_INPUT_TRC		3
#define  STM_TIM234_CCMR2_CC4S_MASK			3UL

#define STM_TIM234_CCMR2_OC3CE	7
#define STM_TIM234_CCMR2_OC3M	4
#define  STM_TIM234_CCMR2_OC3M_FROZEN			0
#define  STM_TIM234_CCMR2_OC3M_SET_HIGH_ON_MATCH	1
#define  STM_TIM234_CCMR2_OC3M_SET_LOW_ON_MATCH		2
#define  STM_TIM234_CCMR2_OC3M_TOGGLE			3
#define  STM_TIM234_CCMR2_OC3M_FORCE_LOW		4
#define  STM_TIM234_CCMR2_OC3M_FORCE_HIGH		5
#define  STM_TIM234_CCMR2_OC3M_PWM_MODE_1		6
#define  STM_TIM234_CCMR2_OC3M_PWM_MODE_2		7
#define  STM_TIM234_CCMR2_OC3M_MASK			7UL
#define STM_TIM234_CCMR2_OC3PE	3
#define STM_TIM234_CCMR2_OC3FE	2
#define STM_TIM234_CCMR2_CC3S	0
#define  STM_TIM234_CCMR2_CC3S_OUTPUT			0
#define  STM_TIM234_CCMR2_CC3S_INPUT_TI3		1
#define  STM_TIM234_CCMR2_CC3S_INPUT_TI4		2
#define  STM_TIM234_CCMR2_CC3S_INPUT_TRC		3
#define  STM_TIM234_CCMR2_CC3S_MASK			3UL

#define STM_TIM234_CCER_CC4NP	15
#define STM_TIM234_CCER_CC4P	13
#define  STM_TIM234_CCER_CC4P_ACTIVE_HIGH	0
#define  STM_TIM234_CCER_CC4P_ACTIVE_LOW	1
#define STM_TIM234_CCER_CC4E	12
#define STM_TIM234_CCER_CC3NP	11
#define STM_TIM234_CCER_CC3P	9
#define  STM_TIM234_CCER_CC3P_ACTIVE_HIGH	0
#define  STM_TIM234_CCER_CC3P_ACTIVE_LOW	1
#define STM_TIM234_CCER_CC3E	8
#define STM_TIM234_CCER_CC2NP	7
#define STM_TIM234_CCER_CC2P	5
#define  STM_TIM234_CCER_CC2P_ACTIVE_HIGH	0
#define  STM_TIM234_CCER_CC2P_ACTIVE_LOW	1
#define STM_TIM234_CCER_CC2E	4
#define STM_TIM234_CCER_CC1NP	3
#define STM_TIM234_CCER_CC1P	1
#define  STM_TIM234_CCER_CC1P_ACTIVE_HIGH	0
#define  STM_TIM234_CCER_CC1P_ACTIVE_LOW	1
#define STM_TIM234_CCER_CC1E	0

struct stm_tim67 {
	vuint32_t	cr1;
	vuint32_t	cr2;
	uint32_t	_unused_08;
	vuint32_t	dier;

	vuint32_t	sr;
	vuint32_t	egr;
	uint32_t	_unused_18;
	uint32_t	_unused_1c;

	uint32_t	_unused_20;
	vuint32_t	cnt;
	vuint32_t	psc;
	vuint32_t	arr;
};

extern struct stm_tim67 stm_tim6;

#define STM_TIM67_CR1_ARPE	(7)
#define STM_TIM67_CR1_OPM	(3)
#define STM_TIM67_CR1_URS	(2)
#define STM_TIM67_CR1_UDIS	(1)
#define STM_TIM67_CR1_CEN	(0)

#define STM_TIM67_CR2_MMS	(4)
#define  STM_TIM67_CR2_MMS_RESET	0
#define  STM_TIM67_CR2_MMS_ENABLE	1
#define  STM_TIM67_CR2_MMS_UPDATE	2
#define  STM_TIM67_CR2_MMS_MASK		7UL

#define STM_TIM67_DIER_UDE	(8)
#define STM_TIM67_DIER_UIE	(0)

#define STM_TIM67_SR_UIF	(0)

#define STM_TIM67_EGR_UG	(0)

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
#define STM_ISR_DMA1_CHANNEL1_POS	11
#define STM_ISR_DMA1_CHANNEL2_POS	12
#define STM_ISR_DMA1_CHANNEL3_POS	13
#define STM_ISR_DMA1_CHANNEL4_POS	14
#define STM_ISR_DMA1_CHANNEL5_POS	15
#define STM_ISR_DMA1_CHANNEL6_POS	16
#define STM_ISR_DMA1_CHANNEL7_POS	17
#define STM_ISR_ADC1_2_POS		18
#define STM_ISR_USB_HP_POS		19
#define STM_ISR_USB_LP_POS		20
#define STM_ISR_CAN_RX1_POS		21
#define STM_ISR_CAN_SCE_POS		22
#define STM_ISR_EXTI9_5_POS		23
#define STM_ISR_TIM1_BRK_POS		24
#define STM_ISR_TIM1_UP_POS		25
#define STM_ISR_TIM1_TRG_COM_POS	26
#define STM_ISR_TIM1_CC_POS		27
#define STM_ISR_TIM2_POS		28
#define STM_ISR_TIM3_POS		29
#define STM_ISR_TIM4_POS		30
#define STM_ISR_I2C1_EV_POS		31
#define STM_ISR_I2C1_ER_POS		32
#define STM_ISR_I2C2_EV_POS		33
#define STM_ISR_I2C2_ER_POS		34
#define STM_ISR_SPI1_POS		35
#define STM_ISR_SPI2_POS		36
#define STM_ISR_USART1_POS		37
#define STM_ISR_USART2_POS		38
#define STM_ISR_USART3_POS		39
#define STM_ISR_EXTI15_10_POS		40
#define STM_ISR_RTC_ALARM_POS		41
#define STM_ISR_USB_WAKEUP_POS		42
#define STM_ISR_TIM8_BRK_POS		43
#define STM_ISR_TIM8_UP_POS		44
#define STM_ISR_TIM8_TRG_COM_POS	45
#define STM_ISR_TIM8_CC_POS		46
#define STM_ISR_ADC3_POS		47
#define STM_ISR_FSMC_POS		48
#define STM_ISR_SDIO_POS		49
#define STM_ISR_TIM5_POS		50
#define STM_ISR_SPI3_POS		51
#define STM_ISR_UART4_POS		52
#define STM_ISR_UART5_POS		53
#define STM_ISR_TIM6_POS		54
#define STM_ISR_TIM7_POS		55
#define STM_ISR_DMA2_CHANNEL1_POS	56
#define STM_ISR_DMA2_CHANNEL2_POS	57
#define STM_ISR_DMA2_CHANNEL3_POS	58
#define STM_ISR_DMA3_CHANNEL4_5_POS	59

#endif
