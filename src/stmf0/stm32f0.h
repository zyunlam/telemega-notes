/*
 * Copyright © 2015 Keith Packard <keithp@keithp.com>
 *
 * This program is free software; you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation; version 2 of the License.
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

#ifndef _STM32F0_H_
#define _STM32F0_H_

#include <stdint.h>

typedef volatile uint32_t	vuint32_t;
typedef volatile void *		vvoid_t;
typedef volatile uint16_t	vuint16_t;

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
	vuint32_t	brr;
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
#define STM_OSPEEDR_LOW			0	/* 2MHz */
#define STM_OSPEEDR_MEDIUM		1	/* 10MHz */
#define STM_OSPEEDR_HIGH		3	/* 10-50MHz */

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
extern struct stm_gpio stm_gpiof;

#define stm_gpiof  (*((struct stm_gpio *) 0x48001400))
#define stm_gpioc  (*((struct stm_gpio *) 0x48000800))
#define stm_gpiob  (*((struct stm_gpio *) 0x48000400))
#define stm_gpioa  (*((struct stm_gpio *) 0x48000000))

struct stm_usart {
	vuint32_t	cr1;	/* control register 1 */
	vuint32_t	cr2;	/* control register 2 */
	vuint32_t	cr3;	/* control register 3 */
	vuint32_t	brr;	/* baud rate register */

	vuint32_t	gtpr;	/* guard time and prescaler */
	vuint32_t	rtor;	/* */
	vuint32_t	rqr;	/* */
	vuint32_t	isr;	/* */

	vuint32_t	icr;	/* */
	vuint32_t	rdr;	/* */
	vuint32_t	tdr;	/* */
};

extern struct stm_usart	stm_usart1;
extern struct stm_usart stm_usart2;

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

#define STM_USART_CR3_ONEBITE	(11)	/* One sample bit method enable */
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

struct stm_tim {
};

extern struct stm_tim stm_tim9;

struct stm_tim1011 {
	vuint32_t	cr1;
	uint32_t	unused_4;
	vuint32_t	smcr;
	vuint32_t	dier;
	vuint32_t	sr;
	vuint32_t	egr;
	vuint32_t	ccmr1;
	uint32_t	unused_1c;
	vuint32_t	ccer;
	vuint32_t	cnt;
	vuint32_t	psc;
	vuint32_t	arr;
	uint32_t	unused_30;
	vuint32_t	ccr1;
	uint32_t	unused_38;
	uint32_t	unused_3c;
	uint32_t	unused_40;
	uint32_t	unused_44;
	uint32_t	unused_48;
	uint32_t	unused_4c;
	vuint32_t	or;
};

extern struct stm_tim1011 stm_tim10;
extern struct stm_tim1011 stm_tim11;

#define STM_TIM1011_CR1_CKD	8
#define  STM_TIM1011_CR1_CKD_1		0
#define  STM_TIM1011_CR1_CKD_2		1
#define  STM_TIM1011_CR1_CKD_4		2
#define  STM_TIM1011_CR1_CKD_MASK	3
#define STM_TIM1011_CR1_ARPE	7
#define STM_TIM1011_CR1_URS	2
#define STM_TIM1011_CR1_UDIS	1
#define STM_TIM1011_CR1_CEN	0

#define STM_TIM1011_SMCR_ETP	15
#define STM_TIM1011_SMCR_ECE	14
#define STM_TIM1011_SMCR_ETPS	12
#define  STM_TIM1011_SMCR_ETPS_OFF	0
#define  STM_TIM1011_SMCR_ETPS_2	1
#define  STM_TIM1011_SMCR_ETPS_4	2
#define  STM_TIM1011_SMCR_ETPS_8	3
#define  STM_TIM1011_SMCR_ETPS_MASK	3
#define STM_TIM1011_SMCR_ETF	8
#define  STM_TIM1011_SMCR_ETF_NONE		0
#define  STM_TIM1011_SMCR_ETF_CK_INT_2		1
#define  STM_TIM1011_SMCR_ETF_CK_INT_4		2
#define  STM_TIM1011_SMCR_ETF_CK_INT_8		3
#define  STM_TIM1011_SMCR_ETF_DTS_2_6		4
#define  STM_TIM1011_SMCR_ETF_DTS_2_8		5
#define  STM_TIM1011_SMCR_ETF_DTS_4_6		6
#define  STM_TIM1011_SMCR_ETF_DTS_4_8		7
#define  STM_TIM1011_SMCR_ETF_DTS_8_6		8
#define  STM_TIM1011_SMCR_ETF_DTS_8_8		9
#define  STM_TIM1011_SMCR_ETF_DTS_16_5		10
#define  STM_TIM1011_SMCR_ETF_DTS_16_6		11
#define  STM_TIM1011_SMCR_ETF_DTS_16_8		12
#define  STM_TIM1011_SMCR_ETF_DTS_32_5		13
#define  STM_TIM1011_SMCR_ETF_DTS_32_6		14
#define  STM_TIM1011_SMCR_ETF_DTS_32_8		15
#define  STM_TIM1011_SMCR_ETF_MASK		15

#define STM_TIM1011_DIER_CC1E	1
#define STM_TIM1011_DIER_UIE	0

#define STM_TIM1011_SR_CC1OF	9
#define STM_TIM1011_SR_CC1IF	1
#define STM_TIM1011_SR_UIF	0

#define STM_TIM1011_EGR_CC1G	1
#define STM_TIM1011_EGR_UG	0

#define STM_TIM1011_CCMR1_OC1CE	7
#define STM_TIM1011_CCMR1_OC1M	4
#define  STM_TIM1011_CCMR1_OC1M_FROZEN			0
#define  STM_TIM1011_CCMR1_OC1M_SET_1_ACTIVE_ON_MATCH	1
#define  STM_TIM1011_CCMR1_OC1M_SET_1_INACTIVE_ON_MATCH	2
#define  STM_TIM1011_CCMR1_OC1M_TOGGLE			3
#define  STM_TIM1011_CCMR1_OC1M_FORCE_INACTIVE		4
#define  STM_TIM1011_CCMR1_OC1M_FORCE_ACTIVE		5
#define  STM_TIM1011_CCMR1_OC1M_PWM_MODE_1		6
#define  STM_TIM1011_CCMR1_OC1M_PWM_MODE_2		7
#define  STM_TIM1011_CCMR1_OC1M_MASK			7
#define STM_TIM1011_CCMR1_OC1PE	3
#define STM_TIM1011_CCMR1_OC1FE	2
#define STM_TIM1011_CCMR1_CC1S	0
#define  STM_TIM1011_CCMR1_CC1S_OUTPUT			0
#define  STM_TIM1011_CCMR1_CC1S_INPUT_TI1		1
#define  STM_TIM1011_CCMR1_CC1S_INPUT_TI2		2
#define  STM_TIM1011_CCMR1_CC1S_INPUT_TRC		3
#define  STM_TIM1011_CCMR1_CC1S_MASK			3

#define  STM_TIM1011_CCMR1_IC1F_NONE		0
#define  STM_TIM1011_CCMR1_IC1F_CK_INT_2	1
#define  STM_TIM1011_CCMR1_IC1F_CK_INT_4	2
#define  STM_TIM1011_CCMR1_IC1F_CK_INT_8	3
#define  STM_TIM1011_CCMR1_IC1F_DTS_2_6		4
#define  STM_TIM1011_CCMR1_IC1F_DTS_2_8		5
#define  STM_TIM1011_CCMR1_IC1F_DTS_4_6		6
#define  STM_TIM1011_CCMR1_IC1F_DTS_4_8		7
#define  STM_TIM1011_CCMR1_IC1F_DTS_8_6		8
#define  STM_TIM1011_CCMR1_IC1F_DTS_8_8		9
#define  STM_TIM1011_CCMR1_IC1F_DTS_16_5	10
#define  STM_TIM1011_CCMR1_IC1F_DTS_16_6	11
#define  STM_TIM1011_CCMR1_IC1F_DTS_16_8	12
#define  STM_TIM1011_CCMR1_IC1F_DTS_32_5	13
#define  STM_TIM1011_CCMR1_IC1F_DTS_32_6	14
#define  STM_TIM1011_CCMR1_IC1F_DTS_32_8	15
#define  STM_TIM1011_CCMR1_IC1F_MASK		15
#define STM_TIM1011_CCMR1_IC1PSC	2
#define  STM_TIM1011_CCMR1_IC1PSC_1		0
#define  STM_TIM1011_CCMR1_IC1PSC_2		1
#define  STM_TIM1011_CCMR1_IC1PSC_4		2
#define  STM_TIM1011_CCMR1_IC1PSC_8		3
#define  STM_TIM1011_CCMR1_IC1PSC_MASK		3
#define STM_TIM1011_CCMR1_CC1S		0

#define STM_TIM1011_CCER_CC1NP		3
#define STM_TIM1011_CCER_CC1P		1
#define STM_TIM1011_CCER_CC1E		0

#define STM_TIM1011_OR_TI1_RMP_RI	3
#define STM_TIM1011_ETR_RMP		2
#define STM_TIM1011_TI1_RMP		0
#define  STM_TIM1011_TI1_RMP_GPIO		0
#define  STM_TIM1011_TI1_RMP_LSI		1
#define  STM_TIM1011_TI1_RMP_LSE		2
#define  STM_TIM1011_TI1_RMP_RTC		3
#define  STM_TIM1011_TI1_RMP_MASK		3

/* Flash interface */

struct stm_flash {
	vuint32_t	acr;
	vuint32_t	keyr;
	vuint32_t	optkeyr;
	vuint32_t	sr;

	vuint32_t	cr;
	vuint32_t	ar;
	vuint32_t	unused_0x18;
	vuint32_t	obr;

	vuint32_t	wrpr;
};

extern struct stm_flash	stm_flash;

#define STM_FLASH_ACR_PRFTBS	(5)
#define STM_FLASH_ACR_PRFTBE	(4)
#define STM_FLASH_ACR_LATENCY	(0)
#define  STM_FLASH_ACR_LATENCY_0		0
#define  STM_FLASH_ACR_LATENCY_1		1

#define STM_FLASH_PECR_OBL_LAUNCH	18
#define STM_FLASH_PECR_ERRIE		17
#define STM_FLASH_PECR_EOPIE		16
#define STM_FLASH_PECR_FPRG		10
#define STM_FLASH_PECR_ERASE		9
#define STM_FLASH_PECR_FTDW		8
#define STM_FLASH_PECR_DATA		4
#define STM_FLASH_PECR_PROG		3
#define STM_FLASH_PECR_OPTLOCK		2
#define STM_FLASH_PECR_PRGLOCK		1
#define STM_FLASH_PECR_PELOCK		0

#define STM_FLASH_SR_EOP		5
#define STM_FLASH_SR_WRPRTERR		4
#define STM_FLASH_SR_PGERR		2
#define STM_FLASH_SR_BSY		0

#define STM_FLASH_CR_OBL_LAUNCH		13
#define STM_FLASH_CR_EOPIE		12
#define STM_FLASH_CR_ERRIE		10
#define STM_FLASH_CR_OPTWRE		9
#define STM_FLASH_CR_LOCK		7
#define STM_FLASH_CR_STRT		6
#define STM_FLASH_CR_OPTER		5
#define STM_FLASH_CR_OPTPG		4
#define STM_FLASH_CR_MER		2
#define STM_FLASH_CR_PER		1
#define STM_FLASH_CR_PG			0

#define STM_FLASH_OBR_DATA1		24
#define STM_FLASH_OBR_DATA0		16
#define STM_FLASH_OBR_BOOT_SEL		15
#define STM_FLASH_OBR_RAM_PARITY_CHECK	14
#define STM_FLASH_OBR_VDDA_MONITOR	13
#define STM_FLASH_OBR_NBOOT1		12
#define STM_FLASH_OBR_NBOOT0		11
#define STM_FLASH_OBR_NRST_STDBY	10
#define STM_FLASH_OBR_NRST_STOP		9
#define STM_FLASH_OBR_WDG_SW		8
#define STM_FLASH_OBR_RDPRT		1
#define  STM_FLASH_OBR_RDPRT_LEVEL0		0
#define  STM_FLASH_OBR_RDPRT_LEVEL1		1
#define  STM_FLASH_OBR_RDPRT_LEVEL2		3
#define STM_FLASH_OBR_OPTERR		0

#define STM_FLASH_KEYR_KEY1	0x45670123
#define STM_FLASH_KEYR_KEY2 	0xcdef89ab

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
	vuint32_t	ahbrstr;
	vuint32_t	cfgr2;

	vuint32_t	cfgr3;
	vuint32_t	cr2;
};

extern struct stm_rcc stm_rcc;

/* Nominal high speed internal oscillator frequency is 16MHz */
#define STM_HSI_FREQ		16000000

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

#define STM_RCC_CFGR_PLL_NODIV	(31)
#define  STM_RCC_CFGR_PLL_NODIV_DIV_1	1
#define  STM_RCC_CFGR_PLL_NODIV_DIV_2	0

#define STM_RCC_CFGR_MCOPRE	(28)
#define  STM_RCC_CFGR_MCOPRE_DIV_1	0
#define  STM_RCC_CFGR_MCOPRE_DIV_2	1
#define  STM_RCC_CFGR_MCOPRE_DIV_4	2
#define  STM_RCC_CFGR_MCOPRE_DIV_8	3
#define  STM_RCC_CFGR_MCOPRE_DIV_16	4
#define  STM_RCC_CFGR_MCOPRE_DIV_32	5
#define  STM_RCC_CFGR_MCOPRE_DIV_64	6
#define  STM_RCC_CFGR_MCOPRE_DIV_128	7
#define  STM_RCC_CFGR_MCOPRE_DIV_MASK	7

#define STM_RCC_CFGR_MCO	(24)
# define STM_RCC_CFGR_MCO_DISABLE	0

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
#define  STM_RCC_CFGR_PLLMUL_14	       	12
#define  STM_RCC_CFGR_PLLMUL_15	       	13
#define  STM_RCC_CFGR_PLLMUL_16	       	14
#define  STM_RCC_CFGR_PLLMUL_MASK	0xf

#define STM_RCC_CFGR_PLLXTPRE	(17)

#define STM_RCC_CFGR_PLLSRC	(15)
# define STM_RCC_CFGR_PLLSRC_HSI_DIV_2	0
# define STM_RCC_CFGR_PLLSRC_HSI	1
# define STM_RCC_CFGR_PLLSRC_HSE	2
# define STM_RCC_CFGR_PLLSRC_HSI48	3

#define STM_RCC_CFGR_ADCPRE	(14)

#define STM_RCC_CFGR_PPRE	(8)
#define  STM_RCC_CFGR_PPRE_DIV_1	0
#define  STM_RCC_CFGR_PPRE_DIV_2	4
#define  STM_RCC_CFGR_PPRE_DIV_4	5
#define  STM_RCC_CFGR_PPRE_DIV_8	6
#define  STM_RCC_CFGR_PPRE_DIV_16	7
#define  STM_RCC_CFGR_PPRE_MASK		7

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
#define  STM_RCC_CFGR_HPRE_MASK		0xf

#define STM_RCC_CFGR_SWS	(2)
#define  STM_RCC_CFGR_SWS_HSI		0
#define  STM_RCC_CFGR_SWS_HSE		1
#define  STM_RCC_CFGR_SWS_PLL		2
#define  STM_RCC_CFGR_SWS_HSI48		3
#define  STM_RCC_CFGR_SWS_MASK		3

#define STM_RCC_CFGR_SW		(0)
#define  STM_RCC_CFGR_SW_HSI		0
#define  STM_RCC_CFGR_SW_HSE		1
#define  STM_RCC_CFGR_SW_PLL		2
#define  STM_RCC_CFGR_SW_HSI48		3
#define  STM_RCC_CFGR_SW_MASK		3

#define STM_RCC_APB2RSTR_DBGMCURST	22
#define STM_RCC_APB2RSTR_TIM17RST	18
#define STM_RCC_APB2RSTR_TIM16RST	17
#define STM_RCC_APB2RSTR_TIM15RST	16
#define STM_RCC_APB2RSTR_USART1RST	14
#define STM_RCC_APB2RSTR_SPI1RST	12
#define STM_RCC_APB2RSTR_TIM1RST	11
#define STM_RCC_APB2RSTR_ADCRST		9
#define STM_RCC_APB2RSTR_USART8RST	7
#define STM_RCC_APB2RSTR_USART7RST	6
#define STM_RCC_APB2RSTR_USART6RST	5
#define STM_RCC_APB2RSTR_SYSCFGRST	1

#define STM_RCC_APB1RSTR_CECRST		30
#define STM_RCC_APB1RSTR_DACRST		29
#define STM_RCC_APB1RSTR_PWRRST		28
#define STM_RCC_APB1RSTR_CRSRST		27
#define STM_RCC_APB1RSTR_CANRST		25
#define STM_RCC_APB1RSTR_USBRST		23
#define STM_RCC_APB1RSTR_I2C2RST	22
#define STM_RCC_APB1RSTR_I1C1RST	21
#define STM_RCC_APB1RSTR_USART5RST	20
#define STM_RCC_APB1RSTR_USART4RST	19
#define STM_RCC_APB1RSTR_USART3RST	18
#define STM_RCC_APB1RSTR_USART2RST	17
#define STM_RCC_APB1RSTR_SPI2RST	14
#define STM_RCC_APB1RSTR_WWDGRST	11
#define STM_RCC_APB1RSTR_TIM14RST	8
#define STM_RCC_APB1RSTR_TIM7RST	5
#define STM_RCC_APB1RSTR_TIM6RST	4
#define STM_RCC_APB1RSTR_TIM3RST	1
#define STM_RCC_APB1RSTR_TIM2RST	0

#define STM_RCC_AHBENR_TSCEN	24
#define STM_RCC_AHBENR_IOPFEN	22
#define STM_RCC_AHBENR_IOPEEN	21
#define STM_RCC_AHBENR_IOPDEN	20
#define STM_RCC_AHBENR_IOPCEN	19
#define STM_RCC_AHBENR_IOPBEN	18
#define STM_RCC_AHBENR_IOPAEN	17
#define STM_RCC_AHBENR_CRCEN	6
#define STM_RCC_AHBENR_FLITFEN	4
#define STM_RCC_AHBENR_SRAMEN	2
#define STM_RCC_AHBENR_DMA2EN	1
#define STM_RCC_AHBENR_DMAEN	0

#define STM_RCC_APB2ENR_DBGMCUEN	22
#define STM_RCC_APB2ENR_TIM17EN		18
#define STM_RCC_APB2ENR_TIM16EN		17
#define STM_RCC_APB2ENR_TIM15EN		16
#define STM_RCC_APB2ENR_USART1EN	14
#define STM_RCC_APB2ENR_SPI1EN		12
#define STM_RCC_APB2ENR_TIM1EN		11
#define STM_RCC_APB2ENR_ADCEN		9
#define STM_RCC_APB2ENR_USART8EN	7
#define STM_RCC_APB2ENR_USART7EN	6
#define STM_RCC_APB2ENR_USART6EN	5
#define STM_RCC_APB2ENR_SYSCFGCOMPEN	0

#define STM_RCC_APB1ENR_CECEN		30
#define STM_RCC_APB1ENR_DACEN		29
#define STM_RCC_APB1ENR_PWREN		28
#define STM_RCC_APB1ENR_CRSEN		27
#define STM_RCC_APB1ENR_CANEN		25
#define STM_RCC_APB1ENR_USBEN		23
#define STM_RCC_APB1ENR_I2C2EN		22
#define STM_RCC_APB1ENR_IC21EN		21
#define STM_RCC_APB1ENR_USART5EN	20
#define STM_RCC_APB1ENR_USART4EN	19
#define STM_RCC_APB1ENR_USART3EN	18
#define STM_RCC_APB1ENR_USART2EN	17
#define STM_RCC_APB1ENR_SPI2EN		14
#define STM_RCC_APB1ENR_WWDGEN		11
#define STM_RCC_APB1ENR_TIM14EN		8
#define STM_RCC_APB1ENR_TIM7EN		5
#define STM_RCC_APB1ENR_TIM6EN		4
#define STM_RCC_APB1ENR_TIM3EN		1
#define STM_RCC_APB1ENR_TIM2EN		0

#define STM_RCC_CSR_LPWRRSTF		(31)
#define STM_RCC_CSR_WWDGRSTF		(30)
#define STM_RCC_CSR_IWDGRSTF		(29)
#define STM_RCC_CSR_SFTRSTF		(28)
#define STM_RCC_CSR_PORRSTF		(27)
#define STM_RCC_CSR_PINRSTF		(26)
#define STM_RCC_CSR_OBLRSTF		(25)
#define STM_RCC_CSR_RMVF		(24)
#define STM_RCC_CSR_V18PWRRSTF		(23)
#define STM_RCC_CSR_LSIRDY		(1)
#define STM_RCC_CSR_LSION		(0)

#define STM_RCC_CR2_HSI48CAL		24
#define STM_RCC_CR2_HSI48RDY		17
#define STM_RCC_CR2_HSI48ON		16
#define STM_RCC_CR2_HSI14CAL		8
#define STM_RCC_CR2_HSI14TRIM		3
#define STM_RCC_CR2_HSI14DIS		2
#define STM_RCC_CR2_HSI14RDY		1
#define STM_RCC_CR2_HSI14ON		0

#define STM_RCC_CFGR3_USART3SW		18
#define STM_RCC_CFGR3_USART2SW		16
#define STM_RCC_CFGR3_ADCSW		8
#define STM_RCC_CFGR3_USBSW		7
#define STM_RCC_CFGR3_CECSW		6
#define STM_RCC_CFGR3_I2C1SW		4
#define STM_RCC_CFGR3_USART1SW		0

struct stm_crs {
	vuint32_t	cr;
	vuint32_t	cfgr;
	vuint32_t	isr;
	vuint32_t	icr;
};

extern struct stm_crs stm_crs;

#define STM_CRS_CR_TRIM		8
#define STM_CRS_CR_SWSYNC	7
#define STM_CRS_CR_AUTOTRIMEN	6
#define STM_CRS_CR_CEN		5
#define STM_CRS_CR_ESYNCIE	3
#define STM_CRS_CR_ERRIE	2
#define STM_CRS_CR_SYNCWARNIE	1
#define STM_CRS_CR_SYNCOKIE	0

#define STM_CRS_CFGR_SYNCPOL	31
#define STM_CRS_CFGR_SYNCSRC	28
#define  STM_CRS_CFGR_SYNCSRC_GPIO	0
#define  STM_CRS_CFGR_SYNCSRC_LSE	1
#define  STM_CRS_CFGR_SYNCSRC_USB	2
#define STM_CRS_CFGR_SYNCDIV	24
#define  STM_CRS_CFGR_SYNCDIV_1		0
#define  STM_CRS_CFGR_SYNCDIV_2		1
#define  STM_CRS_CFGR_SYNCDIV_4		2
#define  STM_CRS_CFGR_SYNCDIV_8		3
#define  STM_CRS_CFGR_SYNCDIV_16	4
#define  STM_CRS_CFGR_SYNCDIV_32	5
#define  STM_CRS_CFGR_SYNCDIV_64	6
#define  STM_CRS_CFGR_SYNCDIV_128	7
#define STM_CRS_CFGR_FELIM	16
#define STM_CRS_CFGR_RELOAD	0

#define STM_CRS_ISR_FECAP	16
#define STM_CRS_ISR_FEDIR	15
#define STM_CRS_ISR_TRIMOVF	10
#define STM_CRS_ISR_SYNCMISS	9
#define STM_CRS_ISR_SYNCERR	8
#define STM_CRS_ISR_ESYNCF	3
#define STM_CRS_ISR_ERRF	2
#define STM_CRS_ISR_SYNCWARNF	1
#define STM_CRS_ISR_SYNCOKF	0

#define STM_CRS_ICR_ESYNCC	3
#define STM_CRS_ICR_ERRC	2
#define STM_CRS_ICR_SYNCWARNC	1
#define STM_CRS_ICR_SYNCOKC	0

struct stm_pwr {
	vuint32_t	cr;
	vuint32_t	csr;
};

extern struct stm_pwr stm_pwr;

#define STM_PWR_CR_DBP		(8)

#define STM_PWR_CR_PLS		(5)
#define  STM_PWR_CR_PLS_2_0	0
#define  STM_PWR_CR_PLS_2_1	1
#define  STM_PWR_CR_PLS_2_2	2
#define  STM_PWR_CR_PLS_2_3	3
#define  STM_PWR_CR_PLS_2_4	4
#define  STM_PWR_CR_PLS_2_5	5
#define  STM_PWR_CR_PLS_2_6	6
#define  STM_PWR_CR_PLS_EXT	7
#define  STM_PWR_CR_PLS_MASK	7

#define STM_PWR_CR_PVDE		(4)
#define STM_PWR_CR_CSBF		(3)
#define STM_PWR_CR_CWUF		(2)
#define STM_PWR_CR_PDDS		(1)
#define STM_PWR_CR_LPSDSR	(0)

#define STM_PWR_CSR_EWUP3	(10)
#define STM_PWR_CSR_EWUP2	(9)
#define STM_PWR_CSR_EWUP1	(8)
#define STM_PWR_CSR_REGLPF	(5)
#define STM_PWR_CSR_VOSF	(4)
#define STM_PWR_CSR_VREFINTRDYF	(3)
#define STM_PWR_CSR_PVDO	(2)
#define STM_PWR_CSR_SBF		(1)
#define STM_PWR_CSR_WUF		(0)

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
#define  STM_TIM67_CR2_MMS_MASK		7

#define STM_TIM67_DIER_UDE	(8)
#define STM_TIM67_DIER_UIE	(0)

#define STM_TIM67_SR_UIF	(0)

#define STM_TIM67_EGR_UG	(0)

struct stm_lcd {
	vuint32_t	cr;
	vuint32_t	fcr;
	vuint32_t	sr;
	vuint32_t	clr;
	uint32_t	unused_0x10;
	vuint32_t	ram[8*2];
};

extern struct stm_lcd stm_lcd;

#define STM_LCD_CR_MUX_SEG		(7)

#define STM_LCD_CR_BIAS			(5)
#define  STM_LCD_CR_BIAS_1_4		0
#define  STM_LCD_CR_BIAS_1_2		1
#define  STM_LCD_CR_BIAS_1_3		2
#define  STM_LCD_CR_BIAS_MASK		3

#define STM_LCD_CR_DUTY			(2)
#define  STM_LCD_CR_DUTY_STATIC		0
#define  STM_LCD_CR_DUTY_1_2		1
#define  STM_LCD_CR_DUTY_1_3		2
#define  STM_LCD_CR_DUTY_1_4		3
#define  STM_LCD_CR_DUTY_1_8		4
#define  STM_LCD_CR_DUTY_MASK		7

#define STM_LCD_CR_VSEL			(1)
#define STM_LCD_CR_LCDEN		(0)

#define STM_LCD_FCR_PS			(22)
#define  STM_LCD_FCR_PS_1		0x0
#define  STM_LCD_FCR_PS_2		0x1
#define  STM_LCD_FCR_PS_4		0x2
#define  STM_LCD_FCR_PS_8		0x3
#define  STM_LCD_FCR_PS_16		0x4
#define  STM_LCD_FCR_PS_32		0x5
#define  STM_LCD_FCR_PS_64		0x6
#define  STM_LCD_FCR_PS_128		0x7
#define  STM_LCD_FCR_PS_256		0x8
#define  STM_LCD_FCR_PS_512		0x9
#define  STM_LCD_FCR_PS_1024		0xa
#define  STM_LCD_FCR_PS_2048		0xb
#define  STM_LCD_FCR_PS_4096		0xc
#define  STM_LCD_FCR_PS_8192		0xd
#define  STM_LCD_FCR_PS_16384		0xe
#define  STM_LCD_FCR_PS_32768		0xf
#define  STM_LCD_FCR_PS_MASK		0xf

#define STM_LCD_FCR_DIV			(18)
#define STM_LCD_FCR_DIV_16		0x0
#define STM_LCD_FCR_DIV_17		0x1
#define STM_LCD_FCR_DIV_18		0x2
#define STM_LCD_FCR_DIV_19		0x3
#define STM_LCD_FCR_DIV_20		0x4
#define STM_LCD_FCR_DIV_21		0x5
#define STM_LCD_FCR_DIV_22		0x6
#define STM_LCD_FCR_DIV_23		0x7
#define STM_LCD_FCR_DIV_24		0x8
#define STM_LCD_FCR_DIV_25		0x9
#define STM_LCD_FCR_DIV_26		0xa
#define STM_LCD_FCR_DIV_27		0xb
#define STM_LCD_FCR_DIV_28		0xc
#define STM_LCD_FCR_DIV_29		0xd
#define STM_LCD_FCR_DIV_30		0xe
#define STM_LCD_FCR_DIV_31		0xf
#define STM_LCD_FCR_DIV_MASK		0xf

#define STM_LCD_FCR_BLINK		(16)
#define  STM_LCD_FCR_BLINK_DISABLE		0
#define  STM_LCD_FCR_BLINK_SEG0_COM0		1
#define  STM_LCD_FCR_BLINK_SEG0_COMALL		2
#define  STM_LCD_FCR_BLINK_SEGALL_COMALL	3
#define  STM_LCD_FCR_BLINK_MASK			3

#define STM_LCD_FCR_BLINKF		(13)
#define  STM_LCD_FCR_BLINKF_8			0
#define  STM_LCD_FCR_BLINKF_16			1
#define  STM_LCD_FCR_BLINKF_32			2
#define  STM_LCD_FCR_BLINKF_64			3
#define  STM_LCD_FCR_BLINKF_128			4
#define  STM_LCD_FCR_BLINKF_256			5
#define  STM_LCD_FCR_BLINKF_512			6
#define  STM_LCD_FCR_BLINKF_1024		7
#define  STM_LCD_FCR_BLINKF_MASK		7

#define STM_LCD_FCR_CC			(10)
#define  STM_LCD_FCR_CC_MASK			7

#define STM_LCD_FCR_DEAD		(7)
#define  STM_LCD_FCR_DEAD_MASK			7

#define STM_LCD_FCR_PON			(4)
#define  STM_LCD_FCR_PON_MASK			7

#define STM_LCD_FCR_UDDIE		(3)
#define STM_LCD_FCR_SOFIE		(1)
#define STM_LCD_FCR_HD			(0)

#define STM_LCD_SR_FCRSF		(5)
#define STM_LCD_SR_RDY			(4)
#define STM_LCD_SR_UDD			(3)
#define STM_LCD_SR_UDR			(2)
#define STM_LCD_SR_SOF			(1)
#define STM_LCD_SR_ENS			(0)

#define STM_LCD_CLR_UDDC		(3)
#define STM_LCD_CLR_SOFC		(1)

/* The SYSTICK starts at 0xe000e010 */

struct stm_systick {
	vuint32_t	csr;
	vuint32_t	rvr;
	vuint32_t	cvr;
	vuint32_t	calib;
};

extern struct stm_systick stm_systick;

#define STM_SYSTICK_CSR_ENABLE		0
#define STM_SYSTICK_CSR_TICKINT		1
#define STM_SYSTICK_CSR_CLKSOURCE	2
#define  STM_SYSTICK_CSR_CLKSOURCE_EXTERNAL		0
#define  STM_SYSTICK_CSR_CLKSOURCE_HCLK_8		1
#define STM_SYSTICK_CSR_COUNTFLAG	16

/* The NVIC starts at 0xe000e100, so add that to the offsets to find the absolute address */

struct stm_nvic {
	vuint32_t	iser;		/* 0x000 0xe000e100 Set Enable Register */

	uint8_t		_unused020[0x080 - 0x004];

	vuint32_t	icer;		/* 0x080 0xe000e180 Clear Enable Register */

	uint8_t		_unused0a0[0x100 - 0x084];

	vuint32_t	ispr;		/* 0x100 0xe000e200 Set Pending Register */

	uint8_t		_unused120[0x180 - 0x104];

	vuint32_t	icpr;		/* 0x180 0xe000e280 Clear Pending Register */

	uint8_t		_unused1a0[0x300 - 0x184];

	vuint32_t	ipr[8];		/* 0x300 0xe000e400 Priority Register */
};

extern struct stm_nvic stm_nvic;

#define IRQ_MASK(irq)	(1 << (irq))
#define IRQ_BOOL(v,irq)	(((v) >> (irq)) & 1)

static inline void
stm_nvic_set_enable(int irq) {
	stm_nvic.iser = IRQ_MASK(irq);
}

static inline void
stm_nvic_clear_enable(int irq) {
	stm_nvic.icer = IRQ_MASK(irq);
}

static inline int
stm_nvic_enabled(int irq) {
	return IRQ_BOOL(stm_nvic.iser, irq);
}

static inline void
stm_nvic_set_pending(int irq) {
	stm_nvic.ispr = IRQ_MASK(irq);
}

static inline void
stm_nvic_clear_pending(int irq) {
	stm_nvic.icpr = IRQ_MASK(irq);
}

static inline int
stm_nvic_pending(int irq) {
	return IRQ_BOOL(stm_nvic.ispr, irq);
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
	vuint32_t	mmfar;
	vuint32_t	bfar;
};

extern struct stm_scb stm_scb;

#define STM_SCB_AIRCR_VECTKEY		16
#define  STM_SCB_AIRCR_VECTKEY_KEY		0x05fa
#define STM_SCB_AIRCR_PRIGROUP		8
#define STM_SCB_AIRCR_SYSRESETREQ	2
#define STM_SCB_AIRCR_VECTCLRACTIVE	1
#define STM_SCB_AIRCR_VECTRESET		0

struct stm_mpu {
	vuint32_t	typer;
	vuint32_t	cr;
	vuint32_t	rnr;
	vuint32_t	rbar;

	vuint32_t	rasr;
	vuint32_t	rbar_a1;
	vuint32_t	rasr_a1;
	vuint32_t	rbar_a2;
	vuint32_t	rasr_a2;
	vuint32_t	rbar_a3;
	vuint32_t	rasr_a3;
};

extern struct stm_mpu stm_mpu;

#define STM_MPU_TYPER_IREGION	16
#define  STM_MPU_TYPER_IREGION_MASK	0xff
#define STM_MPU_TYPER_DREGION	8
#define  STM_MPU_TYPER_DREGION_MASK	0xff
#define STM_MPU_TYPER_SEPARATE	0

#define STM_MPU_CR_PRIVDEFENA	2
#define STM_MPU_CR_HFNMIENA	1
#define STM_MPU_CR_ENABLE	0

#define STM_MPU_RNR_REGION	0
#define STM_MPU_RNR_REGION_MASK		0xff

#define STM_MPU_RBAR_ADDR	5
#define STM_MPU_RBAR_ADDR_MASK		0x7ffffff

#define STM_MPU_RBAR_VALID	4
#define STM_MPU_RBAR_REGION	0
#define STM_MPU_RBAR_REGION_MASK	0xf

#define STM_MPU_RASR_XN		28
#define STM_MPU_RASR_AP		24
#define  STM_MPU_RASR_AP_NONE_NONE	0
#define  STM_MPU_RASR_AP_RW_NONE	1
#define  STM_MPU_RASR_AP_RW_RO		2
#define  STM_MPU_RASR_AP_RW_RW		3
#define  STM_MPU_RASR_AP_RO_NONE	5
#define  STM_MPU_RASR_AP_RO_RO		6
#define  STM_MPU_RASR_AP_MASK		7
#define STM_MPU_RASR_TEX	19
#define  STM_MPU_RASR_TEX_MASK		7
#define STM_MPU_RASR_S		18
#define STM_MPU_RASR_C		17
#define STM_MPU_RASR_B		16
#define STM_MPU_RASR_SRD	8
#define  STM_MPU_RASR_SRD_MASK		0xff
#define STM_MPU_RASR_SIZE	1
#define  STM_MPU_RASR_SIZE_MASK		0x1f
#define STM_MPU_RASR_ENABLE	0

#define isr(name) void stm_ ## name ## _isr(void);

isr(nmi)
isr(hardfault)
isr(memmanage)
isr(busfault)
isr(usagefault)
isr(svc)
isr(debugmon)
isr(pendsv)
isr(systick)
isr(wwdg)
isr(pvd)
isr(tamper_stamp)
isr(rtc_wkup)
isr(flash)
isr(rcc)
isr(exti0)
isr(exti1)
isr(exti2)
isr(exti3)
isr(exti4)
isr(dma1_channel1)
isr(dma1_channel2)
isr(dma1_channel3)
isr(dma1_channel4)
isr(dma1_channel5)
isr(dma1_channel6)
isr(dma1_channel7)
isr(adc1)
isr(usb_hp)
isr(usb_lp)
isr(dac)
isr(comp)
isr(exti9_5)
isr(lcd)
isr(tim9)
isr(tim10)
isr(tim11)
isr(tim2)
isr(tim3)
isr(tim4)
isr(i2c1_ev)
isr(i2c1_er)
isr(i2c2_ev)
isr(i2c2_er)
isr(spi1)
isr(spi2)
isr(usart1)
isr(usart2)
isr(usart3)
isr(exti15_10)
isr(rtc_alarm)
isr(usb_fs_wkup)
isr(tim6)
isr(tim7)

#undef isr

#define STM_ISR_WWDG_POS		0
#define STM_ISR_PVD_VDDIO2_POS		1
#define STM_ISR_RTC_POS			2
#define STM_ISR_FLASH_POS		3
#define STM_ISR_RCC_CRS_POS		4
#define STM_ISR_EXTI0_1_POS		5
#define STM_ISR_EXTI2_3_POS		6
#define STM_ISR_EXTI4_15_POS		7
#define STM_ISR_TSC_POS			8
#define STM_ISR_DMA_CH1_POS		9
#define STM_ISR_DMA_CH2_3_DMA2_CH1_2_POS	10
#define STM_ISR_DMA_CH44_5_6_7_DMA2_CH3_4_5_POS	11
#define STM_ISR_ADC_COMP_POS		12
#define STM_ISR_TIM1_BRK_UP_TRG_COM_POS	13
#define STM_ISR_TIM1_CC_POS		14
#define STM_ISR_TIM2_POS		15
#define STM_ISR_TIM3_POS		16
#define STM_ISR_TIM6_DAC_POS		17
#define STM_ISR_TIM7_POS		18
#define STM_ISR_TIM14_POS		19
#define STM_ISR_TIM15_POS		20
#define STM_ISR_TIM16_POS		21
#define STM_ISR_TIM17_POS		22
#define STM_ISR_I2C1_POS		23
#define STM_ISR_I2C2_POS		24
#define STM_ISR_SPI1_POS		25
#define STM_ISR_SPI2_POS		26
#define STM_ISR_USART1_POS		27
#define STM_ISR_USART2_POS		28
#define STM_ISR_UASART3_4_5_6_7_8_POS	29
#define STM_ISR_CEC_CAN_POS		30
#define STM_ISR_USB_POS			31

struct stm_syscfg {
	vuint32_t	cfgr1;
	vuint32_t	exticr[4];
	vuint32_t	cfgr2;
};

extern struct stm_syscfg stm_syscfg;

#define STM_SYSCFG_CFGR1_TIM3_DMA_RMP	30
#define STM_SYSCFG_CFGR1_TIM2_DMA_RMP	29
#define STM_SYSCFG_CFGR1_TIM1_DMA_RMP	28
#define STM_SYSCFG_CFGR1_I2C1_DMA_RMP	27
#define STM_SYSCFG_CFGR1_USART3_DMA_RMP	26
#define STM_SYSCFG_CFGR1_USART2_DMA_RMP	25
#define STM_SYSCFG_CFGR1_SPI2_DMA_RMP	24
#define STM_SYSCFG_CFGR1_I2C_PA10_FMP	23
#define STM_SYSCFG_CFGR1_I2C_PA9_FMP	22
#define STM_SYSCFG_CFGR1_I2C2_FMP	21
#define STM_SYSCFG_CFGR1_I2C1_FMP	20
#define STM_SYSCFG_CFGR1_I2C_PB9_FMP	19
#define STM_SYSCFG_CFGR1_I2C_PB8_FMP	18
#define STM_SYSCFG_CFGR1_I2C_PB7_FMP	17
#define STM_SYSCFG_CFGR1_I2C_PB6_FMP	16
#define STM_SYSCFG_CFGR1_TIM17_DMA_RMP2	14
#define STM_SYSCFG_CFGR1_TIM16_DMA_RMP2	13
#define STM_SYSCFG_CFGR1_TIM17_DMA_RMP	12
#define STM_SYSCFG_CFGR1_TIM16_DMA_RMP	11
#define STM_SYSCFG_CFGR1_USART1_RX_DMA_RMP	10
#define STM_SYSCFG_CFGR1_USART1_TX_DMA_RMP	9
#define STM_SYSCFG_CFGR1_ADC_DMA_RMP		8
#define STM_SYSCFG_CFGR1_IRDA_ENV_SEL	6
#define  STM_SYSCFG_CFGR1_IRDA_ENV_SEL_TIMER16	0
#define  STM_SYSCFG_CFGR1_IRDA_ENV_SEL_USART1	1
#define  STM_SYSCFG_CFGR1_IRDA_ENV_SEL_USART4	2
#define STM_SYSCFG_CFGR1_PA11_PA12_RMP	4
#define STM_SYSCFG_CFGR1_MEM_MODE	0
#define  STM_SYSCFG_CFGR1_MEM_MODE_MAIN_FLASH	0
#define  STM_SYSCFG_CFGR1_MEM_MODE_SYSTEM_FLASH	1
#define  STM_SYSCFG_CFGR1_MEM_MODE_SRAM		3
#define  STM_SYSCFG_CFGR1_MEM_MODE_MASK		3

#if 0
static inline void
stm_exticr_set(struct stm_gpio *gpio, int pin) {
	uint8_t	reg = pin >> 2;
	uint8_t	shift = (pin & 3) << 2;
	uint8_t	val = 0;

	/* Enable SYSCFG */
	stm_rcc.apb2enr |= (1 << STM_RCC_APB2ENR_SYSCFGCOMPEN);

	if (gpio == &stm_gpioa)
		val = STM_SYSCFG_EXTICR_PA;
	else if (gpio == &stm_gpiob)
		val = STM_SYSCFG_EXTICR_PB;
	else if (gpio == &stm_gpioc)
		val = STM_SYSCFG_EXTICR_PC;
	else if (gpio == &stm_gpiof)
		val = STM_SYSCFG_EXTICR_PF;

	stm_syscfg.exticr[reg] = (stm_syscfg.exticr[reg] & ~(0xf << shift)) | val << shift;
}
#endif


struct stm_dma_channel {
	vuint32_t	ccr;
	vuint32_t	cndtr;
	vvoid_t		cpar;
	vvoid_t		cmar;
	vuint32_t	reserved;
};

#define STM_NUM_DMA	6

struct stm_dma {
	vuint32_t		isr;
	vuint32_t		ifcr;
	struct stm_dma_channel	channel[STM_NUM_DMA];
};

extern struct stm_dma stm_dma;

/* DMA channels go from 1 to 6, instead of 0 to 5 (sigh)
 */

#define STM_DMA_INDEX(channel)		((channel) - 1)

#define STM_DMA_ISR(index)		((index) << 2)
#define STM_DMA_ISR_MASK			0xf
#define STM_DMA_ISR_TEIF			3
#define STM_DMA_ISR_HTIF			2
#define STM_DMA_ISR_TCIF			1
#define STM_DMA_ISR_GIF				0

#define STM_DMA_IFCR(index)		((index) << 2)
#define STM_DMA_IFCR_MASK			0xf
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

/* DMA channel assignments. When a peripheral has multiple channels
 * (indicated with _<number>), then it can be configured to either
 * channel using syscfg.cfgr1
 */

#define STM_DMA_CHANNEL_ADC_1		1
#define STM_DMA_CHANNEL_ADC_2		2

#define STM_DMA_CHANNEL_SPI1_RX		2
#define STM_DMA_CHANNEL_SPI1_TX		3

#define STM_DMA_CHANNEL_SPI2_RX		4
#define STM_DMA_CHANNEL_SPI2_TX		5

#define STM_DMA_CHANNEL_USART1_TX_1	2
#define STM_DMA_CHANNEL_USART1_RX_1	3
#define STM_DMA_CHANNEL_USART1_TX_2	4
#define STM_DMA_CHANNEL_USART1_RX_2	5

#define STM_DMA_CHANNEL_USART2_RX	4
#define STM_DMA_CHANNEL_USART2_TX	5

#define STM_DMA_CHANNEL_I2C1_TX		2
#define STM_DMA_CHANNEL_I2C1_RX		3

#define STM_DMA_CHANNEL_I2C2_TX		4
#define STM_DMA_CHANNEL_I2C2_RX		5

#define STM_DMA_CHANNEL_TIM1_CH1	2
#define STM_DMA_CHANNEL_TIM1_CH2	3
#define STM_DMA_CHANNEL_TIM1_CH4	4
#define STM_DMA_CHANNEL_TIM1_TRIG	4
#define STM_DMA_CHANNEL_TIM1_COM	4
#define STM_DMA_CHANNEL_TIM1_CH3	5
#define STM_DMA_CHANNEL_TIM1_UP		5

#define STM_DMA_CHANNEL_TIM2_CH3	1
#define STM_DMA_CHANNEL_TIM2_UP		2
#define STM_DMA_CHANNEL_TIM2_CH2	3
#define STM_DMA_CHANNEL_TIM2_CH4	4
#define STM_DMA_CHANNEL_TIM2_CH1	5

#define STM_DMA_CHANNEL_TIM3_CH3	2
#define STM_DMA_CHANNEL_TIM3_CH4	3
#define STM_DMA_CHANNEL_TIM3_UP		3
#define STM_DMA_CHANNEL_TIM3_CH1	4
#define STM_DMA_CHANNEL_TIM3_TRIG	4

#define STM_DMA_CHANNEL_TIM6_UP_DAC	2

#define STM_DMA_CHANNEL_TIM15_CH1	5
#define STM_DMA_CHANNEL_TIM15_UP	5
#define STM_DMA_CHANNEL_TIM15_TRIG	5
#define STM_DMA_CHANNEL_TIM15_COM	5

#define STM_DMA_CHANNEL_TIM16_CH1_1	3
#define STM_DMA_CHANNEL_TIM16_UP_1	3
#define STM_DMA_CHANNEL_TIM16_CH1_2	4
#define STM_DMA_CHANNEL_TIM16_UP_2	4

#define STM_DMA_CHANNEL_TIM17_CH1_1	1
#define STM_DMA_CHANNEL_TIM17_UP_1	1
#define STM_DMA_CHANNEL_TIM17_CH1_2	2
#define STM_DMA_CHANNEL_TIM17_UP_2	2

/*
 * Only spi channel 1 and 2 can use DMA
 */
#define STM_NUM_SPI	2

struct stm_spi {
	vuint32_t	cr1;
	vuint32_t	cr2;
	vuint32_t	sr;
	vuint32_t	dr;
	vuint32_t	crcpr;
	vuint32_t	rxcrcr;
	vuint32_t	txcrcr;
};

extern struct stm_spi stm_spi1, stm_spi2, stm_spi3;

/* SPI channels go from 1 to 3, instead of 0 to 2 (sigh)
 */

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
#define  STM_SPI_CR1_BR_MASK			7

#define STM_SPI_CR1_MSTR		2
#define STM_SPI_CR1_CPOL		1
#define STM_SPI_CR1_CPHA		0

#define STM_SPI_CR2_TXEIE	7
#define STM_SPI_CR2_RXNEIE	6
#define STM_SPI_CR2_ERRIE	5
#define STM_SPI_CR2_SSOE	2
#define STM_SPI_CR2_TXDMAEN	1
#define STM_SPI_CR2_RXDMAEN	0

#define STM_SPI_SR_BSY		7
#define STM_SPI_SR_OVR		6
#define STM_SPI_SR_MODF		5
#define STM_SPI_SR_CRCERR	4
#define STM_SPI_SR_TXE		1
#define STM_SPI_SR_RXNE		0

struct stm_adc {
	vuint32_t	isr;
	vuint32_t	ier;
	vuint32_t	cr;
	vuint32_t	cfgr1;

	vuint32_t	cfgr2;
	vuint32_t	smpr;
	vuint32_t	r_18;
	vuint32_t	r_1c;

	vuint32_t	tr;
	vuint32_t	r_24;
	vuint32_t	chselr;
	vuint32_t	r_2c;

	vuint32_t	r_30[4];

	vuint32_t	dr;

	uint8_t		r_44[0x308 - 0x44];
	vuint32_t	ccr;
};

extern struct stm_adc stm_adc;

#define STM_ADC_ISR_AWD		7
#define STM_ADC_ISR_OVR		4
#define STM_ADC_ISR_EOSEQ	3
#define STM_ADC_ISR_EOC		2
#define STM_ADC_ISR_EOSMP	1
#define STM_ADC_ISR_ADRDY	0

#define STM_ADC_IER_AWDIE	7
#define STM_ADC_IER_OVRIE	4
#define STM_ADC_IER_EOSEQIE	3
#define STM_ADC_IER_EOCIE	2
#define STM_ADC_IER_EOSMPIE	1
#define STM_ADC_IER_ADRDYIE	0

#define STM_ADC_CR_ADCAL	31
#define STM_ADC_CR_ADSTP	4
#define STM_ADC_CR_ADSTART	2
#define STM_ADC_CR_ADDIS	1
#define STM_ADC_CR_ADEN		0

#define STM_ADC_CFGR1_AWDCH	26
#define STM_ADC_CFGR1_AWDEN	23
#define STM_ADC_CFGR1_AWDSGL	22
#define STM_ADC_CFGR1_DISCEN	16
#define STM_ADC_CFGR1_AUTOOFF	15
#define STM_ADC_CFGR1_WAIT	14
#define STM_ADC_CFGR1_CONT	13
#define STM_ADC_CFGR1_OVRMOD	12
#define STM_ADC_CFGR1_EXTEN	10
#define  STM_ADC_CFGR1_EXTEN_DISABLE	0
#define  STM_ADC_CFGR1_EXTEN_RISING	1
#define  STM_ADC_CFGR1_EXTEN_FALLING	2
#define  STM_ADC_CFGR1_EXTEN_BOTH	3
#define  STM_ADC_CFGR1_EXTEN_MASK	3

#define STM_ADC_CFGR1_EXTSEL	6
#define STM_ADC_CFGR1_ALIGN	5
#define STM_ADC_CFGR1_RES	3
#define  STM_ADC_CFGR1_RES_12		0
#define  STM_ADC_CFGR1_RES_10		1
#define  STM_ADC_CFGR1_RES_8		2
#define  STM_ADC_CFGR1_RES_6		3
#define  STM_ADC_CFGR1_RES_MASK		3
#define STM_ADC_CFGR1_SCANDIR	2
#define  STM_ADC_CFGR1_SCANDIR_UP	0
#define  STM_ADC_CFGR1_SCANDIR_DOWN	1
#define STM_ADC_CFGR1_DMACFG	1
#define  STM_ADC_CFGR1_DMACFG_ONESHOT	0
#define  STM_ADC_CFGR1_DMACFG_CIRCULAR	1
#define STM_ADC_CFGR1_DMAEN	0

#define STM_ADC_CFGR2_CKMODE	30
#define  STM_ADC_CFGR2_CKMODE_ADCCLK	0
#define  STM_ADC_CFGR2_CKMODE_PCLK_2	1
#define  STM_ADC_CFGR2_CKMODE_PCLK_4	2

#define STM_ADC_SMPR_SMP	0
#define  STM_ADC_SMPR_SMP_1_5		0
#define  STM_ADC_SMPR_SMP_7_5		1
#define  STM_ADC_SMPR_SMP_13_5		2
#define  STM_ADC_SMPR_SMP_28_5		3
#define  STM_ADC_SMPR_SMP_41_5		4
#define  STM_ADC_SMPR_SMP_55_5		5
#define  STM_ADC_SMPR_SMP_71_5		6
#define  STM_ADC_SMPR_SMP_239_5		7

#define STM_ADC_TR_HT		16
#define STM_ADC_TR_LT		0

#define STM_ADC_CCR_VBATEN	24
#define STM_ADC_CCR_TSEN	23
#define STM_ADC_CCR_VREFEN	22

struct stm_cal {
	uint16_t	ts_cal_cold;	/* 30°C */
	uint16_t	vrefint_cal;
	uint16_t	unused_c0;
	uint16_t	ts_cal_hot;	/* 110°C */
};

extern struct stm_cal	stm_cal;

#define stm_temp_cal_cold	30
#define stm_temp_cal_hot	110

struct stm_dbgmcu {
	uint32_t	idcode;
};

extern struct stm_dbgmcu	stm_dbgmcu;

static inline uint16_t
stm_dev_id(void) {
	return stm_dbgmcu.idcode & 0xfff;
}

struct stm_flash_size {
	uint16_t	f_size;
};

extern struct stm_flash_size	stm_flash_size_04x;

/* Returns flash size in bytes */
extern uint32_t
stm_flash_size(void);

struct stm_device_id {
	uint32_t	u_id0;
	uint32_t	u_id1;
	uint32_t	u_id2;
};

extern struct stm_device_id	stm_device_id;

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
#define  STM_I2C_CR2_FREQ_2_MHZ		2
#define  STM_I2C_CR2_FREQ_4_MHZ		4
#define  STM_I2C_CR2_FREQ_8_MHZ		8
#define  STM_I2C_CR2_FREQ_16_MHZ	16
#define  STM_I2C_CR2_FREQ_32_MHZ	32
#define  STM_I2C_CR2_FREQ_MASK		0x3f

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
#define  STM_I2C_SR2_PEC_MASK	0xff00
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
#define  STM_I2C_CCR_MASK	0x7ff

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

	uint32_t	reserved_50;
};

extern struct stm_tim234 stm_tim2, stm_tim3, stm_tim4;

#define STM_TIM234_CR1_CKD	8
#define  STM_TIM234_CR1_CKD_1		0
#define  STM_TIM234_CR1_CKD_2		1
#define  STM_TIM234_CR1_CKD_4		2
#define  STM_TIM234_CR1_CKD_MASK	3
#define STM_TIM234_CR1_ARPE	7
#define STM_TIM234_CR1_CMS	5
#define  STM_TIM234_CR1_CMS_EDGE	0
#define  STM_TIM234_CR1_CMS_CENTER_1	1
#define  STM_TIM234_CR1_CMS_CENTER_2	2
#define  STM_TIM234_CR1_CMS_CENTER_3	3
#define  STM_TIM234_CR1_CMS_MASK	3
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
#define  STM_TIM234_CR2_MMS_MASK		7
#define STM_TIM234_CR2_CCDS	3

#define STM_TIM234_SMCR_ETP	15
#define STM_TIM234_SMCR_ECE	14
#define STM_TIM234_SMCR_ETPS	12
#define  STM_TIM234_SMCR_ETPS_OFF		0
#define  STM_TIM234_SMCR_ETPS_DIV_2	       	1
#define  STM_TIM234_SMCR_ETPS_DIV_4		2
#define  STM_TIM234_SMCR_ETPS_DIV_8		3
#define  STM_TIM234_SMCR_ETPS_MASK		3
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
#define  STM_TIM234_SMCR_ETF_MASK		15
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
#define  STM_TIM234_SMCR_TS_MASK		7
#define STM_TIM234_SMCR_OCCS	3
#define STM_TIM234_SMCR_SMS	0
#define  STM_TIM234_SMCR_SMS_DISABLE		0
#define  STM_TIM234_SMCR_SMS_ENCODER_MODE_1	1
#define  STM_TIM234_SMCR_SMS_ENCODER_MODE_2	2
#define  STM_TIM234_SMCR_SMS_ENCODER_MODE_3	3
#define  STM_TIM234_SMCR_SMS_RESET_MODE		4
#define  STM_TIM234_SMCR_SMS_GATED_MODE		5
#define  STM_TIM234_SMCR_SMS_TRIGGER_MODE	6
#define  STM_TIM234_SMCR_SMS_EXTERNAL_CLOCK	7
#define  STM_TIM234_SMCR_SMS_MASK		7

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
#define  STM_TIM234_CCMR1_OC2M_MASK			7
#define STM_TIM234_CCMR1_OC2PE	11
#define STM_TIM234_CCMR1_OC2FE	10
#define STM_TIM234_CCMR1_CC2S	8
#define  STM_TIM234_CCMR1_CC2S_OUTPUT			0
#define  STM_TIM234_CCMR1_CC2S_INPUT_TI2		1
#define  STM_TIM234_CCMR1_CC2S_INPUT_TI1		2
#define  STM_TIM234_CCMR1_CC2S_INPUT_TRC		3
#define  STM_TIM234_CCMR1_CC2S_MASK			3

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
#define  STM_TIM234_CCMR1_OC1M_MASK			7
#define STM_TIM234_CCMR1_OC1PE	11
#define STM_TIM234_CCMR1_OC1FE	2
#define STM_TIM234_CCMR1_CC1S	0
#define  STM_TIM234_CCMR1_CC1S_OUTPUT			0
#define  STM_TIM234_CCMR1_CC1S_INPUT_TI1		1
#define  STM_TIM234_CCMR1_CC1S_INPUT_TI2		2
#define  STM_TIM234_CCMR1_CC1S_INPUT_TRC		3
#define  STM_TIM234_CCMR1_CC1S_MASK			3

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
#define  STM_TIM234_CCMR2_OC4M_MASK			7
#define STM_TIM234_CCMR2_OC4PE	11
#define STM_TIM234_CCMR2_OC4FE	10
#define STM_TIM234_CCMR2_CC4S	8
#define  STM_TIM234_CCMR2_CC4S_OUTPUT			0
#define  STM_TIM234_CCMR2_CC4S_INPUT_TI4		1
#define  STM_TIM234_CCMR2_CC4S_INPUT_TI3		2
#define  STM_TIM234_CCMR2_CC4S_INPUT_TRC		3
#define  STM_TIM234_CCMR2_CC4S_MASK			3

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
#define  STM_TIM234_CCMR2_OC3M_MASK			7
#define STM_TIM234_CCMR2_OC3PE	11
#define STM_TIM234_CCMR2_OC3FE	2
#define STM_TIM234_CCMR2_CC3S	0
#define  STM_TIM234_CCMR2_CC3S_OUTPUT			0
#define  STM_TIM234_CCMR2_CC3S_INPUT_TI3		1
#define  STM_TIM234_CCMR2_CC3S_INPUT_TI4		2
#define  STM_TIM234_CCMR2_CC3S_INPUT_TRC		3
#define  STM_TIM234_CCMR2_CC3S_MASK			3

#define STM_TIM234_CCER_CC4NP	15
#define STM_TIM234_CCER_CC4P	13
#define STM_TIM234_CCER_CC4E	12
#define STM_TIM234_CCER_CC3NP	11
#define STM_TIM234_CCER_CC3P	9
#define STM_TIM234_CCER_CC3E	8
#define STM_TIM234_CCER_CC2NP	7
#define STM_TIM234_CCER_CC2P	5
#define STM_TIM234_CCER_CC2E	4
#define STM_TIM234_CCER_CC1NP	3
#define STM_TIM234_CCER_CC1P	1
#define STM_TIM234_CCER_CC1E	0

struct stm_usb {
	struct {
		vuint16_t	r;
		uint16_t	_;
	} epr[8];
	uint8_t		reserved_20[0x40 - 0x20];
	vuint16_t	cntr;
	uint16_t	reserved_42;
	vuint16_t	istr;
	uint16_t	reserved_46;
	vuint16_t	fnr;
	uint16_t	reserved_4a;
	vuint16_t	daddr;
	uint16_t	reserved_4e;
	vuint16_t	btable;
	uint16_t	reserved_52;
	vuint16_t	lpmcsr;
	uint16_t	reserved_56;
	vuint16_t	bcdr;
	uint16_t	reserved_5a;
};

extern struct stm_usb stm_usb;

#define STM_USB_EPR_CTR_RX	15
#define  STM_USB_EPR_CTR_RX_WRITE_INVARIANT		1
#define STM_USB_EPR_DTOG_RX	14
#define STM_USB_EPR_DTOG_RX_WRITE_INVARIANT		0
#define STM_USB_EPR_STAT_RX	12
#define  STM_USB_EPR_STAT_RX_DISABLED			0
#define  STM_USB_EPR_STAT_RX_STALL			1
#define  STM_USB_EPR_STAT_RX_NAK			2
#define  STM_USB_EPR_STAT_RX_VALID			3
#define  STM_USB_EPR_STAT_RX_MASK			3
#define  STM_USB_EPR_STAT_RX_WRITE_INVARIANT		0
#define STM_USB_EPR_SETUP	11
#define STM_USB_EPR_EP_TYPE	9
#define  STM_USB_EPR_EP_TYPE_BULK			0
#define  STM_USB_EPR_EP_TYPE_CONTROL			1
#define  STM_USB_EPR_EP_TYPE_ISO			2
#define  STM_USB_EPR_EP_TYPE_INTERRUPT			3
#define  STM_USB_EPR_EP_TYPE_MASK			3
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
#define  STM_USB_EPR_STAT_TX_MASK			3
#define STM_USB_EPR_EA		0
#define  STM_USB_EPR_EA_MASK				0xf

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
#define STM_USB_L1REQ		7
#define STM_USB_ISTR_DIR	4
#define STM_USB_ISTR_EP_ID	0
#define  STM_USB_ISTR_EP_ID_MASK		0xf

#define STM_USB_FNR_RXDP	15
#define STM_USB_FNR_RXDM	14
#define STM_USB_FNR_LCK		13
#define STM_USB_FNR_LSOF	11
#define  STM_USB_FNR_LSOF_MASK			0x3
#define STM_USB_FNR_FN		0
#define  STM_USB_FNR_FN_MASK			0x7ff

#define STM_USB_DADDR_EF	7
#define STM_USB_DADDR_ADD	0
#define  STM_USB_DADDR_ADD_MASK			0x7f

#define STM_USB_BCDR_DPPU	15
#define STM_USB_BCDR_PS2DET	7
#define STM_USB_BCDR_SDET	6
#define STM_USB_BCDR_PDET	5
#define STM_USB_BCDR_DCDET	4
#define STM_USB_BCDR_SDEN	3
#define STM_USB_BCDR_PDEN	2
#define STM_USB_BCDR_DCDEN	1
#define STM_USB_BCDR_BCDEN	0

union stm_usb_bdt {
	struct {
		vuint16_t	addr_tx;
		vuint16_t	count_tx;
		vuint16_t	addr_rx;
		vuint16_t	count_rx;
	} single;
	struct {
		vuint16_t	addr;
		vuint16_t	count;
	} double_tx[2];
	struct {
		vuint16_t	addr;
		vuint16_t	count;
	} double_rx[2];
};

#define STM_USB_BDT_COUNT_RX_BL_SIZE	15
#define STM_USB_BDT_COUNT_RX_NUM_BLOCK	10
#define  STM_USB_BDT_COUNT_RX_NUM_BLOCK_MASK	0x1f
#define STM_USB_BDT_COUNT_RX_COUNT_RX	0
#define  STM_USB_BDT_COUNT_RX_COUNT_RX_MASK	0x1ff

#define STM_USB_BDT_SIZE	8

extern uint8_t stm_usb_sram[];

struct stm_exti {
	vuint32_t	imr;
	vuint32_t	emr;
	vuint32_t	rtsr;
	vuint32_t	ftsr;

	vuint32_t	swier;
	vuint32_t	pr;
};

extern struct stm_exti stm_exti;

#endif /* _STM32F0_H_ */
