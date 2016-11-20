/*
 * Copyright © 2016 Keith Packard <keithp@keithp.com>
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
#include "ao_vga.h"

/* VGA output from the SPI port */

struct ao_modeline {
	long	dot_clock;	/* in Hz */

	/* All timings are in pixels, with the first pixel out at 0,0 */
	int	hactive;	/* active pixels */
	int	hsync_start;	/* start of hsync pulse */
	int	hsync_end;	/* end of hsync pulse */
	int	htotal;		/* total h pixels */

	int	vactive;	/* active scalines */
	int	vsync_start;	/* start of vsync pulse */
	int	vsync_end;	/* end of vsync pulse */
	int	vtotal;		/* total scanlines */
};

const struct ao_modeline vga_640x480x60 = {
	.dot_clock	= 23856000,	/* 23.86MHz dot, 29.82kHz line, 60.00Hz frame */

	.hactive	= 640,
	.hsync_start	= 656,
	.hsync_end	= 720,
	.htotal		= 800,

	.vactive	= 480,
	.vsync_start	= 481,
	.vsync_end	= 484,
	.vtotal		= 497
};

const struct ao_modeline vga_640x480x30 = {
	.dot_clock	= 120000000,	/* 12.00MHz dot, 29.82kHz line, 30.00Hz frame */

	.hactive	= 640,
	.hsync_start	= 656,
	.hsync_end	= 720,
	.htotal		= 800,

	.vactive	= 480,
	.vsync_start	= 481,
	.vsync_end	= 484,
	.vtotal		= 497
};

#define	mode	vga_640x480x60

#define WIDTH_BYTES	(AO_VGA_WIDTH >> 3)
#define SCANOUT		((WIDTH_BYTES + 2) >> 1)

uint32_t	ao_vga_fb[AO_VGA_STRIDE * AO_VGA_HEIGHT];

static uint32_t	*scanline;

#define DMA_INDEX	STM_DMA_INDEX(STM_DMA_CHANNEL_SPI2_TX)

static int	line;
static int	vblank;

#define DMA_CCR(en)	((0 << STM_DMA_CCR_MEM2MEM) |			\
			 (STM_DMA_CCR_PL_VERY_HIGH << STM_DMA_CCR_PL) | \
			 (STM_DMA_CCR_MSIZE_16 << STM_DMA_CCR_MSIZE) |	\
			 (STM_DMA_CCR_PSIZE_16 << STM_DMA_CCR_PSIZE) |	\
			 (1 << STM_DMA_CCR_MINC) |			\
			 (0 << STM_DMA_CCR_PINC) |			\
			 (0 << STM_DMA_CCR_CIRC) |			\
			 (STM_DMA_CCR_DIR_MEM_TO_PER << STM_DMA_CCR_DIR) | \
			 (0 << STM_DMA_CCR_TCIE) |			\
			 (en << STM_DMA_CCR_EN))

int vblank_off = 13;

void stm_tim2_isr(void)
{
	ao_arch_block_interrupts();
	if (!vblank) {
		/* Disable */
		stm_dma.channel[DMA_INDEX].ccr = DMA_CCR(0);
		/* Reset DMA engine for the next scanline */
		stm_dma.channel[DMA_INDEX].cndtr = SCANOUT;
		/* Enable */
		stm_dma.channel[DMA_INDEX].ccr = DMA_CCR(1);
	}
	stm_tim2.sr = ~(1 << STM_TIM234_SR_CC2IF);
	line = stm_tim3.cnt;
 	if (vblank_off <= line && line < (AO_VGA_HEIGHT << 1) + vblank_off + 12) {
		vblank = 0;
		if ((line - vblank_off) & 1)
			scanline += AO_VGA_STRIDE;
	} else {
		scanline = ao_vga_fb;
		vblank = 1;
	}
	stm_dma.channel[DMA_INDEX].cmar = scanline;
	ao_arch_release_interrupts();
}

static void
ao_vga_fb_init(void)
{
	ao_solid(0x0, AO_ALLONES,
		 ao_vga_fb,
		 AO_VGA_STRIDE,
		 0,
		 AO_VGA_WIDTH,
		 AO_VGA_HEIGHT);

	ao_solid(0x0, 0x0,
		 ao_vga_fb + 10 * AO_VGA_STRIDE,
		 AO_VGA_STRIDE,
		 10,
		 10,
		 10);


	ao_solid(0x0, 0x0,
		 ao_vga_fb + 220 * AO_VGA_STRIDE,
		 AO_VGA_STRIDE,
		 10,
		 10,
		 10);

	ao_solid(0x0, 0x0,
		 ao_vga_fb + 10 * AO_VGA_STRIDE,
		 AO_VGA_STRIDE,
		 220,
		 10,
		 10);

	ao_solid(0x0, 0x0,
		 ao_vga_fb + 220 * AO_VGA_STRIDE,
		 AO_VGA_STRIDE,
		 220,
		 10,
		 10);

	ao_text("Hello, Bdale!",
		ao_vga_fb + 100 * AO_VGA_STRIDE,
		AO_VGA_STRIDE,
		20);

	ao_text("UL",
		ao_vga_fb,
		AO_VGA_STRIDE,
		0);

	ao_text("BL",
		ao_vga_fb + (240 - 7) * AO_VGA_STRIDE,
		AO_VGA_STRIDE,
		0);
}

void
ao_vga_init(void)
{
	/* Initialize spi2 using PB15 for output */
	stm_rcc.ahbenr |= (1 << STM_RCC_AHBENR_GPIOBEN);

	stm_ospeedr_set(&stm_gpiob, 15, STM_OSPEEDR_40MHz);
	stm_afr_set(&stm_gpiob, 15, STM_AFR_AF5);

	/* turn on SPI */
	stm_rcc.apb1enr |= (1 << STM_RCC_APB1ENR_SPI2EN);

	stm_spi2.cr1 = ((1 << STM_SPI_CR1_BIDIMODE) |		/* Two wire mode */
			(1 << STM_SPI_CR1_BIDIOE) |
			(0 << STM_SPI_CR1_CRCEN) |		/* CRC disabled */
			(0 << STM_SPI_CR1_CRCNEXT) |
			(1 << STM_SPI_CR1_DFF) |
			(0 << STM_SPI_CR1_RXONLY) |
			(1 << STM_SPI_CR1_SSM) |        	/* Software SS handling */
			(1 << STM_SPI_CR1_SSI) |		/*  ... */
			(1 << STM_SPI_CR1_LSBFIRST) |		/* Little endian */
			(1 << STM_SPI_CR1_SPE) |		/* Enable SPI unit */
			(0 << STM_SPI_CR1_BR) |			/* baud rate to pclk/2 */
			(1 << STM_SPI_CR1_MSTR) |
			(0 << STM_SPI_CR1_CPOL) |		/* Format 0 */
			(0 << STM_SPI_CR1_CPHA));
	stm_spi2.cr2 = ((0 << STM_SPI_CR2_TXEIE) |
			(0 << STM_SPI_CR2_RXNEIE) |
			(0 << STM_SPI_CR2_ERRIE) |
			(0 << STM_SPI_CR2_SSOE) |
			(1 << STM_SPI_CR2_TXDMAEN) |
			(0 << STM_SPI_CR2_RXDMAEN));

	(void) stm_spi2.dr;
	(void) stm_spi2.sr;

	/* Grab the DMA channel for SPI2 MOSI */
	stm_dma.channel[DMA_INDEX].cpar = &stm_spi2.dr;
	stm_dma.channel[DMA_INDEX].cmar = ao_vga_fb;

	/* hclock on timer 2 */

	/* Turn on timer 2 */
	stm_rcc.apb1enr |= (1 << STM_RCC_APB1ENR_TIM2EN);

	/* Turn on GPIOA */
	stm_rcc.ahbenr |= (1 << STM_RCC_AHBENR_GPIOAEN);

	stm_tim2.psc = 0;

	/* Disable channels while modifying */
	stm_tim2.ccer = 0;

	/* Channel 1 hsync PWM values */
	stm_tim2.ccr1 = mode.hsync_end - mode.hsync_start;

	/* Channel 2 trigger scanout */
	/* wait for the time to start scanout */
	stm_tim2.ccr2 = 90;

	stm_tim2.ccmr1 = ((0 << STM_TIM234_CCMR1_OC2CE) |
			  (STM_TIM234_CCMR1_OC2M_SET_HIGH_ON_MATCH << STM_TIM234_CCMR1_OC2M)  |
			  (1 << STM_TIM234_CCMR1_OC2PE) |
			  (0 << STM_TIM234_CCMR1_OC2FE) |

			  (0 << STM_TIM234_CCMR1_OC1CE) |
			  (STM_TIM234_CCMR1_OC1M_PWM_MODE_1 << STM_TIM234_CCMR1_OC1M)  |
			  (1 << STM_TIM234_CCMR1_OC1PE) |
			  (0 << STM_TIM234_CCMR1_OC1FE) |
			  (STM_TIM234_CCMR1_CC1S_OUTPUT << STM_TIM234_CCMR1_CC1S));

	stm_tim2.arr = mode.htotal;
	stm_tim2.cnt = 0;

	/* Update the register contents */
	stm_tim2.egr |= (1 << STM_TIM234_EGR_UG);

	/* Enable the timer */

	/* Enable the output */
	stm_tim2.ccer = ((0 << STM_TIM234_CCER_CC1NP) |
			 (STM_TIM234_CCER_CC1P_ACTIVE_LOW << STM_TIM234_CCER_CC1P) |
			 (1 << STM_TIM234_CCER_CC1E));

	stm_tim2.cr2 = ((0 << STM_TIM234_CR2_TI1S) |
			(STM_TIM234_CR2_MMS_UPDATE << STM_TIM234_CR2_MMS) |
			(0 << STM_TIM234_CR2_CCDS));

	stm_tim2.smcr = 0;

	stm_tim2.dier = ((1 << STM_TIM234_DIER_CC2IE));

	stm_tim2.cr1 = ((STM_TIM234_CR1_CKD_1 << STM_TIM234_CR1_CKD) |
			(1 << STM_TIM234_CR1_ARPE) |
			(STM_TIM234_CR1_CMS_EDGE << STM_TIM234_CR1_CMS) |
			(STM_TIM234_CR1_DIR_UP << STM_TIM234_CR1_DIR) |
			(0 << STM_TIM234_CR1_OPM) |
			(1 << STM_TIM234_CR1_URS) |
			(0 << STM_TIM234_CR1_UDIS) |
			(0 << STM_TIM234_CR1_CEN));

	/* Configure pins */

	/* PA5 is Timer 2 CH1 output */
	stm_ospeedr_set(&stm_gpioa, 5, STM_OSPEEDR_40MHz);
	stm_afr_set(&stm_gpioa, 5, STM_AFR_AF1);

	/* Turn on timer 3, slaved to timer 1 using ITR1 (table 61) */

	/* Use CH1 on PB6 (AF2) */

	stm_rcc.apb1enr |= (1 << STM_RCC_APB1ENR_TIM3EN);

	/* Turn on GPIOB */
	stm_rcc.ahbenr |= (1 << STM_RCC_AHBENR_GPIOBEN);

	/* No prescale */
	stm_tim3.psc = 0;

	/* Channel 1 vsync PWM values */
	stm_tim3.ccr1 = mode.vsync_end - mode.vsync_start;
	stm_tim3.ccmr1 = ((0 << STM_TIM234_CCMR1_OC2CE) |
			  (0 << STM_TIM234_CCMR1_OC2PE) |
			  (0 << STM_TIM234_CCMR1_OC2FE) |

			  (0 << STM_TIM234_CCMR1_OC1CE) |
			  (STM_TIM234_CCMR1_OC1M_PWM_MODE_1 << STM_TIM234_CCMR1_OC1M)  |
			  (1 << STM_TIM234_CCMR1_OC1PE) |
			  (0 << STM_TIM234_CCMR1_OC1FE) |
			  (STM_TIM234_CCMR1_CC1S_OUTPUT << STM_TIM234_CCMR1_CC1S));

	stm_tim3.arr = mode.vtotal;
	stm_tim3.cnt = 0;

	/* Update the register contents */
	stm_tim3.egr |= (1 << STM_TIM234_EGR_UG);

	/* Enable the timer */

	/* Enable the output */
	stm_tim3.ccer = ((0 << STM_TIM234_CCER_CC1NP) |
			 (STM_TIM234_CCER_CC1P_ACTIVE_LOW << STM_TIM234_CCER_CC1P) |
			 (1 << STM_TIM234_CCER_CC1E));

	stm_tim3.cr2 = ((0 << STM_TIM234_CR2_TI1S) |
			(STM_TIM234_CR2_MMS_UPDATE << STM_TIM234_CR2_MMS) |
			(0 << STM_TIM234_CR2_CCDS));

	stm_tim3.smcr = 0;
	stm_tim3.smcr = ((0 << STM_TIM234_SMCR_ETP) |
			 (0 << STM_TIM234_SMCR_ECE) |
			 (STM_TIM234_SMCR_ETPS_OFF << STM_TIM234_SMCR_ETPS) |
			 (STM_TIM234_SMCR_ETF_NONE << STM_TIM234_SMCR_ETF) |
			 (0 << STM_TIM234_SMCR_MSM) |
			 (STM_TIM234_SMCR_TS_ITR1 << STM_TIM234_SMCR_TS) |
			 (0 << STM_TIM234_SMCR_OCCS) |
			 (STM_TIM234_SMCR_SMS_EXTERNAL_CLOCK << STM_TIM234_SMCR_SMS));

	stm_tim3.dier = 0;

	stm_tim3.cr1 = ((STM_TIM234_CR1_CKD_1 << STM_TIM234_CR1_CKD) |
			(1 << STM_TIM234_CR1_ARPE) |
			(STM_TIM234_CR1_CMS_EDGE << STM_TIM234_CR1_CMS) |
			(STM_TIM234_CR1_DIR_UP << STM_TIM234_CR1_DIR) |
			(0 << STM_TIM234_CR1_OPM) |
			(1 << STM_TIM234_CR1_URS) |
			(0 << STM_TIM234_CR1_UDIS) |
			(1 << STM_TIM234_CR1_CEN));

	/* Configure pins */

	/* PB4 is Timer 3 CH1 output */
	stm_ospeedr_set(&stm_gpiob, 4, STM_OSPEEDR_40MHz);
	stm_afr_set(&stm_gpiob, 4, STM_AFR_AF2);


	/* Enable the scanline interrupt */
	stm_nvic_set_priority(STM_ISR_TIM2_POS, 0);
	stm_nvic_set_enable(STM_ISR_TIM2_POS);
}

void
ao_vga_enable(int enable)
{
	if (enable) {
		vblank_off = enable;
		ao_vga_fb_init();
		stm_tim2.cr1 |= (1 << STM_TIM234_CR1_CEN);
		stm_systick.csr &= ~(1 << STM_SYSTICK_CSR_ENABLE);
	} else {
		stm_tim2.cr1 &= ~(1 << STM_TIM234_CR1_CEN);
		stm_systick.csr |= (1 << STM_SYSTICK_CSR_ENABLE);
	}
}
