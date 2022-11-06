/*
 * Copyright © 2022 Keith Packard <keithp@keithp.com>
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

#include <ao.h>
#include <ao_dma_samd21.h>

static uint8_t		ao_spi_mutex[SAMD21_NUM_SERCOM];
static uint8_t		ao_spi_pin_config[SAMD21_NUM_SERCOM];

struct ao_spi_samd21_info {
	struct samd21_sercom	*sercom;
};

static const struct ao_spi_samd21_info ao_spi_samd21_info[SAMD21_NUM_SERCOM] = {
	{
		.sercom = &samd21_sercom0,
	},
	{
		.sercom = &samd21_sercom1,
	},
	{
		.sercom = &samd21_sercom2,
	},
	{
		.sercom = &samd21_sercom3,
	},
	{
		.sercom = &samd21_sercom4,
	},
	{
		.sercom = &samd21_sercom5,
	},
};

static uint8_t	spi_dev_null;

#define USE_DMA	1

#if USE_DMA

#define AO_SAMD21_SPI_MISO_DMA_ID	0
#define AO_SAMD21_SPI_MOSI_DMA_ID	1

static uint8_t	ao_spi_done[SAMD21_NUM_SERCOM];

static void
_ao_spi_recv_dma_done(uint8_t dma_id, void *closure)
{
	uint8_t	id = (uint8_t) (uintptr_t) closure;

	(void) dma_id;
	ao_spi_done[id] = 1;
	ao_wakeup(&ao_spi_done[id]);
}
#endif

static inline uint32_t
dma_chctrlb(uint8_t id, bool tx)
{
	uint32_t	chctrlb = 0;

	/* No complicated actions needed */
	chctrlb |= SAMD21_DMAC_CHCTRLB_CMD_NOACT << SAMD21_DMAC_CHCTRLB_CMD;

	/* Trigger after each byte transferred */
	chctrlb |= SAMD21_DMAC_CHCTRLB_TRIGACT_BEAT << SAMD21_DMAC_CHCTRLB_TRIGACT;

	/* Set the trigger source */
	if (tx)
		chctrlb |= SAMD21_DMAC_CHCTRLB_TRIGSRC_SERCOM_TX(id) << SAMD21_DMAC_CHCTRLB_TRIGSRC;
	else
		chctrlb |= SAMD21_DMAC_CHCTRLB_TRIGSRC_SERCOM_RX(id) << SAMD21_DMAC_CHCTRLB_TRIGSRC;

	/* RX has priority over TX so that we don't drop incoming bytes */
	if (tx)
		chctrlb |= SAMD21_DMAC_CHCTRLB_LVL_LVL0 << SAMD21_DMAC_CHCTRLB_LVL;
	else
		chctrlb |= SAMD21_DMAC_CHCTRLB_LVL_LVL3 << SAMD21_DMAC_CHCTRLB_LVL;

	/* No events needed */
	chctrlb |= 0UL << SAMD21_DMAC_CHCTRLB_EVOE;
	chctrlb |= 0UL << SAMD21_DMAC_CHCTRLB_EVIE;

	/* And no actions either */
	chctrlb |= SAMD21_DMAC_CHCTRLB_EVACT_NOACT << SAMD21_DMAC_CHCTRLB_EVACT;

	return chctrlb;
}

static inline uint16_t
dma_btctrl(bool tx, bool step)
{
	uint16_t	btctrl = 0;

	/* Always step by 1 */
	btctrl |= SAMD21_DMAC_DESC_BTCTRL_STEPSIZE_X1 << SAMD21_DMAC_DESC_BTCTRL_STEPSIZE;

	/* Step the source if transmit, otherwise step the dest */
	if (tx)
		btctrl |= SAMD21_DMAC_DESC_BTCTRL_STEPSEL_SRC << SAMD21_DMAC_DESC_BTCTRL_STEPSEL;
	else
		btctrl |= SAMD21_DMAC_DESC_BTCTRL_STEPSEL_DST << SAMD21_DMAC_DESC_BTCTRL_STEPSEL;

	/* Set the increment if stepping */
	if (tx) {
		if (step)
			btctrl |= 1UL << SAMD21_DMAC_DESC_BTCTRL_SRCINC;
		else
			btctrl |= 0UL << SAMD21_DMAC_DESC_BTCTRL_SRCINC;
		btctrl |= 0UL << SAMD21_DMAC_DESC_BTCTRL_DSTINC;
	} else {
		btctrl |= 0UL << SAMD21_DMAC_DESC_BTCTRL_SRCINC;
		if (step)
			btctrl |= 1UL << SAMD21_DMAC_DESC_BTCTRL_DSTINC;
		else
			btctrl |= 0UL << SAMD21_DMAC_DESC_BTCTRL_DSTINC;
	}

	/* byte at a time please */
	btctrl |= SAMD21_DMAC_DESC_BTCTRL_BEATSIZE_BYTE << SAMD21_DMAC_DESC_BTCTRL_BEATSIZE;

	/*
	 * Watch for interrupts on RX -- we need to wait for the last byte to get received
	 * to know the SPI bus is idle
	 */
	if (tx)
		btctrl |= SAMD21_DMAC_DESC_BTCTRL_BLOCKACT_NOACT << SAMD21_DMAC_DESC_BTCTRL_BLOCKACT;
	else
		btctrl |= SAMD21_DMAC_DESC_BTCTRL_BLOCKACT_INT << SAMD21_DMAC_DESC_BTCTRL_BLOCKACT;

	/* don't need any events */
	btctrl |= SAMD21_DMAC_DESC_BTCTRL_EVOSEL_DISABLE << SAMD21_DMAC_DESC_BTCTRL_EVOSEL;

	/* And make the descriptor valid */
	btctrl |= 1UL << SAMD21_DMAC_DESC_BTCTRL_VALID;

	return btctrl;
}

void
ao_spi_send(const void *block, uint16_t len, uint8_t spi_index)
{
	uint8_t			id = AO_SPI_INDEX(spi_index);
	struct samd21_sercom	*sercom = ao_spi_samd21_info[id].sercom;
#if USE_DMA

	ao_arch_block_interrupts();
	ao_spi_done[id] = 0;

	_ao_dma_start_transfer(AO_SAMD21_SPI_MISO_DMA_ID,
			       (void *) &sercom->data,
			       &spi_dev_null,
			       len,
			       dma_chctrlb(id, false),
			       dma_btctrl(false, false),

			       _ao_spi_recv_dma_done,
			       (void *) (uintptr_t) id
		);

	_ao_dma_start_transfer(AO_SAMD21_SPI_MOSI_DMA_ID,
			       (uint8_t *) block + len,	/* must point past the end of the block */
			       (void *) &sercom->data,
			       len,
			       dma_chctrlb(id, true),
			       dma_btctrl(true, true),
			       NULL,
			       NULL
		);

	while (ao_spi_done[id] == 0)
		ao_sleep(&ao_spi_done[id]);

	_ao_dma_done_transfer(AO_SAMD21_SPI_MOSI_DMA_ID);
	_ao_dma_done_transfer(AO_SAMD21_SPI_MISO_DMA_ID);
	ao_arch_release_interrupts();
#else
	const uint8_t *b = block;

	while (len--) {
		sercom->data = *b++;
		while ((sercom->intflag & (1 << SAMD21_SERCOM_INTFLAG_RXC)) == 0)
			;
		(void) sercom->data;
	}
#endif
}


void
ao_spi_recv(void *block, uint16_t len, uint8_t spi_index)
{
	uint8_t			id = AO_SPI_INDEX(spi_index);
	struct samd21_sercom	*sercom = ao_spi_samd21_info[id].sercom;

	uint8_t *b = block;

	while (len--) {
		sercom->data = 0xff;
		while ((sercom->intflag & (1 << SAMD21_SERCOM_INTFLAG_RXC)) == 0)
			;
		*b++ = (uint8_t) sercom->data;
	}
}


void
ao_spi_duplex(const void *out, void *in, uint16_t len, uint8_t spi_index)
{
	uint8_t			id = AO_SPI_INDEX(spi_index);
	struct samd21_sercom	*sercom = ao_spi_samd21_info[id].sercom;

	const uint8_t *o = out;
	uint8_t *i = in;

	while (len--) {
		sercom->data = *o++;
		while ((sercom->intflag & (1 << SAMD21_SERCOM_INTFLAG_RXC)) == 0)
			;
		*i++ = (uint8_t) sercom->data;
	}
}

static void
ao_spi_disable_pin_config(uint8_t spi_pin_config)
{
	switch (spi_pin_config) {
#if HAS_SPI_0
	case AO_SPI_0_PA08_PA09_PA10:
		samd21_port_pmux_clr(&samd21_port_a, 8);	/* MOSI */
		samd21_port_pmux_clr(&samd21_port_a, 9);	/* SCLK */
		samd21_port_pmux_clr(&samd21_port_a, 10);	/* MISO */
		break;
	case AO_SPI_0_PA04_PA05_PA06:
		samd21_port_pmux_clr(&samd21_port_a, 4);	/* MOSI */
		samd21_port_pmux_clr(&samd21_port_a, 5);	/* SCLK */
		samd21_port_pmux_clr(&samd21_port_a, 6);	/* MISO */
		break;
#endif
	}
}

static void
ao_spi_enable_pin_config(uint8_t spi_pin_config)
{
	switch (spi_pin_config) {
#if HAS_SPI_0
	case AO_SPI_0_PA08_PA09_PA10:
		ao_enable_output(&samd21_port_a, 8, 1);
		ao_enable_output(&samd21_port_a, 9, 1);
		ao_enable_input(&samd21_port_a, 10, AO_MODE_PULL_NONE);
		samd21_port_pmux_set(&samd21_port_a, 8, SAMD21_PORT_PMUX_FUNC_C);	/* MOSI */
		samd21_port_pmux_set(&samd21_port_a, 9, SAMD21_PORT_PMUX_FUNC_C);	/* SCLK */
		samd21_port_pmux_set(&samd21_port_a, 10, SAMD21_PORT_PMUX_FUNC_C);	/* MISO */
		break;
	case AO_SPI_0_PA04_PA05_PA06:
		ao_enable_output(&samd21_port_a, 4, 1);
		ao_enable_output(&samd21_port_a, 5, 1);
		ao_enable_input(&samd21_port_a, 6, AO_MODE_PULL_NONE);
		samd21_port_pmux_set(&samd21_port_a, 4, SAMD21_PORT_PMUX_FUNC_C);	/* MOSI */
		samd21_port_pmux_set(&samd21_port_a, 5, SAMD21_PORT_PMUX_FUNC_C);	/* SCLK */
		samd21_port_pmux_set(&samd21_port_a, 6, SAMD21_PORT_PMUX_FUNC_C);	/* MISO */
		break;
#endif
	}
}

static void
ao_spi_config(uint8_t spi_index, uint32_t baud)
{
	uint8_t			spi_pin_config = AO_SPI_PIN_CONFIG(spi_index);
	uint8_t			id = AO_SPI_INDEX(spi_index);
	struct samd21_sercom	*sercom = ao_spi_samd21_info[id].sercom;

	if (spi_pin_config != ao_spi_pin_config[id]) {
		ao_spi_disable_pin_config(ao_spi_pin_config[id]);
		ao_spi_enable_pin_config(spi_pin_config);
		ao_spi_pin_config[id] = spi_pin_config;
	}

	sercom->baud = (uint16_t) baud;

	/* Set spi mode */
	uint32_t ctrla = sercom->ctrla;
	ctrla &= ~((1UL << SAMD21_SERCOM_CTRLA_CPOL) |
		   (1UL << SAMD21_SERCOM_CTRLA_CPHA));
	ctrla |= ((AO_SPI_CPOL(spi_index) << SAMD21_SERCOM_CTRLA_CPOL) |
		  (AO_SPI_CPHA(spi_index) << SAMD21_SERCOM_CTRLA_CPHA));

	/* finish setup and enable the hardware */
	ctrla |= (1 << SAMD21_SERCOM_CTRLA_ENABLE);

	sercom->ctrla = ctrla;

	while (sercom->syncbusy & (1 << SAMD21_SERCOM_SYNCBUSY_ENABLE))
		;
}

void
ao_spi_get(uint8_t spi_index, uint32_t speed)
{
	uint8_t		id = AO_SPI_INDEX(spi_index);

	ao_mutex_get(&ao_spi_mutex[id]);
	ao_spi_config(spi_index, speed);
}

void
ao_spi_put(uint8_t spi_index)
{
	uint8_t			id = AO_SPI_INDEX(spi_index);
	struct samd21_sercom 	*sercom = ao_spi_samd21_info[id].sercom;

	sercom->ctrla &= ~(1UL << SAMD21_SERCOM_CTRLA_ENABLE);
	while (sercom->syncbusy & (1 << SAMD21_SERCOM_SYNCBUSY_ENABLE))
		;
	ao_mutex_put(&ao_spi_mutex[id]);
}

static void
ao_spi_init_sercom(uint8_t id)
{
	struct samd21_sercom *sercom = ao_spi_samd21_info[id].sercom;

	/* Send a clock along */
	samd21_gclk_clkctrl(0, SAMD21_GCLK_CLKCTRL_ID_SERCOM0_CORE + id);

	samd21_nvic_set_enable(SAMD21_NVIC_ISR_SERCOM0_POS + id);
	samd21_nvic_set_priority(SAMD21_NVIC_ISR_SERCOM0_POS + id, 4);

	/* Enable */
	samd21_pm.apbcmask |= (1 << (SAMD21_PM_APBCMASK_SERCOM0 + id));

	/* Reset */
	sercom->ctrla = (1 << SAMD21_SERCOM_CTRLA_SWRST);

	while ((sercom->ctrla & (1 << SAMD21_SERCOM_CTRLA_SWRST)) ||
	       (sercom->syncbusy & (1 << SAMD21_SERCOM_SYNCBUSY_SWRST)))
		;

	/* set SPI mode */
	sercom->ctrla = ((SAMD21_SERCOM_CTRLA_DORD_MSB << SAMD21_SERCOM_CTRLA_DORD) |
			 (0 << SAMD21_SERCOM_CTRLA_CPOL) |
			 (0 << SAMD21_SERCOM_CTRLA_CPHA) |
			 (0 << SAMD21_SERCOM_CTRLA_FORM) |
			 (2 << SAMD21_SERCOM_CTRLA_DIPO) |
			 (0 << SAMD21_SERCOM_CTRLA_DOPO) |
			 (0 << SAMD21_SERCOM_CTRLA_IBON) |
			 (0 << SAMD21_SERCOM_CTRLA_RUNSTDBY) |
			 (SAMD21_SERCOM_CTRLA_MODE_SPI_HOST << SAMD21_SERCOM_CTRLA_MODE) |
			 (0 << SAMD21_SERCOM_CTRLA_ENABLE) |
			 (0 << SAMD21_SERCOM_CTRLA_SWRST));

	sercom->ctrlb = ((1 << SAMD21_SERCOM_CTRLB_RXEN) |
			 (0 << SAMD21_SERCOM_CTRLB_AMODE) |
			 (0 << SAMD21_SERCOM_CTRLB_MSSEN) |
			 (0 << SAMD21_SERCOM_CTRLB_SSDE) |
			 (0 << SAMD21_SERCOM_CTRLB_PLOADEN) |
			 (SAMD21_SERCOM_CTRLB_CHSIZE_8 << SAMD21_SERCOM_CTRLB_CHSIZE));


	ao_spi_enable_pin_config(id);
}

void
ao_spi_init(void)
{
#if HAS_SPI_0
	ao_spi_init_sercom(0);
#endif
#if HAS_SPI_1
	ao_spi_init_sercom(1);
#endif
#if HAS_SPI_2
	ao_spi_init_sercom(2);
#endif
#if HAS_SPI_3
	ao_spi_init_sercom(3);
#endif
#if HAS_SPI_4
	ao_spi_init_sercom(4);
#endif
#if HAS_SPI_5
	ao_spi_init_sercom(5);
#endif
}
