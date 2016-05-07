/*
 * Copyright © 2016 Keith Packard <keithp@keithp.com>
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

#include <ao.h>

struct ao_spi_stm_info {
	uint8_t	miso_dma_index;
	uint8_t mosi_dma_index;
	struct stm_spi *stm_spi;
};

static uint8_t		ao_spi_mutex[STM_NUM_SPI];
static uint8_t		ao_spi_index[STM_NUM_SPI];

static const struct ao_spi_stm_info ao_spi_stm_info[STM_NUM_SPI] = {
	{
		.miso_dma_index = STM_DMA_INDEX(STM_DMA_CHANNEL_SPI1_RX),
		.mosi_dma_index = STM_DMA_INDEX(STM_DMA_CHANNEL_SPI1_TX),
		&stm_spi1
	},
	{
		.miso_dma_index = STM_DMA_INDEX(STM_DMA_CHANNEL_SPI2_RX),
		.mosi_dma_index = STM_DMA_INDEX(STM_DMA_CHANNEL_SPI2_TX),
		&stm_spi2
	}
};

static uint8_t	spi_dev_null;



#define SPI_CR2	((0 << STM_SPI_CR2_LDMA_TX) |				\
		 (0 << STM_SPI_CR2_LDMA_RX) |				\
		 (1 << STM_SPI_CR2_FRXTH) |				\
		 (STM_SPI_CR2_DS_8 << STM_SPI_CR2_DS) |			\
		 (0 << STM_SPI_CR2_TXEIE) |				\
		 (0 << STM_SPI_CR2_RXNEIE) |				\
		 (0 << STM_SPI_CR2_ERRIE) |				\
		 (STM_SPI_CR2_FRF_MOTOROLA << STM_SPI_CR2_FRF) |	\
		 (0 << STM_SPI_CR2_NSSP) |				\
		 (0 << STM_SPI_CR2_SSOE))

#define SPI_CR2_DMA	(SPI_CR2 |			\
			 (1 << STM_SPI_CR2_TXDMAEN) |	\
			 (1 << STM_SPI_CR2_RXDMAEN))

#define SPI_CR2_SYNC	(SPI_CR2 |			\
			 (0 << STM_SPI_CR2_TXDMAEN) |	\
			 (0 << STM_SPI_CR2_RXDMAEN))

void
ao_spi_send(const void *block, uint16_t len, uint8_t spi_index)
{
	struct stm_spi *stm_spi = ao_spi_stm_info[AO_SPI_INDEX(spi_index)].stm_spi;
	uint8_t	mosi_dma_index = ao_spi_stm_info[AO_SPI_INDEX(spi_index)].mosi_dma_index;
	uint8_t	miso_dma_index = ao_spi_stm_info[AO_SPI_INDEX(spi_index)].miso_dma_index;

	/* Set up the transmit DMA to deliver data */
	ao_dma_set_transfer(mosi_dma_index,
			    &stm_spi->dr,
			    (void *) block,
			    len,
			    (0 << STM_DMA_CCR_MEM2MEM) |
			    (STM_DMA_CCR_PL_MEDIUM << STM_DMA_CCR_PL) |
			    (STM_DMA_CCR_MSIZE_8 << STM_DMA_CCR_MSIZE) |
			    (STM_DMA_CCR_PSIZE_8 << STM_DMA_CCR_PSIZE) |
			    (1 << STM_DMA_CCR_MINC) |
			    (0 << STM_DMA_CCR_PINC) |
			    (0 << STM_DMA_CCR_CIRC) |
			    (STM_DMA_CCR_DIR_MEM_TO_PER << STM_DMA_CCR_DIR) |
			    (0 << STM_DMA_CCR_TCIE));

	/* Clear RXNE */
	(void) stm_spi->dr;

	/* Set up the receive DMA -- when this is done, we know the SPI unit
	 * is idle. Without this, we'd have to poll waiting for the BSY bit to
	 * be cleared
	 */
	ao_dma_set_transfer(miso_dma_index,
			    &stm_spi->dr,
			    &spi_dev_null,
			    len,
			    (0 << STM_DMA_CCR_MEM2MEM) |
			    (STM_DMA_CCR_PL_MEDIUM << STM_DMA_CCR_PL) |
			    (STM_DMA_CCR_MSIZE_8 << STM_DMA_CCR_MSIZE) |
			    (STM_DMA_CCR_PSIZE_8 << STM_DMA_CCR_PSIZE) |
			    (0 << STM_DMA_CCR_MINC) |
			    (0 << STM_DMA_CCR_PINC) |
			    (0 << STM_DMA_CCR_CIRC) |
			    (STM_DMA_CCR_DIR_PER_TO_MEM << STM_DMA_CCR_DIR) |
			    (1 << STM_DMA_CCR_TCIE));

	stm_spi->cr2 = SPI_CR2_DMA;

	ao_dma_start(miso_dma_index);
	ao_dma_start(mosi_dma_index);
	ao_arch_critical(
		while (!ao_dma_done[miso_dma_index])
			ao_sleep(&ao_dma_done[miso_dma_index]);
		);
	ao_dma_done_transfer(mosi_dma_index);
	ao_dma_done_transfer(miso_dma_index);
}

void
ao_spi_send_fixed(uint8_t value, uint16_t len, uint8_t spi_index)
{
	struct stm_spi *stm_spi = ao_spi_stm_info[AO_SPI_INDEX(spi_index)].stm_spi;
	uint8_t	mosi_dma_index = ao_spi_stm_info[AO_SPI_INDEX(spi_index)].mosi_dma_index;
	uint8_t	miso_dma_index = ao_spi_stm_info[AO_SPI_INDEX(spi_index)].miso_dma_index;

	/* Set up the transmit DMA to deliver data */
	ao_dma_set_transfer(mosi_dma_index,
			    &stm_spi->dr,
			    &value,
			    len,
			    (0 << STM_DMA_CCR_MEM2MEM) |
			    (STM_DMA_CCR_PL_MEDIUM << STM_DMA_CCR_PL) |
			    (STM_DMA_CCR_MSIZE_8 << STM_DMA_CCR_MSIZE) |
			    (STM_DMA_CCR_PSIZE_8 << STM_DMA_CCR_PSIZE) |
			    (0 << STM_DMA_CCR_MINC) |
			    (0 << STM_DMA_CCR_PINC) |
			    (0 << STM_DMA_CCR_CIRC) |
			    (STM_DMA_CCR_DIR_MEM_TO_PER << STM_DMA_CCR_DIR) |
			    (0 << STM_DMA_CCR_TCIE));

	/* Clear RXNE */
	(void) stm_spi->dr;

	/* Set up the receive DMA -- when this is done, we know the SPI unit
	 * is idle. Without this, we'd have to poll waiting for the BSY bit to
	 * be cleared
	 */
	ao_dma_set_transfer(miso_dma_index,
			    &stm_spi->dr,
			    &spi_dev_null,
			    len,
			    (0 << STM_DMA_CCR_MEM2MEM) |
			    (STM_DMA_CCR_PL_MEDIUM << STM_DMA_CCR_PL) |
			    (STM_DMA_CCR_MSIZE_8 << STM_DMA_CCR_MSIZE) |
			    (STM_DMA_CCR_PSIZE_8 << STM_DMA_CCR_PSIZE) |
			    (0 << STM_DMA_CCR_MINC) |
			    (0 << STM_DMA_CCR_PINC) |
			    (0 << STM_DMA_CCR_CIRC) |
			    (STM_DMA_CCR_DIR_PER_TO_MEM << STM_DMA_CCR_DIR) |
			    (1 << STM_DMA_CCR_TCIE));

	stm_spi->cr2 = SPI_CR2_DMA;
	ao_dma_start(miso_dma_index);
	ao_dma_start(mosi_dma_index);
	ao_arch_critical(
		while (!ao_dma_done[miso_dma_index])
			ao_sleep(&ao_dma_done[miso_dma_index]);
		);
	ao_dma_done_transfer(mosi_dma_index);
	ao_dma_done_transfer(miso_dma_index);
}

void
ao_spi_send_sync(void *block, uint16_t len, uint8_t spi_index)
{
	uint8_t		*b = block;
	struct stm_spi	*stm_spi = ao_spi_stm_info[AO_SPI_INDEX(spi_index)].stm_spi;

	stm_spi->cr2 = SPI_CR2_SYNC;

	/* Clear RXNE */
	(void) stm_spi->dr;

	while (len--) {
		while (!(stm_spi->sr & (1 << STM_SPI_SR_TXE)));
		stm_spi->dr = *b++;
	}
}

void
ao_spi_recv(void *block, uint16_t len, uint8_t spi_index)
{
	struct stm_spi *stm_spi = ao_spi_stm_info[AO_SPI_INDEX(spi_index)].stm_spi;
	uint8_t	mosi_dma_index = ao_spi_stm_info[AO_SPI_INDEX(spi_index)].mosi_dma_index;
	uint8_t	miso_dma_index = ao_spi_stm_info[AO_SPI_INDEX(spi_index)].miso_dma_index;

	spi_dev_null = 0xff;

	/* Set up transmit DMA to make the SPI hardware actually run */
	ao_dma_set_transfer(mosi_dma_index,
			    &stm_spi->dr,
			    &spi_dev_null,
			    len,
			    (0 << STM_DMA_CCR_MEM2MEM) |
			    (STM_DMA_CCR_PL_MEDIUM << STM_DMA_CCR_PL) |
			    (STM_DMA_CCR_MSIZE_8 << STM_DMA_CCR_MSIZE) |
			    (STM_DMA_CCR_PSIZE_8 << STM_DMA_CCR_PSIZE) |
			    (0 << STM_DMA_CCR_MINC) |
			    (0 << STM_DMA_CCR_PINC) |
			    (0 << STM_DMA_CCR_CIRC) |
			    (STM_DMA_CCR_DIR_MEM_TO_PER << STM_DMA_CCR_DIR) |
			    (0 << STM_DMA_CCR_TCIE));

	/* Clear RXNE */
	(void) stm_spi->dr;

	/* Set up the receive DMA to capture data */
	ao_dma_set_transfer(miso_dma_index,
			    &stm_spi->dr,
			    block,
			    len,
			    (0 << STM_DMA_CCR_MEM2MEM) |
			    (STM_DMA_CCR_PL_MEDIUM << STM_DMA_CCR_PL) |
			    (STM_DMA_CCR_MSIZE_8 << STM_DMA_CCR_MSIZE) |
			    (STM_DMA_CCR_PSIZE_8 << STM_DMA_CCR_PSIZE) |
			    (1 << STM_DMA_CCR_MINC) |
			    (0 << STM_DMA_CCR_PINC) |
			    (0 << STM_DMA_CCR_CIRC) |
			    (STM_DMA_CCR_DIR_PER_TO_MEM << STM_DMA_CCR_DIR) |
			    (1 << STM_DMA_CCR_TCIE));

	stm_spi->cr2 = SPI_CR2_DMA;
	ao_dma_start(miso_dma_index);
	ao_dma_start(mosi_dma_index);

	/* Wait until the SPI unit is done */
	ao_arch_critical(
		while (!ao_dma_done[miso_dma_index])
			ao_sleep(&ao_dma_done[miso_dma_index]);
		);

	ao_dma_done_transfer(mosi_dma_index);
	ao_dma_done_transfer(miso_dma_index);
}

void
ao_spi_duplex(void *out, void *in, uint16_t len, uint8_t spi_index)
{
	struct stm_spi *stm_spi = ao_spi_stm_info[AO_SPI_INDEX(spi_index)].stm_spi;
	uint8_t	mosi_dma_index = ao_spi_stm_info[AO_SPI_INDEX(spi_index)].mosi_dma_index;
	uint8_t	miso_dma_index = ao_spi_stm_info[AO_SPI_INDEX(spi_index)].miso_dma_index;

	/* Set up transmit DMA to send data */
	ao_dma_set_transfer(mosi_dma_index,
			    &stm_spi->dr,
			    out,
			    len,
			    (0 << STM_DMA_CCR_MEM2MEM) |
			    (STM_DMA_CCR_PL_MEDIUM << STM_DMA_CCR_PL) |
			    (STM_DMA_CCR_MSIZE_8 << STM_DMA_CCR_MSIZE) |
			    (STM_DMA_CCR_PSIZE_8 << STM_DMA_CCR_PSIZE) |
			    (1 << STM_DMA_CCR_MINC) |
			    (0 << STM_DMA_CCR_PINC) |
			    (0 << STM_DMA_CCR_CIRC) |
			    (STM_DMA_CCR_DIR_MEM_TO_PER << STM_DMA_CCR_DIR) |
			    (0 << STM_DMA_CCR_TCIE));

	/* Clear RXNE */
	(void) stm_spi->dr;

	/* Set up the receive DMA to capture data */
	ao_dma_set_transfer(miso_dma_index,
			    &stm_spi->dr,
			    in,
			    len,
			    (0 << STM_DMA_CCR_MEM2MEM) |
			    (STM_DMA_CCR_PL_MEDIUM << STM_DMA_CCR_PL) |
			    (STM_DMA_CCR_MSIZE_8 << STM_DMA_CCR_MSIZE) |
			    (STM_DMA_CCR_PSIZE_8 << STM_DMA_CCR_PSIZE) |
			    (1 << STM_DMA_CCR_MINC) |
			    (0 << STM_DMA_CCR_PINC) |
			    (0 << STM_DMA_CCR_CIRC) |
			    (STM_DMA_CCR_DIR_PER_TO_MEM << STM_DMA_CCR_DIR) |
			    (1 << STM_DMA_CCR_TCIE));

	stm_spi->cr2 = SPI_CR2_DMA;
	ao_dma_start(miso_dma_index);
	ao_dma_start(mosi_dma_index);

	/* Wait until the SPI unit is done */
	ao_arch_critical(
		while (!ao_dma_done[miso_dma_index])
			ao_sleep(&ao_dma_done[miso_dma_index]);
		);

	ao_dma_done_transfer(mosi_dma_index);
	ao_dma_done_transfer(miso_dma_index);
}

static void
ao_spi_disable_index(uint8_t spi_index)
{
	/* Disable current config
	 */
	switch (AO_SPI_INDEX(spi_index)) {
	case STM_SPI_INDEX(1):
		switch (spi_index) {
		case AO_SPI_1_PA5_PA6_PA7:
			stm_gpio_set(&stm_gpioa, 5, 1);
			stm_moder_set(&stm_gpioa, 5, STM_MODER_OUTPUT);
			stm_moder_set(&stm_gpioa, 6, STM_MODER_INPUT);
			stm_moder_set(&stm_gpioa, 7, STM_MODER_OUTPUT);
			break;
		case AO_SPI_1_PB3_PB4_PB5:
			stm_gpio_set(&stm_gpiob, 3, 1);
			stm_moder_set(&stm_gpiob, 3, STM_MODER_OUTPUT);
			stm_moder_set(&stm_gpiob, 4, STM_MODER_INPUT);
			stm_moder_set(&stm_gpiob, 5, STM_MODER_OUTPUT);
			break;
		}
		break;
	case STM_SPI_INDEX(2):
		switch (spi_index) {
		case AO_SPI_2_PB13_PB14_PB15:
			stm_gpio_set(&stm_gpiob, 13, 1);
			stm_moder_set(&stm_gpiob, 13, STM_MODER_OUTPUT);
			stm_moder_set(&stm_gpiob, 14, STM_MODER_INPUT);
			stm_moder_set(&stm_gpiob, 15, STM_MODER_OUTPUT);
			break;
		}
		break;
	}
}

static void
ao_spi_enable_index(uint8_t spi_index)
{
	switch (AO_SPI_INDEX(spi_index)) {
	case STM_SPI_INDEX(1):
		switch (spi_index) {
		case AO_SPI_1_PA5_PA6_PA7:
			stm_afr_set(&stm_gpioa, 5, STM_AFR_AF0);
			stm_afr_set(&stm_gpioa, 6, STM_AFR_AF0);
			stm_afr_set(&stm_gpioa, 7, STM_AFR_AF0);
			break;
		case AO_SPI_1_PB3_PB4_PB5:
			stm_afr_set(&stm_gpiob, 3, STM_AFR_AF0);
			stm_afr_set(&stm_gpiob, 4, STM_AFR_AF0);
			stm_afr_set(&stm_gpiob, 5, STM_AFR_AF0);
			break;
		}
		break;
	case STM_SPI_INDEX(2):
		switch (spi_index) {
		case AO_SPI_2_PB13_PB14_PB15:
			stm_afr_set(&stm_gpiob, 13, STM_AFR_AF0);
			stm_afr_set(&stm_gpiob, 14, STM_AFR_AF0);
			stm_afr_set(&stm_gpiob, 15, STM_AFR_AF0);
			break;
		}
		break;
	}
}

static void
ao_spi_config(uint8_t spi_index, uint32_t speed)
{
	uint8_t		id = AO_SPI_INDEX(spi_index);
	struct stm_spi	*stm_spi = ao_spi_stm_info[id].stm_spi;

	stm_spi->cr2 = SPI_CR2;
	stm_spi->cr1 = ((0 << STM_SPI_CR1_BIDIMODE) |			/* Three wire mode */
			(0 << STM_SPI_CR1_BIDIOE) |
			(0 << STM_SPI_CR1_CRCEN) |			/* CRC disabled */
			(0 << STM_SPI_CR1_CRCNEXT) |
			(0 << STM_SPI_CR1_CRCL) |
			(0 << STM_SPI_CR1_RXONLY) |
			(1 << STM_SPI_CR1_SSM) |        		/* Software SS handling */
			(1 << STM_SPI_CR1_SSI) |			/*  ... */
			(0 << STM_SPI_CR1_LSBFIRST) |			/* Big endian */
			(1 << STM_SPI_CR1_SPE) |			/* Enable SPI unit */
			(speed << STM_SPI_CR1_BR) |			/* baud rate to pclk/4 */
			(1 << STM_SPI_CR1_MSTR) |
			(0 << STM_SPI_CR1_CPOL) |			/* Format 0 */
			(0 << STM_SPI_CR1_CPHA));

	if (spi_index != ao_spi_index[id]) {

		/* Disable old config
		 */
		ao_spi_disable_index(ao_spi_index[id]);

		/* Enable new config
		 */
		ao_spi_enable_index(spi_index);

		/* Remember current config
		 */
		ao_spi_index[id] = spi_index;
	}
}

uint8_t
ao_spi_try_get(uint8_t spi_index, uint32_t speed, uint8_t task_id)
{
	uint8_t		id = AO_SPI_INDEX(spi_index);

	if (!ao_mutex_try(&ao_spi_mutex[id], task_id))
		return 0;
	ao_spi_config(spi_index, speed);
	return 1;
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
	uint8_t		id = AO_SPI_INDEX(spi_index);
	struct stm_spi	*stm_spi = ao_spi_stm_info[id].stm_spi;

	stm_spi->cr1 = 0;
	ao_mutex_put(&ao_spi_mutex[id]);
}

static void
ao_spi_channel_init(uint8_t spi_index)
{
	uint8_t		id = AO_SPI_INDEX(spi_index);
	struct stm_spi	*stm_spi = ao_spi_stm_info[id].stm_spi;

	ao_spi_disable_index(spi_index);

	stm_spi->cr1 = 0;
	(void) stm_spi->sr;
	stm_spi->cr2 = SPI_CR2_SYNC;
}

void
ao_spi_init(void)
{
#if HAS_SPI_1
# if SPI_1_PA5_PA6_PA7
	stm_rcc.ahbenr |= (1 << STM_RCC_AHBENR_IOPAEN);
	stm_ospeedr_set(&stm_gpioa, 5, SPI_1_OSPEEDR);
	stm_ospeedr_set(&stm_gpioa, 6, SPI_1_OSPEEDR);
	stm_ospeedr_set(&stm_gpioa, 7, SPI_1_OSPEEDR);
# endif
# if SPI_1_PB3_PB4_PB5
	stm_rcc.ahbenr |= (1 << STM_RCC_AHBENR_IOPBEN);
	stm_ospeedr_set(&stm_gpiob, 3, SPI_1_OSPEEDR);
	stm_ospeedr_set(&stm_gpiob, 4, SPI_1_OSPEEDR);
	stm_ospeedr_set(&stm_gpiob, 5, SPI_1_OSPEEDR);
# endif
	stm_rcc.apb2enr |= (1 << STM_RCC_APB2ENR_SPI1EN);
	ao_spi_index[0] = AO_SPI_CONFIG_NONE;
	ao_spi_channel_init(STM_SPI_INDEX(1));
#endif

#if HAS_SPI_2
# if SPI_2_PB10_PB13_PB14
	stm_rcc.ahbenr |= (1 << STM_RCC_AHBENR_IOPBEN);
	stm_ospeedr_set(&stm_gpiob, 13, SPI_2_OSPEEDR);
	stm_ospeedr_set(&stm_gpiob, 14, SPI_2_OSPEEDR);
	stm_ospeedr_set(&stm_gpiob, 15, SPI_2_OSPEEDR);
# endif
	stm_rcc.apb1enr |= (1 << STM_RCC_APB1ENR_SPI2EN);
	ao_spi_index[1] = AO_SPI_CONFIG_NONE;
	ao_spi_channel_init(STM_SPI_INDEX(2));
#endif
}
