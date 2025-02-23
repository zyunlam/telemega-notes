/*
 * Copyright © 2010 Keith Packard <keithp@keithp.com>
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

#include "ao.h"

/* Total bytes of available storage */
uint32_t	ao_storage_total;

/* Block size - device is erased in these units. At least 256 bytes */
uint32_t	ao_storage_block;

/* Byte offset of config block. Will be ao_storage_block bytes long */
uint32_t	ao_storage_config;

/* Storage unit size - device reads and writes must be within blocks of this size. Usually 256 bytes. */
uint16_t	ao_storage_unit;

#define M25_DEBUG	0
/*
 * Each flash chip is arranged in 64kB sectors; the
 * chip cannot erase in units smaller than that.
 *
 * Writing happens in units of 256 byte pages and
 * can only change bits from 1 to 0. So, you can rewrite
 * the same contents, or append to an existing page easily enough
 */

#define M25_WREN	0x06	/* Write Enable */
#define M25_WRDI	0x04	/* Write Disable */
#define M25_RDID	0x9f	/* Read Identification */
#define M25_RDSR	0x05	/* Read Status Register */
#define M25_WRSR	0x01	/* Write Status Register */
#define M25_READ	0x03	/* Read Data Bytes */
#define M25_FAST_READ	0x0b	/* Read Data Bytes at Higher Speed */
#define M25_PP		0x02	/* Page Program */
#define M25_SE		0xd8	/* Sector Erase */
#define M25_BE		0xc7	/* Bulk Erase */
#define M25_DP		0xb9	/* Deep Power-down */

/* RDID response */
#define M25_MANUF_OFFSET	0
#define M25_MEMORY_TYPE_OFFSET	1
#define M25_CAPACITY_OFFSET	2
#define M25_UID_OFFSET		3
#define M25_CFI_OFFSET		4
#define M25_RDID_LEN		4	/* that's all we need */

#define M25_CAPACITY_128KB	0x11
#define M25_CAPACITY_256KB	0x12
#define M25_CAPACITY_512KB	0x13
#define M25_CAPACITY_1MB	0x14
#define M25_CAPACITY_2MB	0x15

/*
 * Status register bits
 */

#define M25_STATUS_SRWD		(1 << 7)	/* Status register write disable */
#define M25_STATUS_BP_MASK	(7 << 2)	/* Block protect bits */
#define M25_STATUS_BP_SHIFT	(2)
#define M25_STATUS_WEL		(1 << 1)	/* Write enable latch */
#define M25_STATUS_WIP		(1 << 0)	/* Write in progress */

/*
 * On teleterra, the m25 chip select pins are
 * wired on P0_0 through P0_3.
 */

#if M25_MAX_CHIPS > 1
static uint32_t ao_m25_size[M25_MAX_CHIPS];	/* number of sectors in each chip */
static ao_port_t ao_m25_pin[M25_MAX_CHIPS];	/* chip select pin for each chip */
static uint8_t ao_m25_numchips;			/* number of chips detected */
#endif
static uint32_t ao_m25_total;			/* total sectors available */
static ao_port_t ao_m25_wip;			/* write in progress */

static uint8_t ao_m25_mutex;

/*
 * This little array is abused to send and receive data. A particular
 * caution -- the read and write addresses are written into the last
 * three bytes of the array by ao_m25_set_page_address and then the
 * first byte is used by ao_m25_wait_wip and ao_m25_write_enable, neither
 * of which touch those last three bytes.
 */

static uint8_t	ao_m25_instruction[4];

#define AO_M25_SPI_SPEED	ao_spi_speed(AO_M25_SPI_BUS, 10000000)	/* this seems like a reasonable minimum speed to require */

#define M25_SELECT(cs)		ao_spi_get_mask(AO_M25_SPI_CS_PORT,cs,AO_M25_SPI_BUS,AO_M25_SPI_SPEED)
#define M25_DESELECT(cs)	ao_spi_put_mask(AO_M25_SPI_CS_PORT,cs,AO_M25_SPI_BUS)

#define M25_BLOCK_SHIFT			16
#define M25_BLOCK			65536L
#define M25_POS_TO_SECTOR(pos)		((uint32_t) ((pos) >> M25_BLOCK_SHIFT))
#define M25_SECTOR_TO_POS(sector)	(((uint32_t) (sector)) << M25_BLOCK_SHIFT)

/*
 * Block until the specified chip is done writing
 */
static void
ao_m25_wait_wip(ao_port_t cs)
{
	if (ao_m25_wip & cs) {
		M25_SELECT(cs);
		ao_m25_instruction[0] = M25_RDSR;
		ao_spi_send(ao_m25_instruction, 1, AO_M25_SPI_BUS);
		do {
			ao_spi_recv(ao_m25_instruction, 1, AO_M25_SPI_BUS);
		} while (ao_m25_instruction[0] & M25_STATUS_WIP);
		M25_DESELECT(cs);
		ao_m25_wip &= (ao_port_t) ~cs;
	}
}

/*
 * Set the write enable latch so that page program and sector
 * erase commands will work. Also mark the chip as busy writing
 * so that future operations will block until the WIP bit goes off
 */
static void
ao_m25_write_enable(ao_port_t cs)
{
	M25_SELECT(cs);
	ao_m25_instruction[0] = M25_WREN;
	ao_spi_send(&ao_m25_instruction, 1, AO_M25_SPI_BUS);
	M25_DESELECT(cs);
	ao_m25_wip |= cs;
}


/*
 * Returns the number of 64kB sectors
 */
static uint32_t
ao_m25_read_capacity(ao_port_t cs)
{
	uint8_t	capacity;
	M25_SELECT(cs);
	ao_m25_instruction[0] = M25_RDID;
	ao_spi_send(ao_m25_instruction, 1, AO_M25_SPI_BUS);
	ao_spi_recv(ao_m25_instruction, M25_RDID_LEN, AO_M25_SPI_BUS);
	M25_DESELECT(cs);

	/* Check to see if the chip is present */
	if (ao_m25_instruction[0] == 0xff)
		return 0;
	capacity = ao_m25_instruction[M25_CAPACITY_OFFSET];

	/* Sanity check capacity number */
	if (capacity < 0x11 || 0x1f < capacity)
		return 0;
	return 1 << (capacity - 0x10);
}

static ao_port_t
ao_m25_set_address(uint32_t pos)
{
	ao_port_t	mask;
#if M25_MAX_CHIPS > 1
	uint32_t	size;
	uint8_t		chip;

	for (chip = 0; chip < ao_m25_numchips; chip++) {
		size = ao_m25_size[chip];
		if (M25_POS_TO_SECTOR(pos) < size)
			break;
		pos -= M25_SECTOR_TO_POS(size);
	}
	if (chip == ao_m25_numchips)
		return 0xff;

	mask = ao_m25_pin[chip];
#else
	mask = AO_M25_SPI_CS_MASK;
#endif
	ao_m25_wait_wip(mask);

	ao_m25_instruction[1] = (uint8_t) (pos >> 16);
	ao_m25_instruction[2] = (uint8_t) (pos >> 8);
	ao_m25_instruction[3] = (uint8_t) (pos);
	return mask;
}

/*
 * Scan the possible chip select lines
 * to see which flash chips are connected
 */
static uint8_t
ao_m25_scan(void)
{
#if M25_MAX_CHIPS > 1
	uint8_t	pin;
	uint32_t size;
#endif

	if (ao_m25_total)
		return 1;

#if M25_MAX_CHIPS > 1
	ao_m25_numchips = 0;
	for (pin = 1; pin != 0; pin <<= 1) {
		if (AO_M25_SPI_CS_MASK & pin) {
			size = ao_m25_read_capacity(pin);
			if (size != 0) {
				ao_m25_size[ao_m25_numchips] = size;
				ao_m25_pin[ao_m25_numchips] = pin;
				ao_m25_total += size;
				ao_m25_numchips++;
			}
		}
	}
#else
	ao_m25_total = ao_m25_read_capacity(AO_M25_SPI_CS_MASK);
#endif
	if (!ao_m25_total)
		return 0;
	ao_storage_total = M25_SECTOR_TO_POS(ao_m25_total);
	ao_storage_block = M25_BLOCK;
	ao_storage_config = ao_storage_total - M25_BLOCK;
	ao_storage_unit = 256;
	return 1;
}

/*
 * Erase the specified sector
 */
uint8_t
ao_storage_device_erase(uint32_t pos)
{
	ao_port_t	cs;

	if (pos >= ao_storage_total || pos + ao_storage_block > ao_storage_total)
		return 0;

	ao_mutex_get(&ao_m25_mutex);
	ao_m25_scan();

	cs = ao_m25_set_address(pos);

	ao_m25_write_enable(cs);

	ao_m25_instruction[0] = M25_SE;
	M25_SELECT(cs);
	ao_spi_send(ao_m25_instruction, 4, AO_M25_SPI_BUS);
	M25_DESELECT(cs);
	ao_m25_wip |= cs;

	ao_mutex_put(&ao_m25_mutex);
	return 1;
}

/*
 * Write to flash
 */
uint8_t
ao_storage_device_write(uint32_t pos, void *d, uint16_t len) 
{
	ao_port_t	cs;

	if (pos >= ao_storage_total || pos + len > ao_storage_total)
		return 0;

	ao_mutex_get(&ao_m25_mutex);
	ao_m25_scan();

	cs = ao_m25_set_address(pos);
	ao_m25_write_enable(cs);

	ao_m25_instruction[0] = M25_PP;
	M25_SELECT(cs);
	ao_spi_send(ao_m25_instruction, 4, AO_M25_SPI_BUS);
	ao_spi_send(d, len, AO_M25_SPI_BUS);
	M25_DESELECT(cs);

	ao_mutex_put(&ao_m25_mutex);
	return 1;
}

/*
 * Read from flash
 */
uint8_t
ao_storage_device_read(uint32_t pos, void *d, uint16_t len) 
{
	ao_port_t	cs;

	if (pos >= ao_storage_total || pos + len > ao_storage_total)
		return 0;
	ao_mutex_get(&ao_m25_mutex);
	ao_m25_scan();

	cs = ao_m25_set_address(pos);

	/* No need to use the FAST_READ as we're running at only 8MHz */
	ao_m25_instruction[0] = M25_READ;
	M25_SELECT(cs);
	ao_spi_send(ao_m25_instruction, 4, AO_M25_SPI_BUS);
	ao_spi_recv(d, len, AO_M25_SPI_BUS);
	M25_DESELECT(cs);

	ao_mutex_put(&ao_m25_mutex);
	return 1;
}

void
ao_storage_flush(void) 
{
}

void
ao_storage_setup(void)
{
	ao_mutex_get(&ao_m25_mutex);
	ao_m25_scan();
	ao_mutex_put(&ao_m25_mutex);
}

void
ao_storage_device_info(void) 
{
#if M25_DEBUG
	ao_port_t	cs;
#endif
#if M25_MAX_CHIPS > 1
	uint8_t chip;
#endif

	ao_mutex_get(&ao_m25_mutex);
	ao_m25_scan();
	ao_mutex_put(&ao_m25_mutex);

#if M25_MAX_CHIPS > 1
	printf ("Detected chips %d size %ld\n", ao_m25_numchips, ao_m25_total);
	for (chip = 0; chip < ao_m25_numchips; chip++)
		printf ("Flash chip %d select %02x size %d\n",
			chip, ao_m25_pin[chip], ao_m25_size[chip]);
#else
	printf ("Detected chips 1 size %ld\n", ao_m25_total);
#endif

#if M25_DEBUG
	printf ("Available chips:\n");
	for (cs = 1; cs != 0; cs <<= 1) {
		if ((AO_M25_SPI_CS_MASK & cs) == 0)
			continue;

		ao_mutex_get(&ao_m25_mutex);
		M25_SELECT(cs);
		ao_m25_instruction[0] = M25_RDID;
		ao_spi_send(ao_m25_instruction, 1, AO_M25_SPI_BUS);
		ao_spi_recv(ao_m25_instruction, M25_RDID_LEN, AO_M25_SPI_BUS);
		M25_DESELECT(cs);

		printf ("Select %02x manf %02x type %02x cap %02x uid %02x\n",
			cs,
			ao_m25_instruction[M25_MANUF_OFFSET],
			ao_m25_instruction[M25_MEMORY_TYPE_OFFSET],
			ao_m25_instruction[M25_CAPACITY_OFFSET],
			ao_m25_instruction[M25_UID_OFFSET]);
		ao_mutex_put(&ao_m25_mutex);
	}
#endif
}

void
ao_storage_device_init(void)
{
	ao_spi_init_cs (AO_M25_SPI_CS_PORT, AO_M25_SPI_CS_MASK);
}
