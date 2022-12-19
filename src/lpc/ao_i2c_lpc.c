/*
 * Copyright © 2021 Keith Packard <keithp@keithp.com>
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

#include <ao.h>

static uint8_t ao_i2c_mutex;

static const uint8_t	*i2c_send;
static uint16_t		i2c_send_len;
static uint8_t		*i2c_recv;
static uint16_t		i2c_recv_len;
static uint8_t		i2c_stop;
static uint8_t		i2c_error;

static void
_ao_i2c_put_byte(void)
{
	lpc_i2c.dat = *i2c_send++;
	lpc_i2c.conset = (1 << LPC_I2C_CONSET_AA);
}

static void
_ao_i2c_get_byte(void)
{
	*i2c_recv++ = (uint8_t) lpc_i2c.dat;
	i2c_recv_len--;
	if (i2c_recv_len == 0) {
		lpc_i2c.conclr = (1 << LPC_I2C_CONCLR_AAC);
		ao_wakeup(&i2c_recv_len);
	} else {
		lpc_i2c.conset = (1 << LPC_I2C_CONSET_AA);
	}
}

struct lpc_stat {
	const char 	*where;
	uint8_t		stat;
};

#define NHISTORY	128
static struct lpc_stat	stat_history[NHISTORY];
static int		stat_count;

static uint8_t
lpc_i2c_stat(const char *where)
{
	uint8_t stat = (uint8_t) lpc_i2c.stat;
	if (stat_count < NHISTORY) {
		stat_history[stat_count].where = where;
		stat_history[stat_count].stat = stat;
		stat_count++;
	}
	return stat;
}

static void
lpc_i2c_dump(void)
{
	int i;

	for (i = 0; i < stat_count; i++) {
		printf("0x%02x %s\n", stat_history[i].stat, stat_history[i].where);
	}
	stat_count = 0;
}

void
lpc_i2c_isr(void)
{
	switch (lpc_i2c_stat("isr")) {
	case LPC_I2C_STAT_ERROR:
		lpc_i2c.conset = ((1 << LPC_I2C_CONSET_STO) |
				  (1 << LPC_I2C_CONSET_AA));
		break;
	case LPC_I2C_STAT_START:
	case LPC_I2C_STAT_REPEAT_START:
		i2c_error = 0;
		/* fall through ... */
	case LPC_I2C_STAT_TX_START_ACK:
	case LPC_I2C_STAT_TX_ACK:
		--i2c_send_len;
		if (i2c_send_len) {
			_ao_i2c_put_byte();
		} else {
			if (i2c_stop)
				lpc_i2c.conset =(1 << LPC_I2C_CONSET_STO);
			ao_wakeup(&i2c_send_len);
		}
		break;
	case LPC_I2C_STAT_TX_START_NACK:
	case LPC_I2C_STAT_TX_NACK:
		lpc_i2c.conset = ((1 << LPC_I2C_CONSET_AA) |
				  (1 << LPC_I2C_CONSET_STO));
		i2c_send_len = 0;
		i2c_error = 1;
		ao_wakeup(&i2c_send_len);
		break;
	case LPC_I2C_STAT_TX_ARB_LOST:
		lpc_i2c.conset =((1 << LPC_I2C_CONSET_AA)|
				 (1 << LPC_I2C_CONSET_STA));
		break;
	case LPC_I2C_STAT_RX_START_ACK:
		lpc_i2c.conset = (1 << LPC_I2C_CONSET_AA);
		break;
	case LPC_I2C_STAT_RX_START_NACK:
	case LPC_I2C_STAT_RX_NACK:
		lpc_i2c.conset = ((1 << LPC_I2C_CONSET_AA) |
				  (1 << LPC_I2C_CONSET_STO));
		i2c_recv_len = 0;
		i2c_error = 1;
		ao_wakeup(&i2c_recv_len);
		break;
	case LPC_I2C_STAT_RX_ACK:
		if (i2c_recv_len)
			_ao_i2c_get_byte();
		break;
	}
	lpc_i2c.conclr = (1 << LPC_I2C_CONCLR_SIC);
}

void
ao_i2c_get(uint8_t index)
{
	(void) index;
	ao_mutex_get(&ao_i2c_mutex);
	lpc_i2c.conset = (1 << LPC_I2C_CONSET_I2EN);
	lpc_i2c.sclh = 0xff;
	lpc_i2c.scll = 0xff;
}

void
ao_i2c_put(uint8_t index)
{
	(void) index;
	lpc_i2c.conclr = (1 << LPC_I2C_CONCLR_I2ENC);
	ao_mutex_put(&ao_i2c_mutex);
}

uint8_t
ao_i2c_start(uint8_t index, uint16_t addr)
{
	uint8_t	a = (uint8_t) addr;

	(void) index;
	ao_arch_block_interrupts();
	(void) lpc_i2c_stat("start");
	i2c_send = &a;
	i2c_send_len = 1;
	i2c_stop = 0;
	lpc_i2c.conset = (1 << LPC_I2C_CONSET_STA);
	while (i2c_send_len)
		ao_sleep(&i2c_send_len);
	ao_arch_release_interrupts();
	return 0;
}

uint8_t
ao_i2c_send(const void *block, uint16_t len, uint8_t index, uint8_t stop)
{
	(void) index;
	ao_arch_block_interrupts();
	(void) lpc_i2c_stat("send");
	i2c_send = block;
	i2c_send_len = len;
	i2c_stop = stop;
	_ao_i2c_put_byte();
	while (i2c_send_len) {
		if (ao_sleep_for(&i2c_send_len, AO_SEC_TO_TICKS(2)))
			break;
	}
	ao_arch_release_interrupts();
	lpc_i2c_dump();
	return 0;
}

uint8_t
ao_i2c_recv(void *block, uint16_t len, uint8_t index, uint8_t stop)
{
	(void) index;
	ao_arch_block_interrupts();
	i2c_recv = block;
	i2c_recv_len = len;
	i2c_stop = stop;
	/* Check to see if a byte is already here */
	if (lpc_i2c.stat == LPC_I2C_STAT_RX_ACK)
		_ao_i2c_get_byte();
	while (i2c_recv_len)
		ao_sleep(&i2c_recv_len);
	ao_arch_release_interrupts();
	return 0;
}

void
ao_i2c_init(void)
{
	/* Configure pins */
	lpc_ioconf.pio0_4 = ao_lpc_alternate(LPC_IOCONF_FUNC_I2C_SCL);
	lpc_ioconf.pio0_5 = ao_lpc_alternate(LPC_IOCONF_FUNC_I2C_SDA);

	/* Enable the device */
	lpc_scb.sysahbclkctrl |= (1 << LPC_SCB_SYSAHBCLKCTRL_I2C);

	/* Reset the device */
	lpc_scb.presetctrl &= ~(1UL << LPC_SCB_PRESETCTRL_I2C_RST_N);
	lpc_scb.presetctrl |= (1 << LPC_SCB_PRESETCTRL_I2C_RST_N);

	/* Enable interrupts */
	lpc_nvic_set_enable(LPC_ISR_I2C_POS);
	lpc_nvic_set_priority(LPC_ISR_I2C_POS, 0);
}
