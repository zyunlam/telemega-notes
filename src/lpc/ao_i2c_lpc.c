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

#define AO_I2C_CLK	AO_LPC_SYSCLK

//#define LPC_I2C_DEBUG

/* Use 400kHz by default */
#ifndef I2C_FAST
#define I2C_FAST	1
#endif

#if I2C_FAST
#define I2C_TIME	1250	/* ns per phase, 400kHz clock */
#else
#define I2C_TIME	5000	/* ns per phase, 100kHz clock */
#endif

#define I2C_DUTY	((I2C_TIME * (AO_I2C_CLK / 1000)) / 1000000)

static uint8_t ao_i2c_mutex;

static uint8_t		i2c_addr;
static const uint8_t	*i2c_send;
static uint16_t		i2c_send_len;
static uint8_t		*i2c_recv;
static uint16_t		i2c_recv_len;
static uint8_t		i2c_stop;
static uint8_t		i2c_error;
static uint8_t		i2c_done;

#ifdef LPC_I2C_DEBUG
struct lpc_stat {
	const char 	*where;
	uint8_t		stat;
	uint8_t		i2c_con;
	uint8_t		i2c_addr;
	uint16_t	i2c_send_len;
	uint16_t	i2c_recv_len;
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
		stat_history[stat_count].i2c_con = (uint8_t) lpc_i2c.conset;
		stat_history[stat_count].i2c_addr = i2c_addr;
		stat_history[stat_count].i2c_send_len = i2c_send_len;
		stat_history[stat_count].i2c_recv_len = i2c_recv_len;
		stat_count++;
	}
	return stat;
}

static void
lpc_i2c_dump(void)
{
	int i;

	for (i = 0; i < stat_count; i++) {
		printf("0x%02x c(%02x) a(%02x) s(%d) r(%d) %s\n",
		       stat_history[i].stat,
		       stat_history[i].i2c_con,
		       stat_history[i].i2c_addr,
		       stat_history[i].i2c_send_len,
		       stat_history[i].i2c_recv_len,
		       stat_history[i].where);
	}
	stat_count = 0;
}
#else
#define lpc_i2c_stat(x) lpc_i2c.stat
#define lpc_i2c_dump()	do {} while(0)
#endif

static void
lpc_i2c_error(void)
{
	lpc_i2c.conclr = ((1 << LPC_I2C_CONCLR_STAC) |
			  (1 << LPC_I2C_CONCLR_SIC));
	lpc_i2c.conset = (1 << LPC_I2C_CONSET_STO);
	i2c_error = 1;
	ao_wakeup(&i2c_done);
}

static void
lpc_i2c_set_ack(void)
{
	/* if more than one byte to go, enable ack, else disable ack */
	if (i2c_recv_len > 1)
		lpc_i2c.conset = (1 << LPC_I2C_CONSET_AA);
	else
		lpc_i2c.conclr = (1 << LPC_I2C_CONCLR_AAC);
}

void
lpc_i2c_isr(void)
{
	switch (lpc_i2c_stat("isr")) {
	case LPC_I2C_STAT_ERROR:
		lpc_i2c_error();
		break;
	case LPC_I2C_STAT_START:
	case LPC_I2C_STAT_REPEAT_START:
		lpc_i2c.dat = i2c_addr;
		lpc_i2c.conclr = ((1 << LPC_I2C_CONCLR_STAC) |
				  (1 << LPC_I2C_CONCLR_SIC));
		break;
	case LPC_I2C_STAT_TX_START_ACK:
	case LPC_I2C_STAT_TX_ACK:
		if (i2c_send_len) {
			lpc_i2c_stat("dout");
			lpc_i2c.dat = *i2c_send++;
			i2c_send_len--;
			lpc_i2c.conclr = (1 << LPC_I2C_CONCLR_SIC);
		} else {
			lpc_i2c_stat("dend");
			if (i2c_stop) {
				lpc_i2c.conset = (1 << LPC_I2C_CONSET_STO);
				lpc_i2c.conclr = (1 << LPC_I2C_CONCLR_SIC);
			}
			i2c_done = 1;
			/*
			 * Need to disable interrupts as the IRQ will
			 * remain asserted until we clear it, and we
			 * can't clear it if we're going to restart as
			 * that will not generate a restart event
			 */
			lpc_nvic_clear_enable(LPC_ISR_I2C_POS);
			ao_wakeup(&i2c_done);
		}
		break;
	case LPC_I2C_STAT_RX_START_NACK:
	case LPC_I2C_STAT_TX_START_NACK:
	case LPC_I2C_STAT_TX_NACK:
	case LPC_I2C_STAT_TX_ARB_LOST:
		lpc_i2c_error();
		break;
	case LPC_I2C_STAT_RX_START_ACK:
		lpc_i2c_set_ack();
		lpc_i2c.conclr = (1 << LPC_I2C_CONCLR_SIC);
		break;
	case LPC_I2C_STAT_RX_NACK:
		/* fall through */
	case LPC_I2C_STAT_RX_ACK:
		if (i2c_recv_len) {
			*i2c_recv++ = (uint8_t) lpc_i2c.dat;
			i2c_recv_len--;
			lpc_i2c_set_ack();
			if (i2c_recv_len == 0) {
				if (i2c_stop) {
					lpc_i2c.conset = (1 << LPC_I2C_CONSET_STO);
					lpc_i2c.conclr = (1 << LPC_I2C_CONCLR_SIC);
				}
				i2c_done = 1;
				lpc_nvic_clear_enable(LPC_ISR_I2C_POS);
				ao_wakeup(&i2c_done);
			}
		}
		break;
	}
}

void
ao_i2c_get(uint8_t index)
{
	(void) index;
	ao_mutex_get(&ao_i2c_mutex);
	i2c_error = 0;
}

void
ao_i2c_put(uint8_t index)
{
	(void) index;
	ao_mutex_put(&ao_i2c_mutex);
}

uint8_t
ao_i2c_start(uint8_t index, uint16_t addr)
{
	(void) index;
	i2c_addr = (uint8_t) addr;
	return 0;
}

uint8_t
ao_i2c_send(const void *block, uint16_t len, uint8_t index, uint8_t stop)
{
	uint8_t	stopped = i2c_stop;

	(void) index;
	ao_arch_block_interrupts();
	(void) lpc_i2c_stat("send");

	i2c_done = 0;
	i2c_send = block;
	i2c_send_len = len;
	i2c_stop = stop;

	/* Clear read bit */
	i2c_addr &= 0xfe;

	lpc_nvic_set_enable(LPC_ISR_I2C_POS);

	/* Send start */
	lpc_i2c.conset = (1 << LPC_I2C_CONSET_STA);

	/* If we're restarting, clear the pending interrupt now */
	if (!stopped)
		lpc_i2c.conclr = (1 << LPC_I2C_CONCLR_SIC);

	while (!i2c_done && !i2c_error)
		ao_sleep(&i2c_done);

	ao_arch_release_interrupts();
	if (stop)
		lpc_i2c_dump();
	return i2c_error == 0;
}

uint8_t
ao_i2c_recv(void *block, uint16_t len, uint8_t index, uint8_t stop)
{
	uint8_t stopped = i2c_stop;
	(void) index;
	ao_arch_block_interrupts();
	(void) lpc_i2c_stat("recv");

	i2c_done = 0;
	i2c_recv = block;
	i2c_recv_len = len;
	i2c_stop = stop;

	/* Set read bit */
	i2c_addr |= 0x01;

	lpc_nvic_set_enable(LPC_ISR_I2C_POS);

	/* Send start */
	lpc_i2c.conset = (1 << LPC_I2C_CONSET_STA);

	/* If we're restarting, clear the pending interrupt now */
	if (!stopped)
		lpc_i2c.conclr = (1 << LPC_I2C_CONCLR_SIC);

	while (!i2c_done && !i2c_error)
		ao_sleep(&i2c_done);

	ao_arch_release_interrupts();
	lpc_i2c_dump();
	return i2c_error == 0;
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

	lpc_i2c.conclr = ((1 << LPC_I2C_CONCLR_I2ENC) |
			  (1 << LPC_I2C_CONCLR_STAC) |
			  (1 << LPC_I2C_CONCLR_STOC) |
			  (1 << LPC_I2C_CONCLR_SIC) |
			  (1 << LPC_I2C_CONCLR_AAC));

	lpc_i2c.conset = (1 << LPC_I2C_CONSET_I2EN);

	i2c_stop = 1;

	/* experimentally determined to be off-by-two? */
	lpc_i2c.sclh = I2C_DUTY - 2;
	lpc_i2c.scll = I2C_DUTY - 2;

	/* Enable interrupts */
	lpc_nvic_set_priority(LPC_ISR_I2C_POS, 0);
}
