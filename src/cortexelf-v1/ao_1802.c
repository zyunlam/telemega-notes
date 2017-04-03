/*
 * Copyright © 2017 Keith Packard <keithp@keithp.com>
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
#include <ao_flip_bits.h>
#include <ao_1802.h>
#include <ao_exti.h>

/* Signals muxed between 1802 and STM */
uint8_t		MRD, TPB, TPA, MWR;

/* Decoded address driven by TPA/TPB signals */
uint16_t	ADDRESS;

/* Decoded data, driven by TPB signal */
uint8_t		DATA;

/* Mux control */
#define MUX_1802	0
#define MUX_STM		1

uint8_t		MUX_CONTROL;

/* Signals driven by 1802 only */
uint8_t		WAIT, CLEAR, Q, SC, N;
uint8_t		DMA_IN, DMA_OUT, INTERRUPT;
uint8_t		EF;

static uint8_t	ma_stm;

uint8_t
MA(void) {
	if (MUX_CONTROL == MUX_1802)
		return (ao_gpio_get_all(MA_PORT) >> MA_SHIFT) & 0xff;
	else
		return ma_stm;
}

static void
MA_set(uint8_t ma) {
	ao_gpio_set_mask(MA_PORT, ((uint16_t) ma) << MA_SHIFT, 0xff << MA_SHIFT);
	ma_stm = ma;
}

static uint8_t	data_stm;

static uint8_t
DATA_get(void) {
	if (MUX_CONTROL == MUX_1802)
		return ao_flip_bits[(ao_gpio_get_all(DATA_PORT) >> DATA_SHIFT) & 0xff];
	else
		return data_stm;
}

static void
DATA_set(uint8_t data) {
	ao_gpio_set_mask(DATA_PORT, ((uint16_t) ao_flip_bits[data]) << DATA_SHIFT, 0xff << DATA_SHIFT);
	data_stm = data;
}

static uint8_t
N_get(void) {
	return (ao_gpio_get_all(N_PORT) >> N_SHIFT) & 0x7;
}

static uint8_t
EF_get(void) {
	return (ao_gpio_get_all(EF_PORT) >> EF_SHIFT) & 0xf;
}

static uint8_t
Q_get(void) {
	return ao_gpio_get(Q_PORT, Q_BIT, Q_PIN);
}

static uint8_t
SC_get(void) {
	static const uint8_t flip_sc[4] = { 0, 2, 1, 3 };
	return flip_sc[(ao_gpio_get_all(SC_PORT) >> SC_SHIFT) & 3];
}

void
mrd(uint8_t value) { MRD = value; }

void
mwr(uint8_t value) { MWR = value; }

void
tpb(uint8_t value) {
	TPB = value;

	/* Latch low address and data on rising edge of TPB */
	if (TPB) {
		ADDRESS = (ADDRESS & 0xff00) | MA();
		DATA = DATA_get();
		N = N_get();
		ao_wakeup(&ADDRESS);
	}
}

void
tpa(uint8_t value) {
	TPA = value;

	/* Latch high address on rising edge of TPA */
	if (TPA) {
		ADDRESS = (ADDRESS & 0x00ff) | ((uint16_t) MA() << 8);
		SC = SC_get();
		if (SC == SC_EXECUTE)
			EF = EF_get();
		ao_wakeup(&ADDRESS);
	}
}

#define ao_1802_in(port, bit, callback) do {				\
		ao_enable_input(port, bit, 0);				\
		ao_exti_enable(port, bit);				\
		ao_exti_set_callback(port, bit, callback);		\
		(*callback)();						\
	} while (0)

static void mrd_isr(void) { mrd(ao_gpio_get(MRD_PORT, MRD_BIT, MRD_PIN)); }
static void mwr_isr(void) { mwr(ao_gpio_get(MWR_PORT, MWR_BIT, MWR_PIN)); }
static void tpb_isr(void) { tpb(ao_gpio_get(TPB_PORT, TPB_BIT, TPB_PIN)); }
static void tpa_isr(void) { tpa(ao_gpio_get(TPA_PORT, TPA_BIT, TPA_PIN)); }
static void q_isr(void) { Q = Q_get(); }

static void
ao_set_1802(void)
{
	ao_gpio_set(MUX_PORT, MUX_BIT, MUX_PIN, 0);
	ao_1802_in(MRD_PORT, MRD_BIT, mrd_isr);
	ao_1802_in(MWR_PORT, MWR_BIT, mwr_isr);
	ao_1802_in(TPB_PORT, TPB_BIT, tpb_isr);
	ao_1802_in(TPA_PORT, TPA_BIT, tpa_isr);
	MUX_CONTROL = MUX_1802;
}

static void
ao_set_arm(void)
{
	ao_enable_output(MRD_PORT, MRD_BIT, MRD_PIN, 1);
	ao_enable_output(MWR_PORT, MWR_BIT, MWR_PIN, 1);
	ao_enable_output(TPB_PORT, TPB_BIT, TPB_PIN, 0);
	ao_enable_output(TPA_PORT, TPA_BIT, TPA_PIN, 0);
	ao_gpio_set(MUX_PORT, MUX_BIT, MUX_PIN, 1);
	MUX_CONTROL = MUX_STM;
}

void
ao_1802_control_init(void)
{
	ao_set_1802();

	ao_1802_in(Q_PORT, Q_BIT, q_isr);
	(void) MA_set;
	(void) DATA_set;
	(void) ao_set_arm;
}
