/*
 * Copyright © 2019 Keith Packard <keithp@keithp.com>
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
#include <ao_adc_samd21.h>

static void
ao_adc_sync(void)
{
	while (samd21_adc.status & (1 << SAMD21_ADC_STATUS_SYNCBUSY))
		;
}

static uint8_t ao_adc_sequence;
static uint8_t ao_adc_ready;

static uint8_t ao_adc_mux[AO_NUM_ADC] = {
#if AO_NUM_ADC > 0
	AO_ADC_SQ0,
#endif
#if AO_NUM_ADC > 1
	AO_ADC_SQ1,
#endif
#if AO_NUM_ADC > 2
	AO_ADC_SQ2,
#endif
#if AO_NUM_ADC > 3
	AO_ADC_SQ3,
#endif
#if AO_NUM_ADC > 4
	AO_ADC_SQ4,
#endif
#if AO_NUM_ADC > 5
	AO_ADC_SQ5,
#endif
#if AO_NUM_ADC > 6
	AO_ADC_SQ6,
#endif
#if AO_NUM_ADC > 7
	AO_ADC_SQ7,
#endif
#if AO_NUM_ADC > 8
	AO_ADC_SQ8,
#endif
#if AO_NUM_ADC > 9
#error set up more ADC
#endif
};

static void
ao_adc_start(void)
{
	uint8_t mux = ao_adc_mux[ao_adc_sequence];
	samd21_adc.inputctrl = ((mux << SAMD21_ADC_INPUTCTRL_MUXPOS) |
				(SAMD21_ADC_INPUTCTRL_MUXNEG_GND << SAMD21_ADC_INPUTCTRL_MUXNEG) |
				(0 << SAMD21_ADC_INPUTCTRL_INPUTSCAN) |
				(0 << SAMD21_ADC_INPUTCTRL_INPUTOFFSET) |
				(SAMD21_ADC_INPUTCTRL_GAIN_DIV2 << SAMD21_ADC_INPUTCTRL_GAIN));
	samd21_adc.swtrig = (1UL << SAMD21_ADC_SWTRIG_START);
}

void
samd21_adc_isr(void)
{
	uint16_t	*out;

	/* Store converted value in packet */
	out = (uint16_t *) &ao_data_ring[ao_data_head].adc;
	out[ao_adc_sequence] = (uint16_t) samd21_adc.result;
	if (++ao_adc_sequence < AO_NUM_ADC) {
		ao_adc_start();
		return;
	}

	AO_DATA_PRESENT(AO_DATA_ADC);
	ao_data_fill(ao_data_head);
	ao_adc_ready = 1;
}

void
ao_adc_poll(void)
{
	if (!ao_adc_ready)
		return;
	ao_adc_ready = 0;
	ao_adc_sequence = 0;
	ao_adc_start();
}

static void
ao_adc_dump(void)
{
	struct ao_data	packet;

	ao_data_get(&packet);
	AO_ADC_DUMP(&packet);
}

const struct ao_cmds ao_adc_cmds[] = {
	{ ao_adc_dump,	"a\0Display current ADC values" },
	{ 0, NULL },
};

static inline void
set_adc(struct samd21_port *port, uint8_t pin)
{
	samd21_port_pmux_set(port, pin, SAMD21_PORT_PMUX_FUNC_B);
	samd21_port_pincfg_set(port, pin,
			       (1 << SAMD21_PORT_PINCFG_DRVSTR) |
			       (1 << SAMD21_PORT_PINCFG_PULLEN) |
			       (1 << SAMD21_PORT_PINCFG_INEN),
			       (0 << SAMD21_PORT_PINCFG_DRVSTR) |
			       (0 << SAMD21_PORT_PINCFG_PULLEN) |
			       (1 << SAMD21_PORT_PINCFG_INEN));
}

void
ao_adc_init(void)
{
	/* supply a clock */
	samd21_gclk_clkctrl(0, SAMD21_GCLK_CLKCTRL_ID_ADC);

	/* enable the device */
	samd21_pm.apbcmask |= (1 << SAMD21_PM_APBCMASK_ADC);

	/* Reset */
	samd21_adc.ctrla = (1 << SAMD21_ADC_CTRLA_SWRST);

	ao_adc_sync();

	while ((samd21_adc.ctrla & (1 << SAMD21_ADC_CTRLA_SWRST)) != 0 ||
	       (samd21_adc.status & (1 << SAMD21_ADC_STATUS_SYNCBUSY)) != 0)
		ao_adc_sync();

	/* Load ADC calibration values */
	uint32_t b = (samd21_aux1.calibration >> SAMD21_AUX1_CALIBRATION_ADC_BIASCAL) & SAMD21_AUX1_CALIBRATION_ADC_BIASCAL_MASK;
	uint32_t l = (samd21_aux1.calibration >> SAMD21_AUX1_CALIBRATION_ADC_LINEARITY) & SAMD21_AUX1_CALIBRATION_ADC_LINEARITY_MASK;

	samd21_adc.calib = (uint16_t) ((b << SAMD21_ADC_CALIB_BIAS_CAL) |
				       (l << SAMD21_ADC_CALIB_LINEARITY_CAL));


	ao_adc_sync();

	samd21_adc.ctrlb = ((0 << SAMD21_ADC_CTRLB_DIFFMODE) |
			    (0 << SAMD21_ADC_CTRLB_LEFTADJ) |
			    (0 << SAMD21_ADC_CTRLB_FREERUN) |
			    (0 << SAMD21_ADC_CTRLB_CORREN) |
			    (SAMD21_ADC_CTRLB_RESSEL_12BIT << SAMD21_ADC_CTRLB_RESSEL) |
			    (SAMD21_ADC_CTRLB_PRESCALER_DIV512 << SAMD21_ADC_CTRLB_PRESCALER));

	ao_adc_sync();

	samd21_adc.sampctrl = 0x1f;

	ao_adc_sync();

	samd21_adc.refctrl = (SAMD21_ADC_REFCTRL_REFSEL_INTVCC1 << SAMD21_ADC_REFCTRL_REFSEL);

	ao_adc_sync();

	samd21_adc.intenset = (1UL << SAMD21_ADC_INTFLAG_RESRDY);

	samd21_adc.ctrla = (1 << SAMD21_ADC_CTRLA_ENABLE);

	/* configure interrupts */
	samd21_nvic_set_enable(SAMD21_NVIC_ISR_ADC_POS);
	samd21_nvic_set_priority(SAMD21_NVIC_ISR_ADC_POS, 0);

	ao_cmd_register(&ao_adc_cmds[0]);

	/* configure pins */
#if AO_NUM_ADC_PIN > 0
	set_adc(AO_ADC_PIN0_PORT, AO_ADC_PIN0_PIN);
#endif
#if AO_NUM_ADC_PIN > 1
	set_adc(AO_ADC_PIN1_PORT, AO_ADC_PIN1_PIN);
#endif
#if AO_NUM_ADC_PIN > 2
	set_adc(AO_ADC_PIN2_PORT, AO_ADC_PIN2_PIN);
#endif
#if AO_NUM_ADC_PIN > 3
	set_adc(AO_ADC_PIN3_PORT, AO_ADC_PIN3_PIN);
#endif
#if AO_NUM_ADC_PIN > 4
	set_adc(AO_ADC_PIN4_PORT, AO_ADC_PIN4_PIN);
#endif
#if AO_NUM_ADC_PIN > 5
	set_adc(AO_ADC_PIN5_PORT, AO_ADC_PIN5_PIN);
#endif
#if AO_NUM_ADC_PIN > 6
	set_adc(AO_ADC_PIN6_PORT, AO_ADC_PIN6_PIN);
#endif
#if AO_NUM_ADC_PIN > 7
	set_adc(AO_ADC_PIN7_PORT, AO_ADC_PIN7_PIN);
#endif
#if AO_NUM_ADC_PIN > 8
	set_adc(AO_ADC_PIN8_PORT, AO_ADC_PIN8_PIN);
#endif
#if AO_NUM_ADC_PIN > 9
#error set up more ADC bits
#endif

	ao_adc_ready = 1;
}
