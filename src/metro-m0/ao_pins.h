/*
 * Copyright © 2022 Keith Packard <keithp@keithp.com>
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

#ifndef _AO_PINS_H_
#define _AO_PINS_H_

#define LED_0_PORT	(&samd21_port_a)
#define LED_0_PIN	2

#define LED_BLUE	(1 << 0)

#define AO_LED_PANIC	LED_BLUE

#define HAS_USB		1
#define USE_USB_STDIO	1

#define HAS_LED		1

#define AO_DFLL48M		48000000
#define AO_XOSC32K		32768

#define AO_AHB_PRESCALER	1
#define AO_APBA_PRESCALER	1

#define HAS_SPI_0		1
#define HAS_SPI_5		1

/*
 * SPI Flash memory
 */

#define M25_MAX_CHIPS		1

#if 1
#define AO_M25_SPI_CS_PORT	(&samd21_port_a)
#define AO_M25_SPI_CS_MASK	(1 << 13)
#define AO_M25_SPI_BUS		AO_SPI_5_PB22_PB23_PB03
#else

#define AO_M25_SPI_CS_PORT	(&samd21_port_a)
#define AO_M25_SPI_CS_MASK	(1 << 14) /* D2 */
#define AO_M25_SPI_BUS		AO_SPI_4_PB10_PB11_PA12

#endif

/*
 * Beeper
 */

#define HAS_BEEP		1

/* Beep on PA11 function F TCC0.3 */

#define AO_BEEP_TCC	(&samd21_tcc0)
#define AO_BEEP_TCC_APBC_MASK	SAMD21_PM_APBCMASK_TCC0
#define AO_BEEP_PORT	(&samd21_port_a)
#define AO_BEEP_PIN	(11)
#define AO_BEEP_FUNC	SAMD21_PORT_PMUX_FUNC_F

/* ADC */
#define AO_DATA_RING		32

#define HAS_ADC			1

struct ao_adc {
	int16_t			a[6];
	int16_t			temp;
};

#define AO_NUM_ADC_PIN	6
#define AO_NUM_ADC	(AO_NUM_ADC_PIN + 1)

#define AO_ADC_DUMP(p)							\
	printf("tick: %5lu a0: %5d a1: %5d a2: %5d a3: %5d a4: %5d a5: %5d temp: %5d\n", \
	       (p)->tick,						\
	       (p)->adc.a[0], (p)->adc.a[1], (p)->adc.a[2],		\
	       (p)->adc.a[3], (p)->adc.a[4], (p)->adc.a[5],		\
	       (p)->adc.temp);

#define AO_ADC_PIN0_PORT	(&samd21_port_a)
#define AO_ADC_PIN0_PIN		2
#define AO_ADC_SQ0		0

#define AO_ADC_PIN1_PORT	(&samd21_port_b)
#define AO_ADC_PIN1_PIN		8
#define AO_ADC_SQ1		2

#define AO_ADC_PIN2_PORT	(&samd21_port_b)
#define AO_ADC_PIN2_PIN		9
#define AO_ADC_SQ2		3

#define AO_ADC_PIN3_PORT	(&samd21_port_a)
#define AO_ADC_PIN3_PIN		4
#define AO_ADC_SQ3		4

#define AO_ADC_PIN4_PORT	(&samd21_port_a)
#define AO_ADC_PIN4_PIN		5
#define AO_ADC_SQ4		5

#define AO_ADC_PIN5_PORT	(&samd21_port_b)
#define AO_ADC_PIN5_PIN		2
#define AO_ADC_SQ5		10

#define AO_ADC_SQ6		SAMD21_ADC_INPUTCTRL_MUXPOS_TEMP

/* GPS */
#define HAS_GPS			1

#define AO_SERIAL_SPEED_UBLOX	AO_SERIAL_SPEED_9600

#define HAS_SERIAL_0		1
#define USE_SERIAL_0_STDIN	0
#define SERIAL_0_PA08_PA09	1

#define ao_gps_getchar		ao_serial0_getchar
#define ao_gps_putchar		ao_serial0_putchar
#define ao_gps_set_speed	ao_serial0_set_speed
#define ao_gps_fifo		(ao_samd21_usart0.rx_fifo)

#endif /* _AO_PINS_H_ */
