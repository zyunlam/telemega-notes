/*
 * Copyright © 2011 Keith Packard <keithp@keithp.com>
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

#ifndef _AO_PINS_H_
#define _AO_PINS_H_

extern uint8_t ao_on_battery;

#define AO_SYSCLK	(ao_on_battery ? STM_HSI_FREQ : 48000000)

#define LED_PORT_ENABLE	STM_RCC_AHBENR_IOPAEN
#define LED_PORT	(&stm_gpioa)
#define LED_PIN_ORANGE	2
#define AO_LED_ORANGE	(1 << LED_PIN_ORANGE)
#define AO_LED_REPORT	AO_LED_ORANGE
#define AO_LED_PANIC	AO_LED_ORANGE

#define LEDS_AVAILABLE	(AO_LED_ORANGE)

#define AO_POWER_MANAGEMENT	0

/* 48MHz clock based on USB */
#define AO_HSI48	1
/* Need HSI running to flash */
#define AO_NEED_HSI	1

/* HCLK = 12MHz usb / 2MHz battery */
#define AO_AHB_PRESCALER	(ao_on_battery ? 16 : 1)
#define AO_RCC_CFGR_HPRE_DIV	(ao_on_battery ? STM_RCC_CFGR_HPRE_DIV_16 : STM_RCC_CFGR_HPRE_DIV_1)

/* APB = 12MHz usb / 2MHz battery */
#define AO_APB_PRESCALER	1
#define AO_RCC_CFGR_PPRE_DIV	STM_RCC_CFGR_PPRE_DIV_1

#define HAS_USB			1
#define AO_PA11_PA12_RMP	1

#define PACKET_HAS_SLAVE	0
#define HAS_SERIAL_1		0
#define HAS_SERIAL_2		1
#define USE_SERIAL_2_STDIN	0
#define USE_SERIAL_2_FLOW	0
#define USE_SERIAL_2_SW_FLOW	0
#define SERIAL_2_PA2_PA3	1
#define SERIAL_2_PA14_PA15	0
#define USE_SERIAL2_FLOW	0
#define USE_SERIAL2_SW_FLOW	0

#define IS_FLASH_LOADER		0

#define HAS_MS5607		1
#define HAS_MS5611		0
#define HAS_MS5607_TASK		0
#define HAS_EEPROM		0
#define HAS_BEEP		0

/* Logging */
#define LOG_INTERVAL		1
#define SAMPLE_SLEEP		AO_MS_TO_TICKS(100)
#define BOOST_DELAY		AO_SEC_TO_TICKS(60)
#define AO_LOG_ID		AO_LOG_ID_MICRO_PEAK2

/* Kalman filter */

#define AO_MK_STEP_100MS	1
#define AO_MK_STEP_96MS		0

/* SPI */
#define HAS_SPI_1		1
#define SPI_1_PA5_PA6_PA7	1
#define SPI_1_PB3_PB4_PB5	0
#define SPI_1_OSPEEDR		STM_OSPEEDR_MEDIUM

#define HAS_SPI_2		0

/* MS5607 */
#define HAS_MS5607		1
#define HAS_MS5611		0
#define AO_MS5607_PRIVATE_PINS	0
#define AO_MS5607_CS_PORT	(&stm_gpioa)
#define AO_MS5607_CS_PIN	4
#define AO_MS5607_CS_MASK	(1 << AO_MS5607_CS_PIN)
#define AO_MS5607_MISO_PORT	(&stm_gpioa)
#define AO_MS5607_MISO_PIN	6
#define AO_MS5607_MISO_MASK	(1 << AO_MS5607_MISO_PIN)
#define AO_MS5607_SPI_INDEX	AO_SPI_1_PA5_PA6_PA7

typedef int32_t alt_t;

#define AO_ALT_VALUE(x)		((x) * (alt_t) 10)

#define AO_DATA_RING		32

#define HAS_ADC			0

static inline void
ao_power_off(void) __attribute((noreturn));

static inline void
ao_power_off(void) {
	for (;;) {
	}
}

extern alt_t ao_max_height;

void ao_delay_until(uint16_t target);

#define ao_async_stop() do {					\
		ao_serial2_drain();				\
		stm_moder_set(&stm_gpioa, 2, STM_MODER_OUTPUT); \
	} while (0)

#define ao_async_start() do {						\
		stm_moder_set(&stm_gpioa, 2, STM_MODER_ALTERNATE);	\
		ao_delay(AO_MS_TO_TICKS(100));				\
	} while (0)

#define ao_async_byte(b) ao_serial2_putchar((char) (b))

#define ao_eeprom_read(pos, ptr, size) ao_storage_read(pos, ptr, size)
#define ao_eeprom_write(pos, ptr, size) ao_storage_write(pos, ptr, size)
#define MAX_LOG_OFFSET	ao_storage_total

extern uint32_t __flash__[];
extern uint32_t __flash_end__[];

#define AO_BOOT_APPLICATION_BOUND	((uint32_t *) __flash__)
#define USE_STORAGE_CONFIG	0

#endif /* _AO_PINS_H_ */
