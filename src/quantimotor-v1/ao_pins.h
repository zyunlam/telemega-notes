/*
 * Copyright © 2024 Bdale Garbee <bdale@gag.com
 * GPLv3
 */

#ifndef _AO_PINS_H_
#define _AO_PINS_H_

#define AO_STACK_SIZE	320

#define IS_FLASH_LOADER	0

/* Crystal on the board */
#define AO_LPC_CLKIN	12000000

/* Main clock frequency. 48MHz for USB so we don't use the USB PLL */
#define AO_LPC_CLKOUT	48000000

/* System clock frequency */
#define AO_LPC_SYSCLK	24000000

#define HAS_EEPROM		0
#define USE_INTERNAL_FLASH	0
#define USE_STORAGE_CONFIG	0
#define USE_EEPROM_CONFIG	0

#define HAS_USB			1
#define HAS_USB_CONNECT		0
#define HAS_USB_VBUS		0
#define HAS_USB_PULLUP		1
#define AO_USB_PULLUP_PORT	0
#define AO_USB_PULLUP_PIN	20 

/* USART */

#define HAS_SERIAL_0            1
#define USE_SERIAL_0_STDIN      0
#define SERIAL_0_18_19          1

#define HAS_BEEP		0
#define HAS_RADIO		0
#define HAS_TELEMETRY		0
#define HAS_RSSI		0

#define HAS_SPI_0		0
#define SPI_SCK0_P0_6		0

#define PACKET_HAS_SLAVE	0
#define PACKET_HAS_MASTER	0

#define LOW_LEVEL_DEBUG		0

#define HAS_GPS			0
#define HAS_FLIGHT		0
#define HAS_LOG			0

#define AO_DATA_RING		16

/* SOM sets "health" high when system is ready */
#define HEALTH_PORT		1
#define HEALTH_PIN		19

/* LED */

#define LED_PORT                0
#define LED_PIN_HEALTH          8
#define AO_LED_HEALTH           (1 << LED_PIN_HEALTH)
#define LEDS_AVAILABLE          (AO_LED_HEALTH)
#define AO_LED_PANIC		AO_LED_HEALTH

/*
 * ADC
 */

#define HAS_ADC                 1

#define AO_NUM_ADC              7

#define AO_ADC_0                1
#define AO_ADC_1                1
#define AO_ADC_2                1
#define AO_ADC_3                1
#define AO_ADC_4                0
#define AO_ADC_5                1
#define AO_ADC_6                1
#define AO_ADC_7                1

struct ao_adc {
        int16_t         v_batt;
        int16_t         v_pyro;
        int16_t         sense_1;
        int16_t         adc3;
        int16_t         adc5;
        int16_t         adc6;
        int16_t         adc7;
};

#endif /* _AO_PINS_H_ */
