/*
 * Copyright © 2022 Bdale Garbee <bdale@gag.com>
 * GPLv3
 */

#ifndef _AO_PINS_H_
#define _AO_PINS_H_

#include <ao_flash_lpc_pins.h>

#define AO_BOOT_PIN		1
#define AO_BOOT_APPLICATION_GPIO	0
#define AO_BOOT_APPLICATION_PIN		7
#define AO_BOOT_APPLICATION_VALUE	1
#define AO_BOOT_APPLICATION_MODE	AO_EXTI_MODE_PULL_UP

#define HAS_USB_PULLUP	1
#define AO_USB_PULLUP_PORT	0
#define AO_USB_PULLUP_PIN	20

#endif /* _AO_PINS_H_ */
