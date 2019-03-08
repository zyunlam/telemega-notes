/*
 * Copyright © 2019 Bdale Garbee <bdale@gag.com>
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

#ifndef _AO_ADS1256_H_
#define _AO_ADS1256_H_

/* commands */
#define AO_ADS1256_WAKEUP		0x00
#define AO_ADS1256_RDATA		0x01
#define AO_ADS1256_RDATAC		0x03
#define AO_ADS1256_SDATAC		0x0f
#define AO_ADS1256_RREG			0x10
#define AO_ADS1256_WREG			0x50
#define AO_ADS1256_SELFCAL		0xf0
#define AO_ADS1256_SELFOCAL		0xf1
#define AO_ADS1256_SELFGCAL		0xf2
#define AO_ADS1256_SYSOCAL		0xf3
#define AO_ADS1256_SYSGCAL		0xf4
#define AO_ADS1256_SYNC			0xfc
#define AO_ADS1256_STANDBY		0xfd
#define AO_ADS1256_RESET		0xfe
#define AO_ADS1256_WAKEUP2		0xff

/* register map */
#define AO_ADS1256_STATUS	0x00
#define AO_ADS1256_ID_ADS1256		0x03
#define AO_ADS1256_MUX		0x01
#define AO_ADS1256_ADCON	0x02
#define AO_ADS1256_DRATE	0x03
#define AO_ADS1256_IO		0x04
#define AO_ADS1256_OFC0		0x05
#define AO_ADS1256_OFC1		0x06
#define AO_ADS1256_OFC2		0x07
#define AO_ADS1256_FSC0		0x08
#define AO_ADS1256_FSC1		0x09
#define AO_ADS1256_FSC2		0x0a

struct ao_ads1256_sample {
	int32_t	ain[AO_ADS1256_CHANNELS];
};

extern struct ao_ads1256_sample	ao_ads1256_current;

void
ao_ads1256_init(void);

#endif /* _AO_ADS1256_H_ */
