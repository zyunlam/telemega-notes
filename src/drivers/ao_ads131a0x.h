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

#ifndef _AO_ADS131A0X_H_
#define _AO_ADS131A0X_H_

/* system commands */
#define AO_ADS131A0X_NULL		0x0000
#define AO_ADS131A0X_RESET		0x0011
#define AO_ADS131A0X_STANDBY		0x0022
#define AO_ADS131A0X_WAKEUP		0x0033
#define AO_ADS131A0X_LOCK		0x0555
#define AO_ADS131A0X_UNLOCK		0x0655

/* register write and read commands */
#define AO_ADS131A0X_RREG		0x2000
#define AO_ADS131A0X_RREGS		0x2000
#define AO_ADS131A0X_WREG		0x4000
#define AO_ADS131A0X_WREGS		0x6000

/* configuration register map */

/* read-only ID registers */
#define AO_ADS131A0X_ID_MSB		0x00
#define AO_ADS131A0X_ID_ADS124A02               0x02
#define AO_ADS131A0X_ID_ADS131A04               0x04
#define AO_ADS131A0X_ID_LSB		0x01

/* status registers */
#define AO_ADS131A0X_STAT_1		0x02
#define AO_ADS131A0X_STAT_P		0x03
#define AO_ADS131A0X_STAT_N		0x04
#define AO_ADS131A0X_STAT_S		0x05
#define AO_ADS131A0X_ERROR_CNT		0x06
#define AO_ADS131A0X_STAT_M2		0x07

/* user configuration registers */
#define AO_ADS131A0X_A_SYS_CFG		0x0b
#define AO_ADS131A0X_D_SYS_CFG		0x0c
#define AO_ADS131A0X_CLK1		0x0d
#define AO_ADS131A0X_CLK2		0x0e
#define AO_ADS131A0X_ADC_ENA		0x0f
#define AO_ADS131A0X_ADC1		0x11
#define AO_ADS131A0X_ADC2		0x12
#define AO_ADS131A0X_ADC3		0x13
#define AO_ADS131A0X_ADC4		0x14

struct ao_ads131a0x_sample {
	int32_t	ain[AO_ADS131A0X_CHANNELS];
};

extern struct ao_ads131a0x_sample	ao_ads131a0x_current;

void
ao_ads131a0x_init(void);

#endif /* _AO_ADS131A0X_H_ */
