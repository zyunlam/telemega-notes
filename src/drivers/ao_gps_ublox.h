/*
 * Copyright © 2013 Keith Packard <keithp@keithp.com>
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

#ifndef _AO_GPS_UBLOX_H_
#define _AO_GPS_UBLOX_H_

struct ublox_hdr {
	uint8_t		class, message;
	uint16_t	length;
};

#define UBLOX_ACK		0x05

#define UBLOX_ACK_NAK		0x00
#define UBLOX_ACK_ACK		0x01

#define UBLOX_NAV		0x01

#define UBLOX_NAV_DOP		0x04

struct ublox_nav_dop {
	uint8_t		class;		/* 0x01 */
	uint8_t		message;	/* 0x04 */
	uint16_t	length;		/* 18 */

	uint32_t	itow;		/* ms */
	uint16_t	gdop;
	uint16_t	ddop;
	uint16_t	tdop;
	uint16_t	vdop;
	uint16_t	hdop;
	uint16_t	ndop;
	uint16_t	edop;
};

#define UBLOX_NAV_POSLLH	0x02

struct ublox_nav_posllh {
	uint8_t		class;		/* 0x01 */
	uint8_t		message;	/* 0x02 */
	uint16_t	length;		/* 28 */

	uint32_t	itow;		/* ms */
	int32_t		lon;		/* deg * 1e7 */
	int32_t		lat;		/* deg * 1e7 */
	int32_t		height;		/* mm */
	int32_t		hmsl;		/* mm */
	uint32_t	hacc;		/* mm */
	uint32_t	vacc;		/* mm */
};

#define UBLOX_NAV_SOL		0x06

struct ublox_nav_sol {
	uint8_t		class;		/* 0x01 */
	uint8_t		message;	/* 0x06 */
	uint16_t	length;		/* 52 */

	uint32_t	itow;		/* ms */
	int32_t		ftow;		/* ns */
	int16_t		week;
	int8_t		gpsfix;
	uint8_t		flags;
	int32_t		exefx;		/* cm */
	int32_t		exefy;		/* cm */
	int32_t		exefz;		/* cm */
	uint32_t	pacc;		/* cm */
	int32_t		exefvx;		/* cm/s */
	int32_t		exefvy;		/* cm/s */
	int32_t		exefvz;		/* cm/s */
	uint32_t	sacc;		/* cm/s */
	uint16_t	pdop;		/* * 100 */
	uint8_t		reserved1;
	uint8_t		numsv;
	uint32_t	reserved2;
};

#define UBLOX_NAV_SOL_GPSFIX_NO_FIX		0
#define UBLOX_NAV_SOL_GPSFIX_DEAD_RECKONING	1
#define UBLOX_NAV_SOL_GPSFIX_2D			2
#define UBLOX_NAV_SOL_GPSFIX_3D			3
#define UBLOX_NAV_SOL_GPSFIX_GPS_DEAD_RECKONING	4
#define UBLOX_NAV_SOL_GPSFIX_TIME_ONLY		5

#define UBLOX_NAV_SOL_FLAGS_GPSFIXOK		0
#define UBLOX_NAV_SOL_FLAGS_DIFFSOLN		1
#define UBLOX_NAV_SOL_FLAGS_WKNSET		2
#define UBLOX_NAV_SOL_FLAGS_TOWSET		3

#define UBLOX_NAV_PVT		0x07

#define UBLOX_NAV_STATUS	0x03

struct ublox_nav_status {
	uint8_t		class;		/* 0x01 */
	uint8_t		message;	/* 0x03 */
	uint16_t	length;		/* 16 */

	uint8_t		gpsfix;
	uint8_t		flags;
	uint8_t		fixstat;
	uint8_t		flags2;

	uint32_t	ttff;		/* ms */
	uint32_t	msss;		/* ms */
};

#define UBLOX_NAV_STATUS_GPSFIX_NO_FIX			0
#define UBLOX_NAV_STATUS_GPSFIX_DEAD_RECKONING		1
#define UBLOX_NAV_STATUS_GPSFIX_2D			2
#define UBLOX_NAV_STATUS_GPSFIX_3D			3
#define UBLOX_NAV_STATUS_GPSFIX_GPS_DEAD_RECKONING	4
#define UBLOX_NAV_STATUS_GPSFIX_TIME_ONLY		5

#define UBLOX_NAV_STATUS_FLAGS_GPSFIXOK			0
#define UBLOX_NAV_STATUS_FLAGS_DIFFSOLN			1
#define UBLOX_NAV_STATUS_FLAGS_WKNSET			2
#define UBLOX_NAV_STATUS_FLAGS_TOWSET			3

#define UBLOX_NAV_STATUS_FIXSTAT_DGPSISTAT		0
#define UBLOX_NAV_STATUS_FIXSTAT_MAPMATCHING		6
#define UBLOX_NAV_STATUS_FIXSTAT_MAPMATCHING_NONE		0
#define UBLOX_NAV_STATUS_FIXSTAT_MAPMATCHING_VALID		1
#define UBLOX_NAV_STATUS_FIXSTAT_MAPMATCHING_USED		2
#define UBLOX_NAV_STATUS_FIXSTAT_MAPMATCHING_DR			3
#define UBLOX_NAV_STATUS_FIXSTAT_MAPMATCHING_MASK		3

#define UBLOX_NAV_STATUS_FLAGS2_PSMSTATE		0
#define UBLOX_NAV_STATUS_FLAGS2_PSMSTATE_ACQUISITION			0
#define UBLOX_NAV_STATUS_FLAGS2_PSMSTATE_TRACKING			1
#define UBLOX_NAV_STATUS_FLAGS2_PSMSTATE_POWER_OPTIMIZED_TRACKING	2
#define UBLOX_NAV_STATUS_FLAGS2_PSMSTATE_INACTIVE			3
#define UBLOX_NAV_STATUS_FLAGS2_PSMSTATE_MASK				3

#define UBLOX_NAV_SVINFO	0x30

struct ublox_nav_svinfo {
	uint8_t		class;		/* 0x01 */
	uint8_t		message;	/* 0x30 */
	uint16_t	length;		/* 8 + 12 * numch */

	uint32_t	itow;		/* ms */

	uint8_t		numch;
	uint8_t		globalflags;
	uint16_t	reserved;
};

#define UBLOX_NAV_SVINFO_GLOBAL_FLAGS_CHIPGEN	0
#define UBLOX_NAV_SVINFO_GLOBAL_FLAGS_CHIPGEN_ANTARIS	0
#define UBLOX_NAV_SVINFO_GLOBAL_FLAGS_CHIPGEN_U_BLOX_5	1
#define UBLOX_NAV_SVINFO_GLOBAL_FLAGS_CHIPGEN_U_BLOX_6	2
#define UBLOX_NAV_SVINFO_GLOBAL_FLAGS_CHIPGEN_MASK	7

struct ublox_nav_svinfo_block {
	uint8_t		chn;
	uint8_t		svid;
	uint8_t		flags;
	uint8_t		quality;

	uint8_t		cno;		/* dbHz */
	int8_t		elev;		/* deg */
	int16_t		azim;		/* deg */

	int32_t		prres;		/* cm */
};

#define UBLOX_NAV_SVINFO_BLOCK_FLAGS_SVUSED	0
#define UBLOX_NAV_SVINFO_BLOCK_FLAGS_DIFFCORR	1
#define UBLOX_NAV_SVINFO_BLOCK_FLAGS_ORBITAVAIL	2
#define UBLOX_NAV_SVINFO_BLOCK_FLAGS_ORBITEPH	3
#define UBLOX_NAV_SVINFO_BLOCK_FLAGS_UNHEALTHY	4
#define UBLOX_NAV_SVINFO_BLOCK_FLAGS_ORBITALM	5
#define UBLOX_NAV_SVINFO_BLOCK_FLAGS_ORBITAOP	6
#define UBLOX_NAV_SVINFO_BLOCK_FLAGS_SMOOTHED	7

#define UBLOX_NAV_SVINFO_BLOCK_QUALITY_QUALITYIND	0
#define UBLOX_NAV_SVINFO_BLOCK_QUALITY_QUALITYIND_IDLE			0
#define UBLOX_NAV_SVINFO_BLOCK_QUALITY_QUALITYIND_SEARCHING		1
#define UBLOX_NAV_SVINFO_BLOCK_QUALITY_QUALITYIND_ACQUIRED		2
#define UBLOX_NAV_SVINFO_BLOCK_QUALITY_QUALITYIND_UNUSABLE		3
#define UBLOX_NAV_SVINFO_BLOCK_QUALITY_QUALITYIND_CODE_LOCK		4
#define UBLOX_NAV_SVINFO_BLOCK_QUALITY_QUALITYIND_CARRIER_LOCKED_5	5
#define UBLOX_NAV_SVINFO_BLOCK_QUALITY_QUALITYIND_CARRIER_LOCKED_6	6
#define UBLOX_NAV_SVINFO_BLOCK_QUALITY_QUALITYIND_CARRIER_LOCKED_7	7
#define UBLOX_NAV_SVINFO_BLOCK_QUALITY_QUALITYIND_MASK			7

#define UBLOX_NAV_TIMEUTC	0x21

struct ublox_nav_timeutc {
	uint8_t		class;		/* 0x01 */
	uint8_t		message;	/* 0x21 */
	uint16_t	length;		/* 20 */

	uint32_t	itow;		/* ms */
	uint32_t	tacc;		/* ns */
	int32_t		nano;		/* ns */

	uint16_t	year;
	uint8_t		month;
	uint8_t		day;

	uint8_t		hour;
	uint8_t		min;
	uint8_t		sec;
	uint8_t		valid;
};

#define UBLOX_NAV_TIMEUTC_VALID_VALIDTOW	0
#define UBLOX_NAV_TIMEUTC_VALID_VALIDWKN	1
#define UBLOX_NAV_TIMEUTC_VALID_VALIDUTC	2

#define UBLOX_NAV_VELNED	0x12

struct ublox_nav_velned {
	uint8_t		class;		/* 0x01 */
	uint8_t		message;	/* 0x12 */
	uint16_t	length;		/* 36 */

	uint32_t	itow;		/* ms */

	int32_t		veln;		/* cm/s */
	int32_t		vele;		/* cm/s */
	int32_t		veld;		/* cm/s */

	uint32_t	speed;		/* cm/s */
	uint32_t	gspeed;		/* cm/s */

	int32_t		heading;	/* deg */
	uint32_t	sacc;		/* cm/s */
	uint32_t	cacc;		/* deg */
};

#define UBLOX_NAV_SAT	0x35

struct ublox_nav_sat {
	uint8_t		class;		/* 0x01 */
	uint8_t		message;	/* 0x35 */
	uint16_t	length;		/* 8 + 12 * numsvs */

	uint32_t	itow;		/* time of week */
	uint8_t		version;	/* Message version (0x01) */
	uint8_t		numSvs;		/* number of satellites */
	uint16_t	reserved;
};

struct ublox_nav_sat_sat {
	uint8_t		gnssId;		/* GNSS identifier */
	uint8_t		svId;		/* satellite identifier */
	uint8_t		cno;		/* carrier to noise ratio */
	int8_t		elev;		/* elevation */
	int16_t		azim;		/* azimuth */
	int16_t		prRes;		/* pseudorange residual */
	uint32_t	flags;
};

#define UBLOX_NAV_SAT_FLAGS_QUALITY	0
#define  UBLOX_NAV_SAT_FLAGS_QUALITY_NO_SIGNAL		0
#define  UBLOX_NAV_SAT_FLAGS_QUALITY_SEARCHING		1
#define  UBLOX_NAV_SAT_FLAGS_QUALITY_ACQUIRED		2
#define  UBLOX_NAV_SAT_FLAGS_QUALITY_UNUSABLE		3
#define  UBLOX_NAV_SAT_FLAGS_QUALITY_CODE_LOCKED	4
#define  UBLOX_NAV_SAT_FLAGS_QUALITY_CARRIER_LOCKED	5
#define  UBLOX_NAV_SAT_FLAGS_QUALITY_MASK		7
#define UBLOX_NAV_SAT_FLAGS_SV_USED	3
#define UBLOX_NAV_SAT_FLAGS_SV_HEALTH	4
#define  UBLOX_NAV_SAT_FLAGS_SV_HEALTH_UNKNOWN		0
#define  UBLOX_NAV_SAT_FLAGS_SV_HEALTH_HEALTHY		1
#define  UBLOX_NAV_SAT_FLAGS_SV_HEALTH_UNHEALTHY	2
#define  UBLOX_NAV_SAT_FLAGS_SV_HEALTH_MASK		3
#define UBLOX_NAV_SAT_FLAGS_DIFF_CORR	6
#define UBLOX_NAV_SAT_FLAGS_SMOOTHED	7
#define UBLOX_NAV_SAT_FLAGS_ORBIT_SOURCE	8
#define  UBLOX_NAV_SAT_FLAGS_ORBIT_SOURCE_NONE			0
#define  UBLOX_NAV_SAT_FLAGS_ORBIT_SOURCE_EPHEMERIS		1
#define  UBLOX_NAV_SAT_FLAGS_ORBIT_SOURCE_ALMANAC		2
#define  UBLOX_NAV_SAT_FLAGS_ORBIT_SOURCE_ASSIST_NOW_OFFLINE	3
#define  UBLOX_NAV_SAT_FLAGS_ORBIT_SOURCE_ASSIST_NOW_AUTONOMOUS	4
#define  UBLOX_NAV_SAT_FLAGS_ORBIT_SOURCE_MASK			7
#define UBLOX_NAV_SAT_FLAGS_EPH_AVAILABLE	11
#define UBLOX_NAV_SAT_FLAGS_ALM_AVAILABLE	12
#define UBLOX_NAV_SAT_FLAGS_ANO_AVAILABLE	13
#define UBLOX_NAV_SAT_FLAGS_AOP_AVAILABLE	14
#define UBLOX_NAV_SAT_FLAGS_SBAS_CORR_USED	16
#define UBLOX_NAV_SAT_FLAGS_RTCM_CORR_USED	17
#define UBLOX_NAV_SAT_FLAGS_SLAS_CORR_USED	18
#define UBLOX_NAV_SAT_FLAGS_SPARTN_CORR_USED	19
#define UBLOX_NAV_SAT_FLAGS_PR_CORR_USED	20
#define UBLOX_NAV_SAT_FLAGS_CR_CORR_USED	21
#define UBLOX_NAV_SAT_FLAGS_DO_CORR_USED	22
#define UBLOX_CFG	0x06

#define UBLOX_CFG_NAV5	0x24

#define UBLOX_CFG_NAV5_MASK_DYN			0
#define UBLOX_CFG_NAV5_MASK_MINE1		1
#define UBLOX_CFG_NAV5_MASK_FIXMODE		2
#define UBLOX_CFG_NAV5_MASK_DRLIM		3
#define UBLOX_CFG_NAV5_MASK_POSMASK		4
#define UBLOX_CFG_NAV5_MASK_TIMEMASK		5
#define UBLOX_CFG_NAV5_MASK_STATICHOLDMASK	6
#define UBLOX_CFG_NAV5_MASK_DGPSMASK		7

#define UBLOX_CFG_NAV5_DYNMODEL_PORTABLE	0
#define UBLOX_CFG_NAV5_DYNMODEL_STATIONARY	2
#define UBLOX_CFG_NAV5_DYNMODEL_PEDESTRIAN	3
#define UBLOX_CFG_NAV5_DYNMODEL_AUTOMOTIVE	4
#define UBLOX_CFG_NAV5_DYNMODEL_SEA		5
#define UBLOX_CFG_NAV5_DYNMODEL_AIRBORNE_1G	6
#define UBLOX_CFG_NAV5_DYNMODEL_AIRBORNE_2G	7
#define UBLOX_CFG_NAV5_DYNMODEL_AIRBORNE_4G	8

#define UBLOX_CFG_NAV5_FIXMODE_2D		1
#define UBLOX_CFG_NAV5_FIXMODE_3D		2
#define UBLOX_CFG_NAV5_FIXMODE_AUTO		3

#define UBLOX_CFG_MSG		0x01
#define UBLOX_CFG_VALSET	0x8a
#define  UBLOX_CFG_VALSET_VERSION	0x00
#define  UBLOX_CFG_VALSET_LAYER_RAM	0x01
#define  UBLOX_CFG_VALSET_LAYER_BBR	0x02
#define  UBLOX_CFG_VALSET_LAYER_FLASH	0x04

#define UBLOX_CFG_MSGOUT_NMEA_ID_DTM_I2C	0x209100a6 /* U1 -- Output rate of the NMEA-GX-DTM message on port I2C */
#define UBLOX_CFG_MSGOUT_NMEA_ID_DTM_SPI	0x209100aa /* U1 -- Output rate of the NMEA-GX-DTM message on port SPI */
#define UBLOX_CFG_MSGOUT_NMEA_ID_DTM_UART1	0x209100a7 /* U1 -- Output rate of the NMEA-GX-DTM message on port UART1 */
#define UBLOX_CFG_MSGOUT_NMEA_ID_GBS_I2C	0x209100dd /* U1 -- Output rate of the NMEA-GX-GBS message on port I2C */
#define UBLOX_CFG_MSGOUT_NMEA_ID_GBS_SPI	0x209100e1 /* U1 -- Output rate of the NMEA-GX-GBS message on port SPI */
#define UBLOX_CFG_MSGOUT_NMEA_ID_GBS_UART1	0x209100de /* U1 -- Output rate of the NMEA-GX-GBS message on port UART1 */
#define UBLOX_CFG_MSGOUT_NMEA_ID_GGA_I2C	0x209100ba /* U1 -- Output rate of the NMEA-GX-GGA message on port I2C */
#define UBLOX_CFG_MSGOUT_NMEA_ID_GGA_SPI	0x209100be /* U1 -- Output rate of the NMEA-GX-GGA message on port SPI */
#define UBLOX_CFG_MSGOUT_NMEA_ID_GGA_UART1	0x209100bb /* U1 -- Output rate of the NMEA-GX-GGA message on port UART1 */
#define UBLOX_CFG_MSGOUT_NMEA_ID_GLL_I2C	0x209100c9 /* U1 -- Output rate of the NMEA-GX-GLL message on port I2C */
#define UBLOX_CFG_MSGOUT_NMEA_ID_GLL_SPI	0x209100cd /* U1 -- Output rate of the NMEA-GX-GLL message on port SPI */
#define UBLOX_CFG_MSGOUT_NMEA_ID_GLL_UART1	0x209100ca /* U1 -- Output rate of the NMEA-GX-GLL message on port UART1 */
#define UBLOX_CFG_MSGOUT_NMEA_ID_GNS_I2C	0x209100b5 /* U1 -- Output rate of the NMEA-GX-GNS message on port I2C */
#define UBLOX_CFG_MSGOUT_NMEA_ID_GNS_SPI	0x209100b9 /* U1 -- Output rate of the NMEA-GX-GNS message on port SPI */
#define UBLOX_CFG_MSGOUT_NMEA_ID_GNS_UART1	0x209100b6 /* U1 -- Output rate of the NMEA-GX-GNS message on port UART1 */
#define UBLOX_CFG_MSGOUT_NMEA_ID_GRS_I2C	0x209100ce /* U1 -- Output rate of the NMEA-GX-GRS message on port I2C */
#define UBLOX_CFG_MSGOUT_NMEA_ID_GRS_SPI	0x209100d2 /* U1 -- Output rate of the NMEA-GX-GRS message on port SPI */
#define UBLOX_CFG_MSGOUT_NMEA_ID_GRS_UART1	0x209100cf /* U1 -- Output rate of the NMEA-GX-GRS message on port UART1 */
#define UBLOX_CFG_MSGOUT_NMEA_ID_GSA_I2C	0x209100bf /* U1 -- Output rate of the NMEA-GX-GSA message on port I2C */
#define UBLOX_CFG_MSGOUT_NMEA_ID_GSA_SPI	0x209100c3 /* U1 -- Output rate of the NMEA-GX-GSA message on port SPI */
#define UBLOX_CFG_MSGOUT_NMEA_ID_GSA_UART1	0x209100c0 /* U1 -- Output rate of the NMEA-GX-GSA message on port UART1 */
#define UBLOX_CFG_MSGOUT_NMEA_ID_GST_I2C	0x209100d3 /* U1 -- Output rate of the NMEA-GX-GST message on port I2C */
#define UBLOX_CFG_MSGOUT_NMEA_ID_GST_SPI	0x209100d7 /* U1 -- Output rate of the NMEA-GX-GST message on port SPI */
#define UBLOX_CFG_MSGOUT_NMEA_ID_GST_UART1	0x209100d4 /* U1 -- Output rate of the NMEA-GX-GST message on port UART1 */
#define UBLOX_CFG_MSGOUT_NMEA_ID_GSV_I2C	0x209100c4 /* U1 -- Output rate of the NMEA-GX-GSV message on port I2C */
#define UBLOX_CFG_MSGOUT_NMEA_ID_GSV_SPI	0x209100c8 /* U1 -- Output rate of the NMEA-GX-GSV message on port SPI */
#define UBLOX_CFG_MSGOUT_NMEA_ID_GSV_UART1	0x209100c5 /* U1 -- Output rate of the NMEA-GX-GSV message on port UART1 */
#define UBLOX_CFG_MSGOUT_NMEA_ID_RLM_I2C	0x20910400 /* U1 -- Output rate of the NMEA-GX-RLM message on port I2C */
#define UBLOX_CFG_MSGOUT_NMEA_ID_RLM_SPI	0x20910404 /* U1 -- Output rate of the NMEA-GX-RLM message on port SPI */
#define UBLOX_CFG_MSGOUT_NMEA_ID_RLM_UART1	0x20910401 /* U1 -- Output rate of the NMEA-GX-RLM message on port UART1 */
#define UBLOX_CFG_MSGOUT_NMEA_ID_RMC_I2C	0x209100ab /* U1 -- Output rate of the NMEA-GX-RMC message on port I2C */
#define UBLOX_CFG_MSGOUT_NMEA_ID_RMC_SPI	0x209100af /* U1 -- Output rate of the NMEA-GX-RMC message on port SPI */
#define UBLOX_CFG_MSGOUT_NMEA_ID_RMC_UART1	0x209100ac /* U1 -- Output rate of the NMEA-GX-RMC message on port UART1 */
#define UBLOX_CFG_MSGOUT_NMEA_ID_VLW_I2C	0x209100e7 /* U1 -- Output rate of the NMEA-GX-VLW message on port I2C */
#define UBLOX_CFG_MSGOUT_NMEA_ID_VLW_SPI	0x209100eb /* U1 -- Output rate of the NMEA-GX-VLW message on port SPI */
#define UBLOX_CFG_MSGOUT_NMEA_ID_VLW_UART1	0x209100e8 /* U1 -- Output rate of the NMEA-GX-VLW message on port UART1 */
#define UBLOX_CFG_MSGOUT_NMEA_ID_VTG_I2C	0x209100b0 /* U1 -- Output rate of the NMEA-GX-VTG message on port I2C */
#define UBLOX_CFG_MSGOUT_NMEA_ID_VTG_SPI	0x209100b4 /* U1 -- Output rate of the NMEA-GX-VTG message on port SPI */
#define UBLOX_CFG_MSGOUT_NMEA_ID_VTG_UART1	0x209100b1 /* U1 -- Output rate of the NMEA-GX-VTG message on port UART1 */
#define UBLOX_CFG_MSGOUT_NMEA_ID_ZDA_I2C	0x209100d8 /* U1 -- Output rate of the NMEA-GX-ZDA message on port I2C */
#define UBLOX_CFG_MSGOUT_NMEA_ID_ZDA_SPI	0x209100dc /* U1 -- Output rate of the NMEA-GX-ZDA message on port SPI */
#define UBLOX_CFG_MSGOUT_NMEA_ID_ZDA_UART1	0x209100d9 /* U1 -- Output rate of the NMEA-GX-ZDA message on port UART1 */
#define UBLOX_CFG_MSGOUT_PUBX_ID_POLYP_I2C	0x209100ec /* U1 -- Output rate of the NMEA-GX-PUBX00 message on port I2C */
#define UBLOX_CFG_MSGOUT_PUBX_ID_POLYP_SPI	0x209100f0 /* U1 -- Output rate of the NMEA-GX-PUBX00 message on port SPI */
#define UBLOX_CFG_MSGOUT_PUBX_ID_POLYP_UART1	0x209100ed /* U1 -- Output rate of the NMEA-GX-PUBX00 message on port UART1 */
#define UBLOX_CFG_MSGOUT_PUBX_ID_POLYS_I2C	0x209100f1 /* U1 -- Output rate of the NMEA-GX-PUBX03 message on port I2C */
#define UBLOX_CFG_MSGOUT_PUBX_ID_POLYS_SPI	0x209100f5 /* U1 -- Output rate of the NMEA-GX-PUBX03 message on port SPI */
#define UBLOX_CFG_MSGOUT_PUBX_ID_POLYS_UART1	0x209100f2 /* U1 -- Output rate of the NMEA-GX-PUBX03 message on port UART1 */
#define UBLOX_CFG_MSGOUT_PUBX_ID_POLYT_I2C	0x209100f6 /* U1 -- Output rate of the NMEA-GX-PUBX04 message on port I2C */
#define UBLOX_CFG_MSGOUT_PUBX_ID_POLYT_SPI	0x209100fa /* U1 -- Output rate of the NMEA-GX-PUBX04 message on port SPI */
#define UBLOX_CFG_MSGOUT_PUBX_ID_POLYT_UART1	0x209100f7 /* U1 -- Output rate of the NMEA-GX-PUBX04 message on port UART1 */
#define UBLOX_CFG_MSGOUT_UBX_MON_COMMS_I2C	0x2091034f /* U1 -- Output rate of the UBX-MON-COMMS message on port I2C */
#define UBLOX_CFG_MSGOUT_UBX_MON_COMMS_SPI	0x20910353 /* U1 -- Output rate of the UBX-MON-COMMS message on port SPI */
#define UBLOX_CFG_MSGOUT_UBX_MON_COMMS_UART1	0x20910350 /* U1 -- Output rate of the UBX-MON-COMMS message on port UART1 */
#define UBLOX_CFG_MSGOUT_UBX_MON_HW2_I2C	0x209101b9 /* U1 -- Output rate of the UBX-MON-HW2 message on port I2C */
#define UBLOX_CFG_MSGOUT_UBX_MON_HW2_SPI	0x209101bd /* U1 -- Output rate of the UBX-MON-HW2 message on port SPI */
#define UBLOX_CFG_MSGOUT_UBX_MON_HW2_UART1	0x209101ba /* U1 -- Output rate of the UBX-MON-HW2 message on port UART1 */
#define UBLOX_CFG_MSGOUT_UBX_MON_HW3_I2C	0x20910354 /* U1 -- Output rate of the UBX-MON-HW3 message on port I2C */
#define UBLOX_CFG_MSGOUT_UBX_MON_HW3_SPI	0x20910358 /* U1 -- Output rate of the UBX-MON-HW3 message on port SPI */
#define UBLOX_CFG_MSGOUT_UBX_MON_HW3_UART1	0x20910355 /* U1 -- Output rate of the UBX-MON-HW3 message on port UART1 */
#define UBLOX_CFG_MSGOUT_UBX_MON_HW_I2C	0x209101b4 /* U1 -- Output rate of the UBX-MON-HW message on port I2C */
#define UBLOX_CFG_MSGOUT_UBX_MON_HW_SPI	0x209101b8 /* U1 -- Output rate of the UBX-MON-HW message on port SPI */
#define UBLOX_CFG_MSGOUT_UBX_MON_HW_UART1	0x209101b5 /* U1 -- Output rate of the UBX-MON-HW message on port UART1 */
#define UBLOX_CFG_MSGOUT_UBX_MON_IO_I2C	0x209101a5 /* U1 -- Output rate of the UBX-MON-IO message on port I2C */
#define UBLOX_CFG_MSGOUT_UBX_MON_IO_SPI	0x209101a9 /* U1 -- Output rate of the UBX-MON-IO message on port SPI */
#define UBLOX_CFG_MSGOUT_UBX_MON_IO_UART1	0x209101a6 /* U1 -- Output rate of the UBX-MON-IO message on port UART1 */
#define UBLOX_CFG_MSGOUT_UBX_MON_MSGPP_I2C 	0x20910196 /* U1 -- Output rate of the UBX-MON-MSGPP message on port I2C */
#define UBLOX_CFG_MSGOUT_UBX_MON_MSGPP_SPI 	0x2091019a /* U1 -- Output rate of the UBX-MON-MSGPP message on port SPI */
#define UBLOX_CFG_MSGOUT_UBX_MON_MSGPP_UART1	0x20910197 /* U1 -- Output rate of the UBX-MON-MSGPP message on port UART1 */
#define UBLOX_CFG_MSGOUT_UBX_MON_RF_I2C	0x20910359 /* U1 -- Output rate of the UBX-MON-RF message on port I2C */
#define UBLOX_CFG_MSGOUT_UBX_MON_RF_SPI	0x2091035d /* U1 -- Output rate of the UBX-MON-RF message on port SPI */
#define UBLOX_CFG_MSGOUT_UBX_MON_RF_UART1	0x2091035a /* U1 -- Output rate of the UBX-MON-RF message on port UART1 */
#define UBLOX_CFG_MSGOUT_UBX_MON_RXBUF_I2C 	0x209101a0 /* U1 -- Output rate of the UBX-MON-RXBUF message on port I2C */
#define UBLOX_CFG_MSGOUT_UBX_MON_RXBUF_SPI 	0x209101a4 /* U1 -- Output rate of the UBX-MON-RXBUF message on port SPI */
#define UBLOX_CFG_MSGOUT_UBX_MON_RXBUF_UART1	0x209101a1 /* U1 -- Output rate of the UBX-MON-RXBUF message on port UART1 */
#define UBLOX_CFG_MSGOUT_UBX_MON_RXR_I2C	0x20910187 /* U1 -- Output rate of the UBX-MON-RXR message on port I2C */
#define UBLOX_CFG_MSGOUT_UBX_MON_RXR_SPI	0x2091018b /* U1 -- Output rate of the UBX-MON-RXR message on port SPI */
#define UBLOX_CFG_MSGOUT_UBX_MON_RXR_UART1	0x20910188 /* U1 -- Output rate of the UBX-MON-RXR message on port UART1 */
#define UBLOX_CFG_MSGOUT_UBX_MON_SPAN_I2C	0x2091038b /* U1 -- Output rate of the UBX-MON-SPAN message on port I2C */
#define UBLOX_CFG_MSGOUT_UBX_MON_SPAN_SPI	0x2091038f /* U1 -- Output rate of the UBX-MON-SPAN message on port SPI */
#define UBLOX_CFG_MSGOUT_UBX_MON_SPAN_UART1	0x2091038c /* U1 -- Output rate of the UBX-MON-SPAN message on port UART1 */
#define UBLOX_CFG_MSGOUT_UBX_MON_TXBUF_I2C 	0x2091019b /* U1 -- Output rate of the UBX-MON-TXBUF message on port I2C */
#define UBLOX_CFG_MSGOUT_UBX_MON_TXBUF_SPI 	0x2091019f /* U1 -- Output rate of the UBX-MON-TXBUF message on port SPI */
#define UBLOX_CFG_MSGOUT_UBX_MON_TXBUF_UART1	0x2091019c /* U1 -- Output rate of the UBX-MON-TXBUF message on port UART1 */
#define UBLOX_CFG_MSGOUT_UBX_NAV_AOPSTATUS_I2C	0x20910079 /* U1 -- Output rate of the UBX-NAV-AOPSTATUS message on port I2C */
#define UBLOX_CFG_MSGOUT_UBX_NAV_AOPSTATUS_SPI	0x2091007d /* U1 -- Output rate of the UBX-NAV-AOPSTATUS message on port SPI */
#define UBLOX_CFG_MSGOUT_UBX_NAV_AOPSTATUS_UART1	0x2091007a /* U1 -- Output rate of the UBX-NAV-AOPSTATUS message on port UART1 */
#define UBLOX_CFG_MSGOUT_UBX_NAV_CLOCK_I2C 	0x20910065 /* U1 -- Output rate of the UBX-NAV-CLOCK message on port I2C */
#define UBLOX_CFG_MSGOUT_UBX_NAV_CLOCK_SPI 	0x20910069 /* U1 -- Output rate of the UBX-NAV-CLOCK message on port SPI */
#define UBLOX_CFG_MSGOUT_UBX_NAV_CLOCK_UART1	0x20910066 /* U1 -- Output rate of the UBX-NAV-CLOCK message on port UART1 */
#define UBLOX_CFG_MSGOUT_UBX_NAV_COV_I2C	0x20910083 /* U1 -- Output rate of the UBX-NAV-COV message on port I2C */
#define UBLOX_CFG_MSGOUT_UBX_NAV_COV_SPI	0x20910087 /* U1 -- Output rate of the UBX-NAV-COV message on port SPI */
#define UBLOX_CFG_MSGOUT_UBX_NAV_COV_UART1	0x20910084 /* U1 -- Output rate of the UBX-NAV-COV message on port UART1 */
#define UBLOX_CFG_MSGOUT_UBX_NAV_DOP_I2C	0x20910038 /* U1 -- Output rate of the UBX-NAV-DOP message on port I2C */
#define UBLOX_CFG_MSGOUT_UBX_NAV_DOP_SPI	0x2091003c /* U1 -- Output rate of the UBX-NAV-DOP message on port SPI */
#define UBLOX_CFG_MSGOUT_UBX_NAV_DOP_UART1	0x20910039 /* U1 -- Output rate of the UBX-NAV-DOP message on port UART1 */
#define UBLOX_CFG_MSGOUT_UBX_NAV_EELL_I2C	0x20910313 /* U1 -- Output rate of the UBX-NAV-EELL message on port I2C */
#define UBLOX_CFG_MSGOUT_UBX_NAV_EELL_SPI	0x20910317 /* U1 -- Output rate of the UBX-NAV-EELL message on port SPI */
#define UBLOX_CFG_MSGOUT_UBX_NAV_EELL_UART1	0x20910314 /* U1 -- Output rate of the UBX-NAV-EELL message on port UART1 */
#define UBLOX_CFG_MSGOUT_UBX_NAV_EOE_I2C	0x2091015f /* U1 -- Output rate of the UBX-NAV-EOE message on port I2C */
#define UBLOX_CFG_MSGOUT_UBX_NAV_EOE_SPI	0x20910163 /* U1 -- Output rate of the UBX-NAV-EOE message on port SPI */
#define UBLOX_CFG_MSGOUT_UBX_NAV_EOE_UART1	0x20910160 /* U1 -- Output rate of the UBX-NAV-EOE message on port UART1 */
#define UBLOX_CFG_MSGOUT_UBX_NAV_ODO_I2C	0x2091007e /* U1 -- Output rate of the UBX-NAV-ODO message on port I2C */
#define UBLOX_CFG_MSGOUT_UBX_NAV_ODO_SPI	0x20910082 /* U1 -- Output rate of the UBX-NAV-ODO message on port SPI */
#define UBLOX_CFG_MSGOUT_UBX_NAV_ODO_UART1	0x2091007f /* U1 -- Output rate of the UBX-NAV-ODO message on port UART1 */
#define UBLOX_CFG_MSGOUT_UBX_NAV_ORB_I2C	0x20910010 /* U1 -- Output rate of the UBX-NAV-ORB message on port I2C */
#define UBLOX_CFG_MSGOUT_UBX_NAV_ORB_SPI	0x20910014 /* U1 -- Output rate of the UBX-NAV-ORB message on port SPI */
#define UBLOX_CFG_MSGOUT_UBX_NAV_ORB_UART1	0x20910011 /* U1 -- Output rate of the UBX-NAV-ORB message on port UART1 */
#define UBLOX_CFG_MSGOUT_UBX_NAV_POSECEF_I2C	0x20910024 /* U1 -- Output rate of the UBX-NAV-POSECEF message on port I2C */
#define UBLOX_CFG_MSGOUT_UBX_NAV_POSECEF_SPI	0x20910028 /* U1 -- Output rate of the UBX-NAV-POSECEF message on port SPI */
#define UBLOX_CFG_MSGOUT_UBX_NAV_POSECEF_UART1	0x20910025 /* U1 -- Output rate of the UBX-NAV-POSECEF message on port UART1 */
#define UBLOX_CFG_MSGOUT_UBX_NAV_POSLLH_I2C	0x20910029 /* U1 -- Output rate of the UBX-NAV-POSLLH message on port I2C */
#define UBLOX_CFG_MSGOUT_UBX_NAV_POSLLH_SPI 	0x2091002d /* U1 -- Output rate of the UBX-NAV-POSLLH message on port SPI */
#define UBLOX_CFG_MSGOUT_UBX_NAV_POSLLH_UART1	0x2091002a /* U1 -- Output rate of the UBX-NAV-POSLLH message on port UART1 */
#define UBLOX_CFG_MSGOUT_UBX_NAV_PVT_I2C	0x20910006 /* U1 -- Output rate of the UBX-NAV-PVT message on port I2C */
#define UBLOX_CFG_MSGOUT_UBX_NAV_PVT_SPI	0x2091000a /* U1 -- Output rate of the UBX-NAV-PVT message on port SPI */
#define UBLOX_CFG_MSGOUT_UBX_NAV_PVT_UART1	0x20910007 /* U1 -- Output rate of the UBX-NAV-PVT message on port UART1 */
#define UBLOX_CFG_MSGOUT_UBX_NAV_SAT_I2C	0x20910015 /* U1 -- Output rate of the UBX-NAV-SAT message on port I2C */
#define UBLOX_CFG_MSGOUT_UBX_NAV_SAT_SPI	0x20910019 /* U1 -- Output rate of the UBX-NAV-SAT message on port SPI */
#define UBLOX_CFG_MSGOUT_UBX_NAV_SAT_UART1	0x20910016 /* U1 -- Output rate of the UBX-NAV-SAT message on port UART1 */
#define UBLOX_CFG_MSGOUT_UBX_NAV_SBAS_I2C	0x2091006a /* U1 -- Output rate of the UBX-NAV-SBAS message on port I2C */
#define UBLOX_CFG_MSGOUT_UBX_NAV_SBAS_SPI	0x2091006e /* U1 -- Output rate of the UBX-NAV-SBAS message on port SPI */
#define UBLOX_CFG_MSGOUT_UBX_NAV_SBAS_UART1	0x2091006b /* U1 -- Output rate of the UBX-NAV-SBAS message on port UART1 */
#define UBLOX_CFG_MSGOUT_UBX_NAV_SIG_I2C	0x20910345 /* U1 -- Output rate of the UBX-NAV-SIG message on port I2C */
#define UBLOX_CFG_MSGOUT_UBX_NAV_SIG_SPI	0x20910349 /* U1 -- Output rate of the UBX-NAV-SIG message on port SPI */
#define UBLOX_CFG_MSGOUT_UBX_NAV_SIG_UART1	0x20910346 /* U1 -- Output rate of the UBX-NAV-SIG message on port UART1 */
#define UBLOX_CFG_MSGOUT_UBX_NAV_SLAS_I2C	0x20910336 /* U1 -- Output rate of the UBX-NAV-SLAS message on port I2C */
#define UBLOX_CFG_MSGOUT_UBX_NAV_SLAS_SPI	0x2091033a /* U1 -- Output rate of the UBX-NAV-SLAS message on port SPI */
#define UBLOX_CFG_MSGOUT_UBX_NAV_SLAS_UART1	0x20910337 /* U1 -- Output rate of the UBX-NAV-SLAS message on port UART1 */
#define UBLOX_CFG_MSGOUT_UBX_NAV_STATUS_I2C	0x2091001a /* U1 -- Output rate of the UBX-NAV-STATUS message on port I2C */
#define UBLOX_CFG_MSGOUT_UBX_NAV_STATUS_SPI 	0x2091001e /* U1 -- Output rate of the UBX-NAV-STATUS message on port SPI */
#define UBLOX_CFG_MSGOUT_UBX_NAV_STATUS_UART1	0x2091001b /* U1 -- Output rate of the UBX-NAV-STATUS message on port UART1 */
#define UBLOX_CFG_MSGOUT_UBX_NAV_TIMEBDS_I2C	0x20910051 /* U1 -- Output rate of the UBX-NAV-TIMEBDS message on port I2C */
#define UBLOX_CFG_MSGOUT_UBX_NAV_TIMEBDS_SPI	0x20910055 /* U1 -- Output rate of the UBX-NAV-TIMEBDS message on port SPI */
#define UBLOX_CFG_MSGOUT_UBX_NAV_TIMEBDS_UART1	0x20910052 /* U1 -- Output rate of the UBX-NAV-TIMEBDS message on port UART1 */
#define UBLOX_CFG_MSGOUT_UBX_NAV_TIMEGAL_I2C	0x20910056 /* U1 -- Output rate of the UBX-NAV-TIMEGAL message on port I2C */
#define UBLOX_CFG_MSGOUT_UBX_NAV_TIMEGAL_SPI	0x2091005a /* U1 -- Output rate of the UBX-NAV-TIMEGAL message on port SPI */
#define UBLOX_CFG_MSGOUT_UBX_NAV_TIMEGAL_UART1	0x20910057 /* U1 -- Output rate of the UBX-NAV-TIMEGAL message on port UART1 */
#define UBLOX_CFG_MSGOUT_UBX_NAV_TIMEGLO_I2C	0x2091004c /* U1 -- Output rate of the UBX-NAV-TIMEGLO message on port I2C */
#define UBLOX_CFG_MSGOUT_UBX_NAV_TIMEGLO_SPI	0x20910050 /* U1 -- Output rate of the UBX-NAV-TIMEGLO message on port SPI */
#define UBLOX_CFG_MSGOUT_UBX_NAV_TIMEGLO_UART1	0x2091004d /* U1 -- Output rate of the UBX-NAV-TIMEGLO message on port UART1 */
#define UBLOX_CFG_MSGOUT_UBX_NAV_TIMEGPS_I2C	0x20910047 /* U1 -- Output rate of the UBX-NAV-TIMEGPS message on port I2C */
#define UBLOX_CFG_MSGOUT_UBX_NAV_TIMEGPS_SPI	0x2091004b /* U1 -- Output rate of the UBX-NAV-TIMEGPS message on port SPI */
#define UBLOX_CFG_MSGOUT_UBX_NAV_TIMEGPS_UART1	0x20910048 /* U1 -- Output rate of the UBX-NAV-TIMEGPS message on port UART1 */
#define UBLOX_CFG_MSGOUT_UBX_NAV_TIMELS_I2C 	0x20910060 /* U1 -- Output rate of the UBX-NAV-TIMELS message on port I2C */
#define UBLOX_CFG_MSGOUT_UBX_NAV_TIMELS_SPI 	0x20910064 /* U1 -- Output rate of the UBX-NAV-TIMELS message on port SPI */
#define UBLOX_CFG_MSGOUT_UBX_NAV_TIMELS_UART1	0x20910061 /* U1 -- Output rate of the UBX-NAV-TIMELS message on port UART1 */
#define UBLOX_CFG_MSGOUT_UBX_NAV_TIMEQZSS_I2C 	0x20910386 /* U1 -- Output rate of the UBX-NAV-TIMEQZSS message on port I2C */
#define UBLOX_CFG_MSGOUT_UBX_NAV_TIMEQZSS_SPI 	0x2091038a /* U1 -- Output rate of the UBX-NAV-TIMEQZSS message on port SPI */
#define UBLOX_CFG_MSGOUT_UBX_NAV_TIMEQZSS_UART1 	0x20910387 /* U1 -- Output rate of the UBX-NAV-TIMEQZSS message on port UART1 */
#define UBLOX_CFG_MSGOUT_UBX_NAV_TIMEUTC_I2C	0x2091005b /* U1 -- Output rate of the UBX-NAV-TIMEUTC message on port I2C */
#define UBLOX_CFG_MSGOUT_UBX_NAV_TIMEUTC_SPI	0x2091005f /* U1 -- Output rate of the UBX-NAV-TIMEUTC message on port SPI */
#define UBLOX_CFG_MSGOUT_UBX_NAV_TIMEUTC_UART1	0x2091005c /* U1 -- Output rate of the UBX-NAV-TIMEUTC message on port UART1 */
#define UBLOX_CFG_MSGOUT_UBX_NAV_VELECEF_I2C	0x2091003d /* U1 -- Output rate of the UBX-NAV-VELECEF message on port I2C */
#define UBLOX_CFG_MSGOUT_UBX_NAV_VELECEF_SPI	0x20910041 /* U1 -- Output rate of the UBX-NAV-VELECEF message on port SPI */
#define UBLOX_CFG_MSGOUT_UBX_NAV_VELECEF_UART1	0x2091003e /* U1 -- Output rate of the UBX-NAV-VELECEF message on port UART1 */
#define UBLOX_CFG_MSGOUT_UBX_NAV_VELNED_I2C	0x20910042 /* U1 -- Output rate of the UBX-NAV-VELNED message on port I2C */
#define UBLOX_CFG_MSGOUT_UBX_NAV_VELNED_SPI	0x20910046 /* U1 -- Output rate of the UBX-NAV-VELNED message on port SPI */
#define UBLOX_CFG_MSGOUT_UBX_NAV_VELNED_UART1	0x20910043 /* U1 -- Output rate of the UBX-NAV-VELNED message on port UART1 */
#define UBLOX_CFG_MSGOUT_UBX_RXM_MEASX_I2C 	0x20910204 /* U1 -- Output rate of the UBX-RXM-MEASX message on port I2C */
#define UBLOX_CFG_MSGOUT_UBX_RXM_MEASX_SPI 	0x20910208 /* U1 -- Output rate of the UBX-RXM-MEASX message on port SPI */
#define UBLOX_CFG_MSGOUT_UBX_RXM_MEASX_UART1	0x20910205 /* U1 -- Output rate of the UBX-RXM-MEASX message on port UART1 */
#define UBLOX_CFG_MSGOUT_UBX_RXM_RAWX_I2C	0x209102a4 /* U1 -- Output rate of the UBX-RXM-RAWX message on port I2C */
#define UBLOX_CFG_MSGOUT_UBX_RXM_RAWX_SPI	0x209102a8 /* U1 -- Output rate of the UBX-RXM-RAWX message on port SPI */
#define UBLOX_CFG_MSGOUT_UBX_RXM_RAWX_UART1	0x209102a5 /* U1 -- Output rate of the UBX-RXM-RAWX message on port UART1 */
#define UBLOX_CFG_MSGOUT_UBX_RXM_RLM_I2C	0x2091025e /* U1 -- Output rate of the UBX-RXM-RLM message on port I2C */
#define UBLOX_CFG_MSGOUT_UBX_RXM_RLM_SPI	0x20910262 /* U1 -- Output rate of the UBX-RXM-RLM message on port SPI */
#define UBLOX_CFG_MSGOUT_UBX_RXM_RLM_UART1	0x2091025f /* U1 -- Output rate of the UBX-RXM-RLM message on port UART1 */
#define UBLOX_CFG_MSGOUT_UBX_RXM_SFRBX_I2C 	0x20910231 /* U1 -- Output rate of the UBX-RXM-SFRBX message on port I2C */
#define UBLOX_CFG_MSGOUT_UBX_RXM_SFRBX_SPI 	0x20910235 /* U1 -- Output rate of the UBX-RXM-SFRBX message on port SPI */
#define UBLOX_CFG_MSGOUT_UBX_RXM_SFRBX_UART1	0x20910232 /* U1 -- Output rate of the UBX-RXM-SFRBX message on port UART1 */
#define UBLOX_CFG_MSGOUT_UBX_TIM_TM2_I2C	0x20910178 /* U1 -- Output rate of the UBX-TIM-TM2 message on port I2C */
#define UBLOX_CFG_MSGOUT_UBX_TIM_TM2_SPI	0x2091017c /* U1 -- Output rate of the UBX-TIM-TM2 message on port SPI */
#define UBLOX_CFG_MSGOUT_UBX_TIM_TM2_UART1	0x20910179 /* U1 -- Output rate of the UBX-TIM-TM2 message on port UART1 */
#define UBLOX_CFG_MSGOUT_UBX_TIM_TP_I2C	0x2091017d /* U1 -- Output rate of the UBX-TIM-TP message on port I2C */
#define UBLOX_CFG_MSGOUT_UBX_TIM_TP_SPI	0x20910181 /* U1 -- Output rate of the UBX-TIM-TP message on port SPI */
#define UBLOX_CFG_MSGOUT_UBX_TIM_TP_UART1	0x2091017e /* U1 -- Output rate of the UBX-TIM-TP message on port UART1 */
#define UBLOX_CFG_MSGOUT_UBX_TIM_VRFY_I2C	0x20910092 /* U1 -- Output rate of the UBX-TIM-VRFY message on port I2C */
#define UBLOX_CFG_MSGOUT_UBX_TIM_VRFY_SPI	0x20910096 /* U1 -- Output rate of the UBX-TIM-VRFY message on port SPI */
#define UBLOX_CFG_MSGOUT_UBX_TIM_VRFY_UART1	0x20910093 /* U1 -- Output rate of the UBX-TIM-VRFY message on port UART1 */

#endif /* _AO_GPS_UBLOX_H_ */
