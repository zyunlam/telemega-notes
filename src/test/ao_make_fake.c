/*
 * Copyright © 2009 Keith Packard <keithp@keithp.com>
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

#define _GNU_SOURCE

#include <stdint.h>
#include <stdio.h>
#include <stdlib.h>
#include <stddef.h>
#include <string.h>
#include <getopt.h>
#include <math.h>

#define GRAVITY 9.80665

#define AO_HERTZ	100

#define HAS_ADC 1
#define AO_DATA_RING	64
#define ao_data_ring_next(n)	(((n) + 1) & (AO_DATA_RING - 1))
#define ao_data_ring_prev(n)	(((n) - 1) & (AO_DATA_RING - 1))

#define AO_GPS_NEW_DATA		1
#define AO_GPS_NEW_TRACKING	2

int ao_gps_new;

#if !defined(TELEMEGA) && !defined(TELEMETRUM_V2) && !defined(EASYMINI)
#define TELEMETRUM_V1 1
#endif

#if TELEMEGA
#define AO_ADC_NUM_SENSE	6
#define HAS_MS5607		1
#define HAS_MPU6000		1
#define HAS_MMA655X		1
#define HAS_HMC5883 		1
#define HAS_BEEP		1
#define AO_CONFIG_MAX_SIZE	1024
#define AO_MMA655X_INVERT	0

struct ao_adc {
	int16_t			sense[AO_ADC_NUM_SENSE];
	int16_t			v_batt;
	int16_t			v_pbatt;
	int16_t			temp;
};
#endif

#if TELEMETRUM_V2
#define AO_ADC_NUM_SENSE	2
#define HAS_MS5607		1
#define HAS_MMA655X		1
#define AO_MMA655X_INVERT	0
#define HAS_BEEP		1
#define AO_CONFIG_MAX_SIZE	1024

struct ao_adc {
	int16_t			sense_a;
	int16_t			sense_m;
	int16_t			v_batt;
	int16_t			temp;
};
#endif

#if EASYMINI
#define AO_ADC_NUM_SENSE	2
#define HAS_MS5607		1
#define HAS_BEEP		1
#define AO_CONFIG_MAX_SIZE	1024

struct ao_adc {
	int16_t			sense_a;
	int16_t			sense_m;
	int16_t			v_batt;
};
#endif

#if TELEMETRUM_V1
/*
 * One set of samples read from the A/D converter
 */
struct ao_adc {
	int16_t		accel;		/* accelerometer */
	int16_t		pres;		/* pressure sensor */
	int16_t		pres_real;	/* unclipped */
	int16_t		temp;		/* temperature sensor */
	int16_t		v_batt;		/* battery voltage */
	int16_t		sense_d;	/* drogue continuity sense */
	int16_t		sense_m;	/* main continuity sense */
};

#ifndef HAS_ACCEL
#define HAS_ACCEL 1
#define HAS_ACCEL_REF 0
#endif

#endif

#define __pdata
#define __data
#define __xdata
#define __code
#define __reentrant

#define HAS_FLIGHT 1
#define HAS_IGNITE 1
#define HAS_USB 1
#define HAS_GPS 1

#include <ao_data.h>
#include <ao_log.h>
#include <ao_telemetry.h>
#include <ao_sample.h>
#include <ao_fake_flight.h>

#if TELEMEGA
int ao_gps_count;
struct ao_telemetry_location ao_gps_first;
struct ao_telemetry_location ao_gps_prev;
struct ao_telemetry_location ao_gps_static;

struct ao_telemetry_satellite ao_gps_tracking;

static inline double sqr(double a) { return a * a; }

void
cc_great_circle (double start_lat, double start_lon,
		 double end_lat, double end_lon,
		 double *dist, double *bearing)
{
	const double rad = M_PI / 180;
	const double earth_radius = 6371.2 * 1000;	/* in meters */
	double lat1 = rad * start_lat;
	double lon1 = rad * -start_lon;
	double lat2 = rad * end_lat;
	double lon2 = rad * -end_lon;

//	double d_lat = lat2 - lat1;
	double d_lon = lon2 - lon1;

	/* From http://en.wikipedia.org/wiki/Great-circle_distance */
	double vdn = sqrt(sqr(cos(lat2) * sin(d_lon)) +
			  sqr(cos(lat1) * sin(lat2) -
			      sin(lat1) * cos(lat2) * cos(d_lon)));
	double vdd = sin(lat1) * sin(lat2) + cos(lat1) * cos(lat2) * cos(d_lon);
	double d = atan2(vdn,vdd);
	double course;

	if (cos(lat1) < 1e-20) {
		if (lat1 > 0)
			course = M_PI;
		else
			course = -M_PI;
	} else {
		if (d < 1e-10)
			course = 0;
		else
			course = acos((sin(lat2)-sin(lat1)*cos(d)) /
				      (sin(d)*cos(lat1)));
		if (sin(lon2-lon1) > 0)
			course = 2 * M_PI-course;
	}
	*dist = d * earth_radius;
	*bearing = course * 180/M_PI;
}

double
ao_distance_from_pad(void)
{
	double	dist, bearing;
	if (!ao_gps_count)
		return 0;

	cc_great_circle(ao_gps_first.latitude / 1e7,
			ao_gps_first.longitude / 1e7,
			ao_gps_static.latitude / 1e7,
			ao_gps_static.longitude / 1e7,
			&dist, &bearing);
	return dist;
}

double
ao_gps_angle(void)
{
	double	dist, bearing;
	double	height;
	double	angle;

	if (ao_gps_count < 2)
		return 0;

	cc_great_circle(ao_gps_prev.latitude / 1e7,
			ao_gps_prev.longitude / 1e7,
			ao_gps_static.latitude / 1e7,
			ao_gps_static.longitude / 1e7,
			&dist, &bearing);
	height = AO_TELEMETRY_LOCATION_ALTITUDE(&ao_gps_static) - AO_TELEMETRY_LOCATION_ALTITUDE(&ao_gps_prev);

	angle = atan2(dist, height);
	return angle * 180/M_PI;
}
#endif

#define to_fix16(x) ((int16_t) ((x) * 65536.0 + 0.5))
#define to_fix32(x) ((int32_t) ((x) * 65536.0 + 0.5))
#define from_fix(x)	((x) >> 16)

#define ACCEL_NOSE_UP	(ao_accel_2g >> 2)

#define FALSE 0
#define TRUE 1

volatile struct ao_data ao_data_ring[AO_DATA_RING];
volatile uint8_t ao_data_head;
int	ao_summary = 0;

#define ao_led_on(l)
#define ao_led_off(l)
#define ao_timer_set_adc_interval(i)
#define ao_wakeup(wchan) ao_dump_state()
#define ao_cmd_register(c)
#define ao_usb_disable()
#define ao_telemetry_set_interval(x)
#define ao_rdf_set(rdf)
#define ao_packet_slave_start()
#define ao_packet_slave_stop()
#define flush()

enum ao_igniter {
	ao_igniter_drogue = 0,
	ao_igniter_main = 1
};

struct ao_data ao_data_static;

int	drogue_height;
double	drogue_time;
int	main_height;
double	main_time;

int	tick_offset;

static ao_k_t	ao_k_height;

int16_t
ao_time(void)
{
	return ao_data_static.tick;
}

void
ao_delay(int16_t interval)
{
	return;
}

void
ao_ignite(enum ao_igniter igniter)
{
	double time = (double) (ao_data_static.tick + tick_offset) / 100;

	if (igniter == ao_igniter_drogue) {
		drogue_time = time;
		drogue_height = ao_k_height >> 16;
	} else {
		main_time = time;
		main_height = ao_k_height >> 16;
	}
}

struct ao_task {
	int dummy;
};

#define ao_add_task(t,f,n) ((void) (t))

#define ao_log_start()
#define ao_log_stop()

#define AO_MS_TO_TICKS(ms)	((ms) / 10)
#define AO_SEC_TO_TICKS(s)	((s) * 100)

#define AO_FLIGHT_TEST

int	ao_flight_debug;

struct ao_eeprom	*eeprom;
uint32_t		eeprom_offset;

FILE *emulator_in;
FILE *fake_out;
char *emulator_app;
char *emulator_name;
char *emulator_info;
double emulator_error_max = 4;
double emulator_height_error_max = 20;	/* noise in the baro sensor */

void
ao_dump_state(void);

void
ao_sleep(void *wchan);

const char * const ao_state_names[] = {
	"startup", "idle", "pad", "boost", "fast",
	"coast", "drogue", "main", "landed", "invalid"
};

struct ao_cmds {
	void		(*func)(void);
	const char	*help;
};

#define ao_xmemcpy(d,s,c) memcpy(d,s,c)
#define ao_xmemset(d,v,c) memset(d,v,c)
#define ao_xmemcmp(d,s,c) memcmp(d,s,c)

#define AO_NEED_ALTITUDE_TO_PRES 1
#if TELEMEGA || TELEMETRUM_V2 || EASYMINI
#include "ao_convert_pa.c"
#include <ao_ms5607.h>
struct ao_ms5607_prom	ao_ms5607_prom;
#include "ao_ms5607_convert.c"
#if TELEMEGA
#define AO_PYRO_NUM	4
#include <ao_pyro.h>
#endif
#else
#include "ao_convert.c"
#endif

#include <ao_config.h>
#include <ao_fake_flight.h>
#include <ao_eeprom_read.h>
#include <ao_log.h>

#define ao_config_get()

struct ao_config ao_config;

#define DATA_TO_XDATA(x) (x)


extern int16_t ao_ground_accel, ao_flight_accel;
extern int16_t ao_accel_2g;

typedef int16_t	accel_t;

uint16_t	ao_serial_number;
uint16_t	ao_flight_number;

extern uint16_t	ao_sample_tick;

extern alt_t	ao_sample_height;
extern accel_t	ao_sample_accel;
extern int32_t	ao_accel_scale;
extern alt_t	ao_ground_height;
extern alt_t	ao_sample_alt;

double ao_sample_qangle;

int ao_sample_prev_tick;
uint16_t	prev_tick;

#include "ao_kalman.c"
#include "ao_sqrt.c"
#include "ao_sample.c"
#include "ao_flight.c"
#if TELEMEGA
#define AO_PYRO_NUM	4

#define AO_PYRO_0	0
#define AO_PYRO_1	1
#define AO_PYRO_2	2
#define AO_PYRO_3	3

#define PYRO_DBG	1
#endif
#include "ao_eeprom_read.c"
#include "ao_eeprom_read_old.c"

#define to_double(f)	((f) / 65536.0)

static int	ao_records_read = 0;
static int	ao_eof_read = 0;
#if !EASYMINI
static int	ao_flight_ground_accel;
#endif
static int	ao_flight_started = 0;

#if HAS_MPU6000
static struct ao_mpu6000_sample	ao_ground_mpu6000;
#endif

#if HAS_ACCEL
int ao_error_h_sq_avg;
#endif

void
ao_test_exit(void)
{
	putc(0, fake_out);
	fflush(fake_out);
	exit(0);
}

void
ao_insert(void)
{
	putc(1, fake_out);
	fwrite(&ao_data_static, sizeof (ao_data_static), 1, fake_out);
}


uint16_t
uint16(uint8_t *bytes, int off)
{
	return (uint16_t) bytes[off] | (((uint16_t) bytes[off+1]) << 8);
}

int16_t
int16(uint8_t *bytes, int off)
{
	return (int16_t) uint16(bytes, off);
}

uint32_t
uint32(uint8_t *bytes, int off)
{
	return (uint32_t) bytes[off] | (((uint32_t) bytes[off+1]) << 8) |
		(((uint32_t) bytes[off+2]) << 16) |
		(((uint32_t) bytes[off+3]) << 24);
}

int32_t
int32(uint8_t *bytes, int off)
{
	return (int32_t) uint32(bytes, off);
}

uint32_t
uint24(uint8_t *bytes, int off)
{
	return (uint32_t) bytes[off] | (((uint32_t) bytes[off+1]) << 8) |
		(((uint32_t) bytes[off+2]) << 16);
}

int32_t
int24(uint8_t *bytes, int off)
{
	return (int32_t) uint24(bytes, off);
}

static int log_format;

void
ao_sleep(void *wchan)
{
}

void fake_read(void)
{
	if (ao_records_read > 2 && ao_records_read < 500)
	{
#if TELEMEGA
		ao_data_static.mpu6000 = ao_ground_mpu6000;
#endif
#if TELEMETRUM_V1
		ao_data_static.adc.accel = ao_flight_ground_accel;
#endif
		++ao_records_read;
		ao_insert();
		return;
	}

	if (eeprom) {
#if TELEMEGA
		struct ao_log_mega	*log_mega;
#endif
#if TELEMETRUM_V2
		struct ao_log_metrum	*log_metrum;
#endif
#if EASYMINI
		struct ao_log_mini	*log_mini;
#endif
#if TELEMETRUM_V1
		struct ao_log_record	*log_record;
#endif

		if (eeprom_offset >= eeprom->len) {
			if (++ao_eof_read >= 100) {
				ao_test_exit();
			}
			ao_data_static.tick += 10;
			ao_insert();
			return;
		}
		switch (eeprom->log_format) {
#if TELEMEGA
		case AO_LOG_FORMAT_TELEMEGA_OLD:
		case AO_LOG_FORMAT_TELEMEGA:
			log_mega = (struct ao_log_mega *) &eeprom->data[eeprom_offset];
			eeprom_offset += sizeof (*log_mega);
			switch (log_mega->type) {
			case AO_LOG_FLIGHT:
				ao_flight_number = log_mega->u.flight.flight;
				ao_flight_ground_accel = log_mega->u.flight.ground_accel;
				ao_flight_started = 1;
				ao_ground_pres = log_mega->u.flight.ground_pres;
				ao_ground_height = ao_pa_to_altitude(ao_ground_pres);
				ao_ground_accel_along = log_mega->u.flight.ground_accel_along;
				ao_ground_accel_across = log_mega->u.flight.ground_accel_across;
				ao_ground_accel_through = log_mega->u.flight.ground_accel_through;
				ao_ground_roll = log_mega->u.flight.ground_roll;
				ao_ground_pitch = log_mega->u.flight.ground_pitch;
				ao_ground_yaw = log_mega->u.flight.ground_yaw;
				ao_ground_mpu6000.accel_x = ao_ground_accel_across;
				ao_ground_mpu6000.accel_y = ao_ground_accel_along;
				ao_ground_mpu6000.accel_z = ao_ground_accel_through;
				ao_ground_mpu6000.gyro_x = ao_ground_pitch >> 9;
				ao_ground_mpu6000.gyro_y = ao_ground_roll >> 9;
				ao_ground_mpu6000.gyro_z = ao_ground_yaw >> 9;
				break;
			case AO_LOG_STATE:
				break;
			case AO_LOG_SENSOR:
				ao_data_static.tick = log_mega->tick;
				ao_data_static.ms5607_raw.pres = log_mega->u.sensor.pres;
				ao_data_static.ms5607_raw.temp = log_mega->u.sensor.temp;
				ao_data_static.mpu6000.accel_x = log_mega->u.sensor.accel_x;
				ao_data_static.mpu6000.accel_y = log_mega->u.sensor.accel_y;
				ao_data_static.mpu6000.accel_z = log_mega->u.sensor.accel_z;
				ao_data_static.mpu6000.gyro_x = log_mega->u.sensor.gyro_x;
				ao_data_static.mpu6000.gyro_y = log_mega->u.sensor.gyro_y;
				ao_data_static.mpu6000.gyro_z = log_mega->u.sensor.gyro_z;
				ao_data_static.hmc5883.x = log_mega->u.sensor.mag_x;
				ao_data_static.hmc5883.y = log_mega->u.sensor.mag_y;
				ao_data_static.hmc5883.z = log_mega->u.sensor.mag_z;
				ao_data_static.mma655x = log_mega->u.sensor.accel;
				if (ao_config.pad_orientation != AO_PAD_ORIENTATION_ANTENNA_UP)
					ao_data_static.mma655x = ao_data_accel_invert(ao_data_static.mma655x);
				ao_records_read++;
				ao_insert();
				return;
			case AO_LOG_TEMP_VOLT:
				break;
			case AO_LOG_GPS_TIME:
				ao_gps_prev = ao_gps_static;
				ao_gps_static.tick = log_mega->tick;
				ao_gps_static.latitude = log_mega->u.gps.latitude;
				ao_gps_static.longitude = log_mega->u.gps.longitude;
				{
					int16_t	altitude_low = log_mega->u.gps.altitude_low;
					int16_t altitude_high = log_mega->u.gps.altitude_high;
					int32_t altitude = altitude_low | ((int32_t) altitude_high << 16);

					AO_TELEMETRY_LOCATION_SET_ALTITUDE(&ao_gps_static, altitude);
				}
				ao_gps_static.flags = log_mega->u.gps.flags;
				if (!ao_gps_count)
					ao_gps_first = ao_gps_static;
				ao_gps_count++;
				break;
			case AO_LOG_GPS_SAT:
				break;
			}
			break;
#endif
#if TELEMETRUM_V2
		case AO_LOG_FORMAT_TELEMETRUM:
			log_metrum = (struct ao_log_metrum *) &eeprom->data[eeprom_offset];
			eeprom_offset += sizeof (*log_metrum);
			switch (log_metrum->type) {
			case AO_LOG_FLIGHT:
				ao_flight_started = 1;
				ao_flight_number = log_metrum->u.flight.flight;
				ao_flight_ground_accel = log_metrum->u.flight.ground_accel;
				ao_ground_pres = log_metrum->u.flight.ground_pres;
				ao_ground_height = ao_pa_to_altitude(ao_ground_pres);
				break;
			case AO_LOG_SENSOR:
				ao_data_static.tick = log_metrum->tick;
				ao_data_static.ms5607_raw.pres = log_metrum->u.sensor.pres;
				ao_data_static.ms5607_raw.temp = log_metrum->u.sensor.temp;
				ao_data_static.mma655x = log_metrum->u.sensor.accel;
				ao_records_read++;
				ao_insert();
				return;
			}
			break;
#endif
#if EASYMINI
		case AO_LOG_FORMAT_EASYMINI1:
		case AO_LOG_FORMAT_EASYMINI2:
		case AO_LOG_FORMAT_TELEMINI3:
			log_mini = (struct ao_log_mini *) &eeprom->data[eeprom_offset];
			eeprom_offset += sizeof (*log_mini);
			switch (log_mini->type) {
			case AO_LOG_FLIGHT:
				ao_flight_started = 1;
				ao_flight_number = log_mini->u.flight.flight;
				ao_ground_pres = log_mini->u.flight.ground_pres;
				ao_ground_height = ao_pa_to_altitude(ao_ground_pres);
				break;
			case AO_LOG_SENSOR:
				ao_data_static.tick = log_mini->tick;
				ao_data_static.ms5607_raw.pres = int24(log_mini->u.sensor.pres, 0);
				ao_data_static.ms5607_raw.temp = int24(log_mini->u.sensor.temp, 0);
				ao_records_read++;
				ao_insert();
				return;
			}
			break;
#endif
#if TELEMETRUM_V1
		case AO_LOG_FORMAT_FULL:
		case AO_LOG_FORMAT_TINY:
			log_record = (struct ao_log_record *) &eeprom->data[eeprom_offset];
			eeprom_offset += sizeof (*log_record);
			switch (log_record->type) {
			case AO_LOG_FLIGHT:
				ao_flight_started = 1;
				ao_flight_ground_accel = log_record->u.flight.ground_accel;
				ao_flight_number = log_record->u.flight.flight;
				break;
			case AO_LOG_SENSOR:
			case 'P':	/* ancient telemini */
				ao_data_static.tick = log_record->tick;
				ao_data_static.adc.accel = log_record->u.sensor.accel;
				ao_data_static.adc.pres_real = log_record->u.sensor.pres;
				ao_data_static.adc.pres = log_record->u.sensor.pres;
				ao_records_read++;
				ao_insert();
				return;
			case AO_LOG_TEMP_VOLT:
				ao_data_static.tick = log_record->tick;;
				ao_data_static.adc.temp = log_record->u.temp_volt.temp;
				ao_data_static.adc.v_batt = log_record->u.temp_volt.v_batt;
				break;
			}
			break;
#endif
		default:
			fprintf (stderr, "invalid log format %d\n", log_format);
			ao_test_exit();
		}
	}
}
#define COUNTS_PER_G 264.8

void
ao_dump_state(void)
{
}

static const struct option options[] = {
	{ .name = "summary", .has_arg = 0, .val = 's' },
	{ .name = "debug", .has_arg = 0, .val = 'd' },
	{ .name = "info", .has_arg = 1, .val = 'i' },
	{ .name = "out", .has_arg = 1, .val = 'o' },
	{ 0, 0, 0, 0},
};

void run_flight_fixed(char *name, FILE *f, int summary, char *info)
{
	char			c;
	struct ao_fake_calib	fake_calib;

	emulator_name = name;
	emulator_in = f;
	emulator_info = info;
	ao_summary = summary;

	c = getc(f);
	ungetc(c, f);
	if (c == '{')
		eeprom = ao_eeprom_read(f);
	else
		eeprom = ao_eeprom_read_old(f);

	if (!eeprom) {
		fprintf(stderr, "%s: load failed\n", name);
		exit(1);
	}
#if HAS_MS5607
	ao_ms5607_prom = eeprom->ms5607_prom;
#endif
	ao_config = eeprom->config;
	ao_serial_number = eeprom->serial_number;
	log_format = eeprom->log_format;

	fprintf(fake_out, "F %x %x\n",
	       (int) sizeof (struct ao_fake_calib),
	       (int) sizeof (struct ao_data));

	fake_calib.major = AO_FAKE_CALIB_MAJOR;
	fake_calib.minor = AO_FAKE_CALIB_MINOR;
	fake_calib.accel_plus_g = eeprom->config.accel_plus_g;
	fake_calib.accel_minus_g = eeprom->config.accel_minus_g;
	fake_calib.accel_zero_along = eeprom->config.accel_zero_along;
	fake_calib.accel_zero_across = eeprom->config.accel_zero_across;
	fake_calib.accel_zero_through = eeprom->config.accel_zero_through;
	fake_calib.ms5607_prom = eeprom->ms5607_prom;
	fwrite(&fake_calib, sizeof (fake_calib), 1, fake_out);
	for (;;)
		fake_read();
}

int
main (int argc, char **argv)
{
	int	summary = 0;
	int	c;
	int	i;
	char	*info = NULL;
	char	*out_name = NULL;

#if HAS_ACCEL
	emulator_app="full";
#else
	emulator_app="baro";
#endif
	while ((c = getopt_long(argc, argv, "sdi:o:", options, NULL)) != -1) {
		switch (c) {
		case 's':
			summary = 1;
			break;
		case 'd':
			ao_flight_debug = 1;
			break;
		case 'i':
			info = optarg;
			break;
		case 'o':
			out_name = optarg;
			break;
		}
	}
	if (out_name) {
		fake_out = fopen(out_name, "w");
		if (!fake_out) {
			perror(out_name);
			exit(1);
		}
	}

	if (!fake_out)
		fake_out = stdout;

	if (optind == argc)
		run_flight_fixed("<stdin>", stdin, summary, info);
	else
		for (i = optind; i < argc; i++) {
			FILE	*f = fopen(argv[i], "r");
			if (!f) {
				perror(argv[i]);
				continue;
			}
			run_flight_fixed(argv[i], f, summary, info);
			fclose(f);
		}
	exit(0);
}
