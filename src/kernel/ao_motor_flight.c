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

#ifndef AO_FLIGHT_TEST
#include "ao.h"
#include <ao_log.h>
#endif

#include <ao_flight.h>

/* Main flight thread. */

enum ao_flight_state	ao_flight_state;	/* current flight state */
AO_TICK_TYPE		ao_launch_tick;		/* time of first boost detect */

#if HAS_SENSOR_ERRORS
/* Any sensor can set this to mark the flight computer as 'broken' */
uint8_t			ao_sensor_errors;
#endif

/*
 * track min/max data over a long interval to detect
 * resting
 */
static AO_TICK_TYPE	ao_interval_end;

#define init_bounds(_cur, _min, _max) do {				\
		_min = _max = _cur;					\
	} while (0)

#define check_bounds(_cur, _min, _max) do {	\
		if (_cur < _min)		\
			_min = _cur;		\
		if (_cur > _max)		\
			_max = _cur;		\
	} while(0)

uint8_t			ao_flight_force_idle;

/* Compute ADC value change given a defined pressure change in Pa */

static inline int16_t
ao_delta_pressure_to_adc(uint32_t pressure)
{
	static const double volts_base = AO_PRESSURE_VOLTS_BASE;
	static const double volts_max = AO_PRESSURE_VOLTS_MAX;

	/* Compute change in voltage from the sensor */
	double	volts = (double) pressure / AO_FULL_SCALE_PRESSURE * (volts_max - volts_base);

	/* voltage divider in front of the ADC input to decivolts */
	double	adc_dv = volts * (10.0 * (double) AO_PRESSURE_DIV_MINUS /
				  ((double) AO_PRESSURE_DIV_PLUS + (double) AO_PRESSURE_DIV_MINUS));

	/* convert to ADC output value */
	double	adc = adc_dv * AO_ADC_MAX / AO_ADC_REFERENCE_DV;

	if (adc > AO_ADC_MAX)
		adc = AO_ADC_MAX;
	if (adc < 0)
		adc = 0;

	return (int16_t) adc;
}

#define AO_BOOST_DETECT			ao_delta_pressure_to_adc(AO_BOOST_DETECT_PRESSURE)
#define AO_QUIET_DETECT			ao_delta_pressure_to_adc(AO_QUIET_DETECT_PRESSURE)

/*
 * Landing is detected by getting constant readings from pressure sensor
 * for a fairly long time (AO_INTERVAL_TICKS), along with the max being
 * less than the boost detect pressure
 */
#define AO_INTERVAL_TICKS	AO_SEC_TO_TICKS(10)

static AO_TICK_TYPE		ao_interval_end;
static motor_pressure_t		ao_interval_min_motor_pressure, ao_interval_max_motor_pressure;

#define abs(a)	((a) < 0 ? -(a) : (a))

void
ao_flight(void)
{
	ao_sample_init();
	ao_flight_state = ao_flight_startup;
	for (;;) {

		/*
		 * Process ADC samples, just looping
		 * until the sensors are calibrated.
		 */
		if (!ao_sample())
			continue;

		switch (ao_flight_state) {
		case ao_flight_startup:

			if (!ao_flight_force_idle)
 			{
				/* Set pad mode - we can fly! */
				ao_flight_state = ao_flight_pad;
#if HAS_USB && !HAS_FLIGHT_DEBUG && !HAS_SAMPLE_PROFILE && !DEBUG
				/* Disable the USB controller in flight mode
				 * to save power
				 */
#if HAS_FAKE_FLIGHT
				if (!ao_fake_flight_active)
#endif
					ao_usb_disable();
#endif

#if AO_LED_RED
				/* signal successful initialization by turning off the LED */
				ao_led_off(AO_LED_RED);
#endif
			} else {
				/* Set idle mode */
				ao_flight_state = ao_flight_idle;
#if HAS_SENSOR_ERRORS
				if (ao_sensor_errors)
					ao_flight_state = ao_flight_invalid;
#endif

#if AO_LED_RED
				/* signal successful initialization by turning off the LED */
				ao_led_off(AO_LED_RED);
#endif
			}
			/* wakeup threads due to state change */
			ao_wakeup(&ao_flight_state);

			break;

		case ao_flight_pad:
			/* pad to boost:
			 *
			 * motor pressure rise > 50psi
			 */
			if (ao_sample_motor_pressure - ao_ground_motor_pressure >= AO_BOOST_DETECT) 
			{
				ao_flight_state = ao_flight_boost;
				ao_wakeup(&ao_flight_state);

				ao_launch_tick = ao_sample_tick;

				/* start logging data */
#if HAS_LOG
				ao_log_start();
#endif
				/* Initialize landing detection interval values */
				ao_interval_end = ao_sample_tick + AO_INTERVAL_TICKS;

				init_bounds(ao_sample_motor_pressure, ao_interval_min_motor_pressure, ao_interval_max_motor_pressure);
			}
			break;
		case ao_flight_boost:
			/* boost to landed:
			 *
			 * motor pressure low and stable for more than 10 seconds
			 */
			check_bounds(ao_sample_motor_pressure, ao_interval_min_motor_pressure,
				     ao_interval_max_motor_pressure);

			if ((AO_TICK_SIGNED) (ao_sample_tick - ao_interval_end) >= 0) {
				if (ao_interval_max_motor_pressure - ao_ground_motor_pressure <= AO_BOOST_DETECT &&
				    ao_interval_max_motor_pressure - ao_interval_min_motor_pressure <= AO_QUIET_DETECT_PRESSURE)
				{
					ao_flight_state = ao_flight_landed;
					ao_wakeup(&ao_flight_state);
#if HAS_ADC
					/* turn off the ADC capture */
					ao_timer_set_adc_interval(0);
#endif
				}

				/* Reset interval values */
				ao_interval_end = ao_sample_tick + AO_INTERVAL_TICKS;

				init_bounds(ao_sample_motor_pressure, ao_interval_min_motor_pressure, ao_interval_max_motor_pressure);
			}
			break;
		default:
			break;
		}
	}
}

#if HAS_FLIGHT_DEBUG
static inline int int_part(ao_v_t i)	{ return i >> 4; }
static inline int frac_part(ao_v_t i)	{ return ((i & 0xf) * 100 + 8) / 16; }

static void
ao_flight_dump(void)
{
#if HAS_ACCEL
	ao_v_t	accel;

	accel = ((ao_config.accel_plus_g - ao_sample_accel) * ao_accel_scale) >> 16;
#endif

	printf ("sample:\n");
	printf ("  tick        %d\n", ao_sample_tick);
#if HAS_BARO
	printf ("  raw pres    %ld\n", ao_sample_pres);
#endif
#if HAS_ACCEL
	printf ("  raw accel   %d\n", ao_sample_accel);
#endif
#if HAS_BARO
	printf ("  ground pres %ld\n", ao_ground_pres);
	printf ("  ground alt  %ld\n", ao_ground_height);
#endif
#if HAS_ACCEL
	printf ("  raw accel   %d\n", ao_sample_accel);
	printf ("  groundaccel %d\n", ao_ground_accel);
	printf ("  accel_2g    %d\n", ao_accel_2g);
#endif

#if HAS_BARO
	printf ("  alt         %ld\n", ao_sample_alt);
	printf ("  height      %ld\n", ao_sample_height);
#endif

#if HAS_ACCEL
	printf ("  accel       %d.%02d\n", int_part(accel), frac_part(accel));
#endif


	printf ("kalman:\n");
	printf ("  height      %ld\n", ao_height);
	printf ("  speed       %d.%02d\n", int_part(ao_speed), frac_part(ao_speed));
	printf ("  accel       %d.%02d\n", int_part(ao_accel), frac_part(ao_accel));
	printf ("  max_height  %ld\n", ao_max_height);
	printf ("  avg_height  %ld\n", ao_avg_height);
	printf ("  error_h     %ld\n", ao_error_h);
#if !HAS_ACCEL
	printf ("  error_avg   %d\n", ao_error_h_sq_avg);
#endif
}

static void
ao_gyro_test(void)
{
	ao_flight_state = ao_flight_test;
	ao_getchar();
	ao_flight_state = ao_flight_idle;
}

uint8_t ao_orient_test;

static void
ao_orient_test_select(void)
{
	ao_orient_test = !ao_orient_test;
	printf("orient test %d\n", ao_orient_test);
}

const struct ao_cmds ao_flight_cmds[] = {
	{ ao_flight_dump, 	"F\0Dump flight status" },
	{ ao_gyro_test,		"G\0Test gyro code" },
	{ ao_orient_test_select,"O\0Test orientation code" },
	{ 0, NULL },
};
#endif

static struct ao_task	flight_task;

void
ao_flight_init(void)
{
	ao_flight_state = ao_flight_startup;
#if HAS_FLIGHT_DEBUG
	ao_cmd_register(&ao_flight_cmds[0]);
#endif
	ao_add_task(&flight_task, ao_flight, "flight");
}
