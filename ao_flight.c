/*
 * Copyright © 2009 Keith Packard <keithp@keithp.com>
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

#ifndef AO_FLIGHT_TEST
#include "ao.h"
#endif

/* Main flight thread. */

__pdata enum ao_flight_state	ao_flight_state;	/* current flight state */
__pdata uint16_t		ao_flight_tick;		/* time of last data */
__pdata uint16_t		ao_flight_prev_tick;	/* time of previous data */
__pdata int16_t			ao_flight_accel;	/* filtered acceleration */
__pdata int16_t			ao_flight_pres;		/* filtered pressure */
__pdata int16_t			ao_ground_pres;		/* startup pressure */
__pdata int16_t			ao_ground_accel;	/* startup acceleration */
__pdata int16_t			ao_min_pres;		/* minimum recorded pressure */
__pdata uint16_t		ao_launch_tick;		/* time of launch detect */
__pdata int16_t			ao_main_pres;		/* pressure to eject main */

/*
 * track min/max data over a long interval to detect
 * resting
 */
__pdata uint16_t		ao_interval_end;
__pdata int16_t			ao_interval_cur_min_accel;
__pdata int16_t			ao_interval_cur_max_accel;
__pdata int16_t			ao_interval_cur_min_pres;
__pdata int16_t			ao_interval_cur_max_pres;
__pdata int16_t			ao_interval_min_accel;
__pdata int16_t			ao_interval_max_accel;
__pdata int16_t			ao_interval_min_pres;
__pdata int16_t			ao_interval_max_pres;

__data uint8_t ao_flight_adc;
__pdata int16_t ao_raw_accel, ao_raw_accel_prev, ao_raw_pres;

/* Accelerometer calibration
 *
 * We're sampling the accelerometer through a resistor divider which
 * consists of 5k and 10k resistors. This multiplies the values by 2/3.
 * That goes into the cc1111 A/D converter, which is running at 11 bits
 * of precision with the bits in the MSB of the 16 bit value. Only positive
 * values are used, so values should range from 0-32752 for 0-3.3V. The
 * specs say we should see 40mV/g (uncalibrated), multiply by 2/3 for what
 * the A/D converter sees (26.67 mV/g). We should see 32752/3300 counts/mV,
 * for a final computation of:
 *
 * 26.67 mV/g * 32767/3300 counts/mV = 264.8 counts/g
 *
 * Zero g was measured at 16000 (we would expect 16384).
 * Note that this value is only require to tell if the
 * rocket is standing upright. Once that is determined,
 * the value of the accelerometer is averaged for 100 samples
 * to find the resting accelerometer value, which is used
 * for all further flight computations
 */

#define GRAVITY 9.80665
/* convert m/s to velocity count */
#define VEL_MPS_TO_COUNT(mps) ((int32_t) (((mps) / GRAVITY) * ACCEL_G * 100))

#define ACCEL_G		265
#define ACCEL_ZERO_G	16000
#define ACCEL_NOSE_UP	(ACCEL_G * 2 /3)
#define ACCEL_BOOST	ACCEL_G * 2
#define ACCEL_INT_LAND	(ACCEL_G / 10)
#define ACCEL_VEL_LAND	VEL_MPS_TO_COUNT(10)
#define ACCEL_VEL_MACH	VEL_MPS_TO_COUNT(200)
#define ACCEL_VEL_APOGEE	VEL_MPS_TO_COUNT(2)
#define ACCEL_VEL_MAIN	VEL_MPS_TO_COUNT(100)

/*
 * Barometer calibration
 *
 * We directly sample the barometer. The specs say:
 *
 * Pressure range: 15-115 kPa
 * Voltage at 115kPa: 2.82
 * Output scale: 27mV/kPa
 *
 * If we want to detect launch with the barometer, we need
 * a large enough bump to not be fooled by noise. At typical
 * launch elevations (0-2000m), a 200Pa pressure change cooresponds
 * to about a 20m elevation change. This is 5.4mV, or about 3LSB.
 * As all of our calculations are done in 16 bits, we'll actually see a change
 * of 16 times this though
 *
 * 27 mV/kPa * 32767 / 3300 counts/mV = 268.1 counts/kPa
 */

#define BARO_kPa	268
#define BARO_LAUNCH	(BARO_kPa / 5)	/* .2kPa, or about 20m */
#define BARO_APOGEE	(BARO_kPa / 10)	/* .1kPa, or about 10m */
#define BARO_COAST	(BARO_kPa * 5)  /* 5kpa, or about 500m */
#define BARO_MAIN	(BARO_kPa)	/* 1kPa, or about 100m */
#define BARO_INT_LAND	(BARO_kPa / 20)	/* .05kPa, or about 5m */
#define BARO_LAND	(BARO_kPa * 10)	/* 10kPa or about 1000m */

/* We also have a clock, which can be used to sanity check things in
 * case of other failures
 */

#define BOOST_TICKS_MAX	AO_SEC_TO_TICKS(15)

/* This value is scaled in a weird way. It's a running total of accelerometer
 * readings minus the ground accelerometer reading. That means it measures
 * velocity, and quite accurately too. As it gets updated 100 times a second,
 * it's scaled by 100
 */
__pdata int32_t	ao_flight_vel;
__pdata int32_t ao_min_vel;
__xdata int32_t ao_raw_accel_sum, ao_raw_pres_sum;

/* Landing is detected by getting constant readings from both pressure and accelerometer
 * for a fairly long time (AO_INTERVAL_TICKS)
 */
#define AO_INTERVAL_TICKS	AO_SEC_TO_TICKS(20)

#define abs(a)	((a) < 0 ? -(a) : (a))

void
ao_flight(void)
{
	__pdata static uint16_t	nsamples = 0;

	ao_flight_adc = ao_adc_head;
	ao_raw_accel_prev = 0;
	ao_raw_accel = 0;
	ao_raw_pres = 0;
	ao_flight_tick = 0;
	for (;;) {
		ao_sleep(&ao_adc_ring);
		while (ao_flight_adc != ao_adc_head) {
			__pdata uint8_t ticks;
			__pdata int16_t ao_vel_change;
			ao_flight_prev_tick = ao_flight_tick;

			/* Capture a sample */
			ao_raw_accel = ao_adc_ring[ao_flight_adc].accel;
			ao_raw_pres = ao_adc_ring[ao_flight_adc].pres;
			ao_flight_tick = ao_adc_ring[ao_flight_adc].tick;

			/* Update velocity
			 *
			 * The accelerometer is mounted so that
			 * acceleration yields negative values
			 * while deceleration yields positive values,
			 * so subtract instead of add.
			 */
			ticks = ao_flight_tick - ao_flight_prev_tick;
			ao_vel_change = (((ao_raw_accel + ao_raw_accel_prev) >> 1) - ao_ground_accel);
			ao_raw_accel_prev = ao_raw_accel;

			/* one is a common interval */
			if (ticks == 1)
				ao_flight_vel -= (int32_t) ao_vel_change;
			else
				ao_flight_vel -= (int32_t) ao_vel_change * (int32_t) ticks;

			ao_flight_adc = ao_adc_ring_next(ao_flight_adc);
		}
		ao_flight_accel -= ao_flight_accel >> 4;
		ao_flight_accel += ao_raw_accel >> 4;
		ao_flight_pres -= ao_flight_pres >> 4;
		ao_flight_pres += ao_raw_pres >> 4;

		if (ao_flight_pres < ao_min_pres)
			ao_min_pres = ao_flight_pres;
		if (ao_flight_vel >= 0) {
			if (ao_flight_vel < ao_min_vel)
			    ao_min_vel = ao_flight_vel;
		} else {
			if (-ao_flight_vel < ao_min_vel)
			    ao_min_vel = -ao_flight_vel;
		}

		switch (ao_flight_state) {
		case ao_flight_startup:

			/* startup state:
			 *
			 * Collect 1000 samples of acceleration and pressure
			 * data and average them to find the resting values
			 */
			if (nsamples < 1000) {
				ao_raw_accel_sum += ao_raw_accel;
				ao_raw_pres_sum += ao_raw_pres;
				++nsamples;
				continue;
			}
			ao_ground_accel = (ao_raw_accel_sum / nsamples);
			ao_ground_pres = (ao_raw_pres_sum / nsamples);
			ao_min_pres = ao_ground_pres;
			ao_config_get();
			ao_main_pres = ao_altitude_to_pres(ao_pres_to_altitude(ao_ground_pres) + ao_config.main_deploy);
			ao_flight_vel = 0;
			ao_min_vel = 0;

			/* Go to launchpad state if the nose is pointing up */
			ao_config_get();
			if (ao_flight_accel < ao_config.accel_zero_g - ACCEL_NOSE_UP) {

				/* Disable the USB controller in flight mode
				 * to save power
				 */
				ao_usb_disable();

				/* Turn on telemetry system
				 */
				ao_rdf_set(1);
				ao_telemetry_set_interval(AO_TELEMETRY_INTERVAL_FLIGHT);

				ao_flight_state = ao_flight_launchpad;
				ao_wakeup(DATA_TO_XDATA(&ao_flight_state));
			} else {
				ao_flight_state = ao_flight_idle;

				/* Turn on the Green LED in idle mode
				 */
				ao_led_on(AO_LED_GREEN);
				ao_wakeup(DATA_TO_XDATA(&ao_flight_state));
			}
			/* signal successful initialization by turning off the LED */
			ao_led_off(AO_LED_RED);
			break;
		case ao_flight_launchpad:

			/* pad to boost:
			 *
			 * accelerometer: > 2g
			 *             OR
			 * barometer: > 20m vertical motion
			 *
			 * The accelerometer should always detect motion before
			 * the barometer, but we use both to make sure this
			 * transition is detected
			 */
			if (ao_flight_accel < ao_ground_accel - ACCEL_BOOST ||
			    ao_flight_pres < ao_ground_pres - BARO_LAUNCH)
			{
				ao_flight_state = ao_flight_boost;
				ao_launch_tick = ao_flight_tick;

				/* start logging data */
				ao_log_start();

				/* disable RDF beacon */
				ao_rdf_set(0);

				ao_wakeup(DATA_TO_XDATA(&ao_flight_state));
				break;
			}
			break;
		case ao_flight_boost:

			/* boost to coast:
			 *
			 * accelerometer: start to fall at > 1/4 G
			 *              OR
			 * time: boost for more than 15 seconds
			 *
			 * Detects motor burn out by the switch from acceleration to
			 * deceleration, or by waiting until the maximum burn duration
			 * (15 seconds) has past.
			 */
			if (ao_flight_accel > ao_ground_accel + (ACCEL_G >> 2) ||
			    (int16_t) (ao_flight_tick - ao_launch_tick) > BOOST_TICKS_MAX)
			{
				ao_flight_state = ao_flight_coast;
				ao_wakeup(DATA_TO_XDATA(&ao_flight_state));
				break;
			}
			break;
		case ao_flight_coast:

			/* coast to apogee detect:
			 *
			 * accelerometer: integrated velocity < 200 m/s
			 *               OR
			 * barometer: fall at least 500m from max altitude
			 *
			 * This extra state is required to avoid mis-detecting
			 * apogee due to mach transitions.
			 *
			 * XXX this is essentially a single-detector test
			 * as the 500m altitude change would likely result
			 * in a loss of the rocket. More data on precisely
			 * how big a pressure change the mach transition
			 * generates would be useful here.
			 */
			if (ao_flight_vel < ACCEL_VEL_MACH ||
			    ao_flight_pres > ao_min_pres + BARO_COAST)
			{
				/* set min velocity to current velocity for
				 * apogee detect
				 */
				ao_min_vel = ao_flight_vel;
				ao_flight_state = ao_flight_apogee;
				ao_wakeup(DATA_TO_XDATA(&ao_flight_state));
			}
			break;
		case ao_flight_apogee:

			/* apogee detect to drogue deploy:
			 *
			 * accelerometer: abs(velocity) > min_velocity + 2m/s
			 *               OR
			 * barometer: fall at least 10m
			 *
			 * If the barometer saturates because the flight
			 * goes over its measuring range (about 53k'),
			 * requiring a 10m fall will avoid prematurely
			 * detecting apogee; the accelerometer will take
			 * over in that case and the integrated velocity
			 * measurement should suffice to find apogee
			 */
			if (abs(ao_flight_vel) > ao_min_vel + ACCEL_VEL_APOGEE ||
			    ao_flight_pres > ao_min_pres + BARO_APOGEE)
			{
				/* ignite the drogue charge */
				ao_ignite(ao_igniter_drogue);

				/* slow down the telemetry system */
				ao_telemetry_set_interval(AO_TELEMETRY_INTERVAL_RECOVER);

				/* slow down the ADC sample rate */
				ao_timer_set_adc_interval(10);

				/* Enable RDF beacon */
				ao_rdf_set(1);

				ao_flight_state = ao_flight_drogue;
				ao_wakeup(DATA_TO_XDATA(&ao_flight_state));
			}
			/*
			 * Start recording min/max accel and pres for a while
			 * to figure out when the rocket has landed
			 */
			/* Set the 'last' limits to max range to prevent
			 * early resting detection
			 */
			ao_interval_min_accel = 0;
			ao_interval_max_accel = 0x7fff;
			ao_interval_min_pres = 0;
			ao_interval_max_pres = 0x7fff;

			/* initialize interval values */
			ao_interval_end = ao_flight_tick + AO_INTERVAL_TICKS;

			ao_interval_cur_min_pres = ao_interval_cur_max_pres = ao_flight_pres;
			ao_interval_cur_min_accel = ao_interval_cur_max_accel = ao_flight_accel;

			break;
		case ao_flight_drogue:

			/* drogue to main deploy:
			 *
			 * accelerometer: abs(velocity) > 100m/s (in case the drogue failed)
			 *               OR
			 * barometer: reach main deploy altitude
			 */
			if (ao_flight_vel < -ACCEL_VEL_MAIN ||
			    ao_flight_vel > ACCEL_VEL_MAIN ||
			    ao_flight_pres >= ao_main_pres)
			{
				ao_ignite(ao_igniter_main);
				ao_flight_state = ao_flight_main;
				ao_wakeup(DATA_TO_XDATA(&ao_flight_state));
			}

			/* fall through... */
		case ao_flight_main:

			/* drogue/main to land:
			 *
			 * accelerometer: value stable and velocity less than 10m/s
			 *                           OR
			 * barometer: altitude stable and within 1000m of the launch altitude
			 */

			if (ao_flight_pres < ao_interval_cur_min_pres)
				ao_interval_cur_min_pres = ao_flight_pres;
			if (ao_flight_pres > ao_interval_cur_max_pres)
				ao_interval_cur_max_pres = ao_flight_pres;
			if (ao_flight_accel < ao_interval_cur_min_accel)
				ao_interval_cur_min_accel = ao_flight_accel;
			if (ao_flight_accel > ao_interval_cur_max_accel)
				ao_interval_cur_max_accel = ao_flight_accel;

			if ((int16_t) (ao_flight_tick - ao_interval_end) >= 0) {
				ao_interval_max_pres = ao_interval_cur_max_pres;
				ao_interval_min_pres = ao_interval_cur_min_pres;
				ao_interval_max_accel = ao_interval_cur_max_accel;
				ao_interval_min_accel = ao_interval_cur_min_accel;
				ao_interval_end = ao_flight_tick + AO_INTERVAL_TICKS;
				ao_interval_cur_min_pres = ao_interval_cur_max_pres = ao_flight_pres;
				ao_interval_cur_min_accel = ao_interval_cur_max_accel = ao_flight_accel;
			}

			if ((abs(ao_flight_vel) < ACCEL_VEL_LAND &&
			     (uint16_t) (ao_interval_max_accel - ao_interval_min_accel) < (uint16_t) ACCEL_INT_LAND) ||
			    (ao_flight_pres > ao_ground_pres - BARO_LAND &&
			     (uint16_t) (ao_interval_max_pres - ao_interval_min_pres) < (uint16_t) BARO_INT_LAND))
			{
				ao_flight_state = ao_flight_landed;

				/* turn off the ADC capture */
				ao_timer_set_adc_interval(0);

				ao_wakeup(DATA_TO_XDATA(&ao_flight_state));
			}
			break;
		case ao_flight_landed:
			break;
		}
	}
}

#define AO_ACCEL_COUNT_TO_MSS(count)	((count) / 27)
#define AO_VEL_COUNT_TO_MS(count)	((int16_t) ((count) / 2700))

static void
ao_flight_status(void)
{
	printf("STATE: %7s accel: %d speed: %d altitude: %d main: %d\n",
	       ao_state_names[ao_flight_state],
	       AO_ACCEL_COUNT_TO_MSS(ACCEL_ZERO_G - ao_flight_accel),
	       AO_VEL_COUNT_TO_MS(ao_flight_vel),
	       ao_pres_to_altitude(ao_flight_pres),
	       ao_pres_to_altitude(ao_main_pres));
}

static __xdata struct ao_task	flight_task;

__code struct ao_cmds ao_flight_cmds[] = {
	{ 'f', ao_flight_status,	"f                                  Display current flight state" },
	{ 0, ao_flight_status, NULL }
};

void
ao_flight_init(void)
{
	ao_flight_state = ao_flight_startup;
	ao_interval_min_accel = 0;
	ao_interval_max_accel = 0x7fff;
	ao_interval_min_pres = 0;
	ao_interval_max_pres = 0x7fff;
	ao_interval_end = AO_INTERVAL_TICKS;

	ao_add_task(&flight_task, ao_flight, "flight");
	ao_cmd_register(&ao_flight_cmds[0]);
}
