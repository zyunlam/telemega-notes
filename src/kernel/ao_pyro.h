/*
 * Copyright © 2012 Keith Packard <keithp@keithp.com>
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

#ifndef _AO_PYRO_H_
#define _AO_PYRO_H_

enum ao_pyro_flag {
	ao_pyro_none			= 0x00000000,

	ao_pyro_accel_less		= 0x00000001,
	ao_pyro_accel_greater		= 0x00000002,

	ao_pyro_speed_less		= 0x00000004,
	ao_pyro_speed_greater		= 0x00000008,

	ao_pyro_height_less		= 0x00000010,
	ao_pyro_height_greater		= 0x00000020,

	ao_pyro_orient_less		= 0x00000040,
	ao_pyro_orient_greater		= 0x00000080,

	ao_pyro_time_less		= 0x00000100,
	ao_pyro_time_greater		= 0x00000200,

	ao_pyro_ascending		= 0x00000400,
	ao_pyro_descending		= 0x00000800,

	ao_pyro_after_motor		= 0x00001000,

	ao_pyro_delay			= 0x00002000,

	ao_pyro_state_less		= 0x00004000,
	ao_pyro_state_greater_or_equal  = 0x00008000,
}
#ifdef __GNUC__
	__attribute__ ((packed))
#endif
	;

struct ao_pyro_1_24 {
	enum ao_pyro_flag	flags;
	int16_t			accel_less, accel_greater;
	int16_t			speed_less, speed_greater;
	int16_t			height_less, height_greater;
	int16_t			orient_less, orient_greater;
	int16_t			time_less, time_greater;
	int16_t			delay;
	uint8_t			state_less, state_greater_or_equal;
	int16_t			motor;
	uint16_t		delay_done;
	uint8_t			_unused;	/* was 'fired' */
};

struct ao_pyro {
	enum ao_pyro_flag	flags;
	int16_t			accel_less, accel_greater;
	int16_t			speed_less, speed_greater;
	int16_t			height_less, height_greater;
	int16_t			orient_less, orient_greater;
	int32_t			time_less, time_greater;
	int32_t			delay;
	uint8_t			state_less, state_greater_or_equal;
	int16_t			motor;
	uint32_t		_unused1;	/* was 'delay_done' */
	uint8_t			_unused2;	/* was 'fired' */
};

#define AO_PYRO_8_BIT_VALUE	(ao_pyro_state_less|ao_pyro_state_greater_or_equal)
#define AO_PYRO_32_BIT_VALUE	(ao_pyro_time_less|ao_pyro_time_greater|ao_pyro_delay)

extern uint8_t	ao_pyro_wakeup;

extern uint16_t	ao_pyro_fired;

void
ao_pyro_set(void);

void
ao_pyro_show(void);

void
ao_pyro_init(void);

void
ao_pyro_update_version(void);

void
ao_pyro_manual(uint8_t p);

enum ao_igniter_status
ao_pyro_status(uint8_t p);

void
ao_pyro_print_status(void);

#ifndef AO_PYRO_BATTERY_DIV_PLUS
#define AO_PYRO_BATTERY_DIV_PLUS AO_BATTERY_DIV_PLUS
#define AO_PYRO_BATTERY_DIV_MINUS AO_BATTERY_DIV_MINUS
#ifndef AO_SENSE_PBATT
#define AO_SENSE_PBATT(p) ((p)->adc.v_batt)
#endif
#else
#ifndef AO_SENSE_PBATT
#define AO_SENSE_PBATT(p) ((p)->adc.v_pbatt)
#endif
#endif

/*
 * dv = (sensor * (p+m) * ref_dv)/ (max * m)
 * value * (max * m) = (sensor * (p+m) * ref)
 * value * (max * m) / ((p+m) * ref) = sensor
 */

#define AO_DV_MUL(p,m) ((int32_t) AO_ADC_MAX * (m))
#define AO_DV_DIV(p,m) ((int32_t) AO_ADC_REFERENCE_DV * ((p) + (m)))
#define AO_DV_ADD(p,m) (AO_DV_DIV(p,m) / 2)

#define ao_decivolt_to_adc(dv, p, m) \
	((int16_t) (((int32_t) (dv) * AO_DV_MUL(p,m) + AO_DV_ADD(p,m)) / AO_DV_DIV(p,m)))

#define AO_IGNITER_CLOSED_DV	35
#define AO_IGNITER_OPEN_DV	10

#define AO_PYRO_BATTERY_GOOD_DV	38

#undef AO_IGNITER_OPEN
#undef AO_IGNITER_CLOSED

#define AO_IGNITER_OPEN ao_decivolt_to_adc(AO_IGNITER_OPEN_DV, AO_IGNITE_DIV_PLUS, AO_IGNITE_DIV_MINUS)
#define AO_IGNITER_CLOSED ao_decivolt_to_adc(AO_IGNITER_CLOSED_DV, AO_IGNITE_DIV_PLUS, AO_IGNITE_DIV_MINUS)

#define AO_PYRO_BATTERY_GOOD ao_decivolt_to_adc(AO_PYRO_BATTERY_GOOD_DV, AO_PYRO_BATTERY_DIV_PLUS, AO_PYRO_BATTERY_DIV_MINUS)

/* For devices measuring the pyro battery voltage, we want to use a
 * fraction of that. We'll use 15/16 of the battery voltage as a limit
 * For devices not measuring the pyro battery voltage, we'll use 3.5V
 * instead (this is just TeleMetrum, which permits external pyro
 * batteries but has not provision to measure the voltage)
 */

static inline int16_t
ao_igniter_closed_value(int16_t battery)
{
#if AO_PYRO_BATTERY_DIV_PLUS != AO_IGNITE_DIV_PLUS || AO_PYRO_BATTERY_DIV_MINUS != AO_IGNITE_DIV_MINUS
	(void) battery;
	return AO_IGNITER_CLOSED;
#else
	return (int16_t) (((int32_t) battery * 15) / 16);
#endif
}

static inline int16_t
ao_igniter_open_value(int16_t battery)
{
#if AO_PYRO_BATTERY_DIV_PLUS != AO_IGNITE_DIV_PLUS || AO_PYRO_BATTERY_DIV_MINUS != AO_IGNITE_DIV_MINUS
	(void) battery;
	return AO_IGNITER_OPEN;
#else
	return (int16_t) (((int32_t) battery * 1) / 8);
#endif
}

static inline enum ao_igniter_status
ao_igniter_check(int16_t value, int16_t battery)
{
	if (battery < AO_PYRO_BATTERY_GOOD)
		return ao_igniter_open;
	if (value < ao_igniter_open_value(battery))
		return ao_igniter_open;
	else if (value > ao_igniter_closed_value(battery))
		return ao_igniter_ready;
	else
		return ao_igniter_unknown;
}

#endif
