/*
 * Copyright © 2012 Keith Packard <keithp@keithp.com>
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

#include <ao.h>
#include <ao_pad.h>
#include <ao_74hc165.h>
#include <ao_radio_cmac.h>

static __xdata uint8_t ao_pad_ignite;
static __xdata struct ao_pad_command	command;
static __xdata struct ao_pad_query	query;
static __pdata uint8_t	ao_pad_armed;
static __pdata uint16_t	ao_pad_arm_time;
static __pdata uint8_t	ao_pad_box;
static __xdata uint8_t	ao_pad_disabled;
static __pdata uint16_t	ao_pad_packet_time;

#ifndef AO_PAD_RSSI_MINIMUM
#define AO_PAD_RSSI_MINIMUM	-90
#endif

#define DEBUG	1

#if DEBUG
static __pdata uint8_t	ao_pad_debug;
#define PRINTD(...) (ao_pad_debug ? (printf(__VA_ARGS__), 0) : 0)
#define FLUSHD()    (ao_pad_debug ? (flush(), 0) : 0)
#else
#define PRINTD(...)
#define FLUSHD()
#endif

static void
ao_siren(uint8_t v)
{
#ifdef AO_SIREN
	ao_gpio_set(AO_SIREN_PORT, AO_SIREN_PIN, AO_SIREN, v);
#else
#if HAS_BEEP
	ao_beep(v ? AO_BEEP_MID : 0);
#else
	(void) v;
#endif
#endif
}

static void
ao_strobe(uint8_t v)
{
#ifdef AO_STROBE
	ao_gpio_set(AO_STROBE_PORT, AO_STROBE_PIN, AO_STROBE, v);
#else
	(void) v;
#endif
}

static void
ao_pad_run(void)
{
	AO_PORT_TYPE	pins;

	for (;;) {
		while (!ao_pad_ignite)
			ao_sleep(&ao_pad_ignite);
		/*
		 * Actually set the pad bits
		 */
		pins = 0;
#if AO_PAD_NUM > 0
		if (ao_pad_ignite & (1 << 0))
			pins |= (1 << AO_PAD_PIN_0);
#endif
#if AO_PAD_NUM > 1
		if (ao_pad_ignite & (1 << 1))
			pins |= (1 << AO_PAD_PIN_1);
#endif
#if AO_PAD_NUM > 2
		if (ao_pad_ignite & (1 << 2))
			pins |= (1 << AO_PAD_PIN_2);
#endif
#if AO_PAD_NUM > 3
		if (ao_pad_ignite & (1 << 3))
			pins |= (1 << AO_PAD_PIN_3);
#endif
		PRINTD("ignite pins 0x%x\n", pins);
		ao_gpio_set_bits(AO_PAD_PORT, pins);
		while (ao_pad_ignite) {
			ao_pad_ignite = 0;

			ao_delay(AO_PAD_FIRE_TIME);
		}
		ao_gpio_clr_bits(AO_PAD_PORT, pins);
		PRINTD("turn off pins 0x%x\n", pins);
	}
}

#define AO_PAD_ARM_SIREN_INTERVAL	200

#ifndef AO_PYRO_R_PYRO_SENSE
#define AO_PYRO_R_PYRO_SENSE	100
#define AO_PYRO_R_SENSE_GND	27
#define AO_FIRE_R_POWER_FET	100
#define AO_FIRE_R_FET_SENSE	100
#define AO_FIRE_R_SENSE_GND	27
#endif

static void
ao_pad_monitor(void)
{
	uint8_t			c;
	uint8_t			sample;
	__pdata uint8_t		prev = 0, cur = 0;
	__pdata uint8_t		beeping = 0;
	__xdata volatile struct ao_data	*packet;
	__pdata uint16_t	arm_beep_time = 0;

	sample = ao_data_head;
	for (;;) {
		__pdata int16_t			pyro;
		ao_arch_critical(
			while (sample == ao_data_head)
				ao_sleep((void *) DATA_TO_XDATA(&ao_data_head));
			);


		packet = &ao_data_ring[sample];
		sample = ao_data_ring_next(sample);

		pyro = packet->adc.pyro;

#define VOLTS_TO_PYRO(x) ((int16_t) ((x) * ((1.0 * AO_PYRO_R_SENSE_GND) / \
					    (1.0 * (AO_PYRO_R_SENSE_GND + AO_PYRO_R_PYRO_SENSE)) / 3.3 * AO_ADC_MAX)))


#define VOLTS_TO_FIRE(x) ((int16_t) ((x) * ((1.0 * AO_FIRE_R_SENSE_GND) / \
					    (1.0 * (AO_FIRE_R_SENSE_GND + AO_FIRE_R_FET_SENSE)) / 3.3 * AO_ADC_MAX)))

		/* convert ADC value to voltage in tenths, then add .2 for the diode drop */
		query.battery = (packet->adc.batt + 96) / 192 + 2;
		cur = 0;
		if (pyro > VOLTS_TO_PYRO(10)) {
			query.arm_status = AO_PAD_ARM_STATUS_ARMED;
			cur |= AO_LED_ARMED;
#if AO_FIRE_R_POWER_FET
		} else if (pyro > VOLTS_TO_PYRO(5)) {
			if ((ao_time() % 100) < 50)
				cur |= AO_LED_ARMED;
			query.arm_status = AO_PAD_ARM_STATUS_UNKNOWN;
			arm_beep_time = 0;
#endif
		} else {
			query.arm_status = AO_PAD_ARM_STATUS_DISARMED;
			arm_beep_time = 0;
		}
		if ((ao_time() - ao_pad_packet_time) > AO_SEC_TO_TICKS(2))
			cur |= AO_LED_RED;
		else if (ao_radio_cmac_rssi < AO_PAD_RSSI_MINIMUM)
			cur |= AO_LED_AMBER;
		else
			cur |= AO_LED_GREEN;

		for (c = 0; c < AO_PAD_NUM; c++) {
			int16_t		sense = packet->adc.sense[c];
			uint8_t	status = AO_PAD_IGNITER_STATUS_UNKNOWN;

			/*
			 *	pyro is run through a divider, so pyro = v_pyro * 27 / 127 ~= v_pyro / 20
			 *	v_pyro = pyro * 127 / 27
			 *
			 *		v_pyro \
			 *	100k		igniter
			 *		output /
			 *	100k           \
			 *		sense   relay
			 *	27k            /
			 *		gnd ---
			 *
			 *		v_pyro \
			 *	200k		igniter
			 *		output /
			 *	200k           \
			 *		sense   relay
			 *	22k            /
			 *		gnd ---
			 *
			 *	If the relay is closed, then sense will be 0
			 *	If no igniter is present, then sense will be v_pyro * 27k/227k = pyro * 127 / 227 ~= pyro/2
			 *	If igniter is present, then sense will be v_pyro * 27k/127k ~= v_pyro / 20 = pyro
			 */

#if AO_FIRE_R_POWER_FET
			if (sense <= pyro / 8) {
				status = AO_PAD_IGNITER_STATUS_NO_IGNITER_RELAY_CLOSED;
				if ((ao_time() % 100) < 50)
					cur |= AO_LED_CONTINUITY(c);
			} else
			if (pyro / 8 * 3 <= sense && sense <= pyro / 8 * 5)
				status = AO_PAD_IGNITER_STATUS_NO_IGNITER_RELAY_OPEN;
			else if (pyro / 8 * 7 <= sense) {
				status = AO_PAD_IGNITER_STATUS_GOOD_IGNITER_RELAY_OPEN;
				cur |= AO_LED_CONTINUITY(c);
			}
#else
			if (sense >= pyro / 8 * 5) {
				status = AO_PAD_IGNITER_STATUS_GOOD_IGNITER_RELAY_OPEN;
				cur |= AO_LED_CONTINUITY(c);
			} else {
				status = AO_PAD_IGNITER_STATUS_NO_IGNITER_RELAY_OPEN;
			}
#endif
			query.igniter_status[c] = status;
		}
		if (cur != prev) {
			PRINTD("change leds from %02x to %02x\n",
			       prev, cur);
			FLUSHD();
			ao_led_set(cur);
			prev = cur;
		}

		if (ao_pad_armed && (int16_t) (ao_time() - ao_pad_arm_time) > AO_PAD_ARM_TIME)
			ao_pad_armed = 0;

		if (ao_pad_armed) {
			ao_strobe(1);
			ao_siren(1);
			beeping = 1;
		} else if (query.arm_status == AO_PAD_ARM_STATUS_ARMED && !beeping) {
			if (arm_beep_time == 0) {
				arm_beep_time = AO_PAD_ARM_SIREN_INTERVAL;
				beeping = 1;
				ao_siren(1);
			}
			--arm_beep_time;
		} else if (beeping) {
			beeping = 0;
			ao_siren(0);
			ao_strobe(0);
		}
	}
}

void
ao_pad_disable(void)
{
	if (!ao_pad_disabled) {
		ao_pad_disabled = 1;
		ao_radio_recv_abort();
	}
}

void
ao_pad_enable(void)
{
	ao_pad_disabled = 0;
	ao_wakeup (&ao_pad_disabled);
}

#if HAS_74HC165
static uint8_t
ao_pad_read_box(void)
{
	uint8_t		byte = ao_74hc165_read();
	uint8_t		h, l;

	h = byte >> 4;
	l = byte & 0xf;
	return h * 10 + l;
}
#endif

#if HAS_FIXED_PAD_BOX
#define ao_pad_read_box()	ao_config.pad_box
#endif

#ifdef PAD_BOX
#define ao_pad_read_box()	PAD_BOX
#endif

static void
ao_pad(void)
{
	int16_t	time_difference;
	int8_t	ret;

	ao_pad_box = 0;
	ao_led_set(0);
	for (;;) {
		FLUSHD();
		while (ao_pad_disabled)
			ao_sleep(&ao_pad_disabled);
		ret = ao_radio_cmac_recv(&command, sizeof (command), 0);
		PRINTD ("cmac_recv %d %d\n", ret, ao_radio_cmac_rssi);
		if (ret != AO_RADIO_CMAC_OK)
			continue;
		ao_pad_packet_time = ao_time();

		ao_pad_box = ao_pad_read_box();

		PRINTD ("tick %d box %d (me %d) cmd %d channels %02x\n",
			command.tick, command.box, ao_pad_box, command.cmd, command.channels);

		switch (command.cmd) {
		case AO_LAUNCH_ARM:
			if (command.box != ao_pad_box) {
				PRINTD ("box number mismatch\n");
				break;
			}

			if (command.channels & ~(AO_PAD_ALL_CHANNELS))
				break;

			time_difference = command.tick - ao_time();
			PRINTD ("arm tick %d local tick %d\n", command.tick, ao_time());
			if (time_difference < 0)
				time_difference = -time_difference;
			if (time_difference > 10) {
				PRINTD ("time difference too large %d\n", time_difference);
				break;
			}
			PRINTD ("armed\n");
			ao_pad_armed = command.channels;
			ao_pad_arm_time = ao_time();
			break;

		case AO_LAUNCH_QUERY:
			if (command.box != ao_pad_box) {
				PRINTD ("box number mismatch\n");
				break;
			}

			query.tick = ao_time();
			query.box = ao_pad_box;
			query.channels = AO_PAD_ALL_CHANNELS;
			query.armed = ao_pad_armed;
			PRINTD ("query tick %d box %d channels %02x arm %d arm_status %d igniter %d,%d,%d,%d\n",
				query.tick, query.box, query.channels, query.armed,
				query.arm_status,
				query.igniter_status[0],
				query.igniter_status[1],
				query.igniter_status[2],
				query.igniter_status[3]);
			ao_radio_cmac_send(&query, sizeof (query));
			break;
		case AO_LAUNCH_FIRE:
			if (!ao_pad_armed) {
				PRINTD ("not armed\n");
				break;
			}
			if ((uint16_t) (ao_time() - ao_pad_arm_time) > AO_SEC_TO_TICKS(20)) {
				PRINTD ("late pad arm_time %d time %d\n",
					ao_pad_arm_time, ao_time());
				break;
			}
			PRINTD ("ignite\n");
			ao_pad_ignite = ao_pad_armed;
			ao_pad_arm_time = ao_time();
			ao_wakeup(&ao_pad_ignite);
			break;
		}
	}
}

void
ao_pad_test(void)
{
	uint8_t	c;

	printf ("Arm switch: ");
	switch (query.arm_status) {
	case AO_PAD_ARM_STATUS_ARMED:
		printf ("Armed\n");
		break;
	case AO_PAD_ARM_STATUS_DISARMED:
		printf ("Disarmed\n");
		break;
	case AO_PAD_ARM_STATUS_UNKNOWN:
		printf ("Unknown\n");
		break;
	}

	for (c = 0; c < AO_PAD_NUM; c++) {
		printf ("Pad %d: ", c);
		switch (query.igniter_status[c]) {
		case AO_PAD_IGNITER_STATUS_NO_IGNITER_RELAY_CLOSED:	printf ("No igniter. Relay closed\n"); break;
		case AO_PAD_IGNITER_STATUS_NO_IGNITER_RELAY_OPEN:	printf ("No igniter. Relay open\n"); break;
		case AO_PAD_IGNITER_STATUS_GOOD_IGNITER_RELAY_OPEN:	printf ("Good igniter. Relay open\n"); break;
		case AO_PAD_IGNITER_STATUS_UNKNOWN:			printf ("Unknown\n"); break;
		}
	}
}

void
ao_pad_manual(void)
{
	uint8_t	ignite;
	int	repeat;
	ao_cmd_white();
	if (!ao_match_word("DoIt"))
		return;
	ao_cmd_decimal();
	if (ao_cmd_status != ao_cmd_success)
		return;
	ignite = 1 << ao_cmd_lex_i;
	ao_cmd_decimal();
	if (ao_cmd_status != ao_cmd_success) {
		repeat = 1;
		ao_cmd_status = ao_cmd_success;
	} else
		repeat = ao_cmd_lex_i;
	while (repeat-- > 0) {
		ao_pad_ignite = ignite;
		ao_wakeup(&ao_pad_ignite);
		ao_delay(AO_PAD_FIRE_TIME>>1);
	}
}

static __xdata struct ao_task ao_pad_task;
static __xdata struct ao_task ao_pad_ignite_task;
static __xdata struct ao_task ao_pad_monitor_task;

#if DEBUG
void
ao_pad_set_debug(void)
{
	ao_cmd_decimal();
	if (ao_cmd_status == ao_cmd_success)
		ao_pad_debug = ao_cmd_lex_i != 0;
}


static void
ao_pad_alarm_debug(void)
{
	uint8_t	which, value;
	ao_cmd_decimal();
	if (ao_cmd_status != ao_cmd_success)
		return;
	which = ao_cmd_lex_i;
	ao_cmd_decimal();
	if (ao_cmd_status != ao_cmd_success)
		return;
	value = ao_cmd_lex_i;
	printf ("Set %s to %d\n", which ? "siren" : "strobe", value);
	if (which)
		ao_siren(value);
	else
		ao_strobe(value);
}
#endif

__code struct ao_cmds ao_pad_cmds[] = {
	{ ao_pad_test,	"t\0Test pad continuity" },
	{ ao_pad_manual,	"i <key> <n>\0Fire igniter. <key> is doit with D&I" },
#if DEBUG
	{ ao_pad_set_debug,	"D <0 off, 1 on>\0Debug" },
	{ ao_pad_alarm_debug,	"S <0 strobe, 1 siren> <0 off, 1 on>\0Set alarm output" },
#endif
	{ 0, NULL }
};

void
ao_pad_init(void)
{
#if AO_PAD_NUM > 0
	ao_enable_output(AO_PAD_PORT, AO_PAD_PIN_0, AO_PAD_0, 0);
#endif
#if AO_PAD_NUM > 1
	ao_enable_output(AO_PAD_PORT, AO_PAD_PIN_1, AO_PAD_1, 0);
#endif
#if AO_PAD_NUM > 2
	ao_enable_output(AO_PAD_PORT, AO_PAD_PIN_2, AO_PAD_2, 0);
#endif
#if AO_PAD_NUM > 3
	ao_enable_output(AO_PAD_PORT, AO_PAD_PIN_3, AO_PAD_3, 0);
#endif
#ifdef AO_STROBE
	ao_enable_output(AO_STROBE_PORT, AO_STROBE_PIN, AO_STROBE, 0);
#endif
#ifdef AO_SIREN
	ao_enable_output(AO_SIREN_PORT, AO_SIREN_PIN, AO_SIREN, 0);
#endif
	ao_cmd_register(&ao_pad_cmds[0]);
	ao_add_task(&ao_pad_task, ao_pad, "pad listener");
	ao_add_task(&ao_pad_ignite_task, ao_pad_run, "pad igniter");
	ao_add_task(&ao_pad_monitor_task, ao_pad_monitor, "pad monitor");
}
