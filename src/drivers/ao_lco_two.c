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
#include <ao_lco.h>
#include <ao_event.h>
#include <ao_lco_func.h>
#include <ao_radio_cmac.h>

#define DEBUG	1

#if DEBUG
static uint8_t	ao_lco_debug;
#define DEBUG_EVENT	1
#define DEBUG_STATUS	2
#define PRINTD(l, ...) do { if (!(ao_lco_debug & l)) break; printf ("\r%5u %s: ", ao_tick_count, __func__); printf(__VA_ARGS__); flush(); } while(0)
#else
#define PRINTD(l,...)
#endif

#define AO_LCO_VALID_LAST	1
#define AO_LCO_VALID_EVER	2

static uint8_t	ao_lco_suspended;
static uint8_t	ao_lco_selected;
static uint8_t	ao_lco_valid;
static uint8_t	ao_lco_channels;
static uint16_t	ao_lco_tick_offset;

/* UI values */
static uint8_t	ao_lco_armed;
static uint8_t	ao_lco_firing;

#define ao_lco_box	(ao_config.pad_box)

static struct ao_pad_query	ao_pad_query;

#define MASK_SIZE(n)	(((n) + 7) >> 3)
#define MASK_ID(n)	((n) >> 3)
#define MASK_SHIFT(n)	((n) & 7)

static void
ao_lco_set_armed(int pad, int armed)
{
	uint8_t	bit = (1 << pad);

	if (armed) {
		ao_lco_selected |= bit;
		ao_lco_armed |= bit;
	} else {
		ao_lco_selected &= ~bit;
		ao_lco_armed &= ~bit;
	}
	PRINTD(DEBUG_EVENT, "pad %d bit 0x%x armed %d ao_lco_selected 0x%x ao_lco_armed 0x%x\n",
	       pad, bit, armed, ao_lco_selected, ao_lco_armed);
	ao_wakeup(&ao_lco_armed);
}

static void
ao_lco_suspend(void)
{
	if (!ao_lco_suspended) {
		PRINTD(DEBUG_EVENT, "suspend\n");
		ao_lco_suspended = 1;
		ao_lco_selected = 0;
		ao_lco_armed = 0;
		ao_wakeup(&ao_pad_query);
	}
}

static void
ao_lco_wakeup(void)
{
	if (ao_lco_suspended) {
		ao_lco_suspended = 0;
		ao_wakeup(&ao_lco_suspended);
	}
}

static void
ao_lco_input(void)
{
	static struct ao_event	event;
	uint8_t	timeout;

	ao_config_get();
	for (;;) {
		if (ao_config.pad_idle && !ao_lco_suspended) {
			timeout = ao_event_get_for(&event, AO_SEC_TO_TICKS(ao_config.pad_idle));
			if (timeout) {
				ao_lco_suspend();
				continue;
			}
		} else {
			ao_event_get(&event);
		}
		ao_lco_wakeup();
		PRINTD(DEBUG_EVENT, "event type %d unit %d value %d\n",
		       event.type, event.unit, event.value);
		switch (event.type) {
		case AO_EVENT_BUTTON:
			switch (event.unit) {
			case AO_BUTTON_ARM_0:
				ao_lco_set_armed(0, event.value);
				break;
#if AO_BUTTON_ARM_NUM > 1
			case AO_BUTTON_ARM_1:
				ao_lco_set_armed(1, event.value);
				break;
#endif
			case AO_BUTTON_FIRE:
				if (ao_lco_armed) {
					ao_lco_firing = event.value;
					PRINTD(DEBUG_EVENT, "Firing %d\n", ao_lco_firing);
					ao_wakeup(&ao_lco_armed);
				}
				break;
			}
			break;
		}
	}
}

static AO_LED_TYPE	continuity_led[AO_LED_CONTINUITY_NUM] = {
#ifdef AO_LED_CONTINUITY_0
	AO_LED_CONTINUITY_0,
#endif
#ifdef AO_LED_CONTINUITY_1
	AO_LED_CONTINUITY_1,
#endif
#ifdef AO_LED_CONTINUITY_2
	AO_LED_CONTINUITY_2,
#endif
#ifdef AO_LED_CONTINUITY_3
	AO_LED_CONTINUITY_3,
#endif
#ifdef AO_LED_CONTINUITY_4
	AO_LED_CONTINUITY_4,
#endif
#ifdef AO_LED_CONTINUITY_5
	AO_LED_CONTINUITY_5,
#endif
#ifdef AO_LED_CONTINUITY_6
	AO_LED_CONTINUITY_6,
#endif
#ifdef AO_LED_CONTINUITY_7
	AO_LED_CONTINUITY_7,
#endif
};

static uint8_t
ao_lco_get_channels(void)
{
	int8_t			r;

	r = ao_lco_query(ao_lco_box, &ao_pad_query, &ao_lco_tick_offset);
	if (r == AO_RADIO_CMAC_OK) {
		ao_lco_channels = ao_pad_query.channels;
		ao_lco_valid = AO_LCO_VALID_LAST | AO_LCO_VALID_EVER;
	} else
		ao_lco_valid &= ~AO_LCO_VALID_LAST;
	PRINTD(DEBUG_STATUS, "ao_lco_get_channels() rssi %d valid %d ret %d offset %d\n", ao_radio_cmac_rssi, ao_lco_valid, r, ao_lco_tick_offset);
	ao_wakeup(&ao_pad_query);
	return ao_lco_valid;
}

static void
ao_lco_igniter_status(void)
{
	uint8_t		c;
	uint8_t		t = 0;

	for (;;) {
		ao_sleep(&ao_pad_query);
		while (ao_lco_suspended) {
			ao_led_off(AO_LED_GREEN|AO_LED_AMBER|AO_LED_RED|AO_LED_REMOTE_ARM);
			for (c = 0; c < AO_LED_CONTINUITY_NUM; c++)
				ao_led_off(continuity_led[c]);
			ao_sleep(&ao_lco_suspended);
		}
		PRINTD(DEBUG_STATUS, "RSSI %d VALID %d\n", ao_radio_cmac_rssi, ao_lco_valid);
		if (!(ao_lco_valid & AO_LCO_VALID_LAST)) {
			ao_led_on(AO_LED_RED);
			ao_led_off(AO_LED_GREEN|AO_LED_AMBER);
			continue;
		}
		if (ao_radio_cmac_rssi < -90) {
			ao_led_on(AO_LED_AMBER);
			ao_led_off(AO_LED_RED|AO_LED_GREEN);
		} else {
			ao_led_on(AO_LED_GREEN);
			ao_led_off(AO_LED_RED|AO_LED_AMBER);
		}
		if (ao_pad_query.arm_status)
			ao_led_on(AO_LED_REMOTE_ARM);
		else
			ao_led_off(AO_LED_REMOTE_ARM);

		for (c = 0; c < AO_LED_CONTINUITY_NUM; c++) {
			uint8_t	status;

			if (ao_pad_query.channels & (1 << c))
				status = ao_pad_query.igniter_status[c];
			else
				status = AO_PAD_IGNITER_STATUS_NO_IGNITER_RELAY_OPEN;
			PRINTD(DEBUG_STATUS, "\tchannel %d status %d\n", c, status);
			if (status == AO_PAD_IGNITER_STATUS_GOOD_IGNITER_RELAY_OPEN)
				ao_led_on(continuity_led[c]);
			else
				ao_led_off(continuity_led[c]);
		}
		t = 1-t;
	}
}

static void
ao_lco_arm_warn(void)
{
	int	i;
	for (;;) {
		while (ao_lco_suspended)
			ao_sleep(&ao_lco_suspended);
		while (!ao_lco_armed)
			ao_sleep(&ao_lco_armed);
		for (i = 0; i < ao_lco_armed; i++) {
			ao_beep_for(AO_BEEP_MID, AO_MS_TO_TICKS(100));
			ao_delay(AO_MS_TO_TICKS(100));
		}
		ao_delay(AO_MS_TO_TICKS(300));
	}
}

static struct ao_task ao_lco_input_task;
static struct ao_task ao_lco_monitor_task;
static struct ao_task ao_lco_arm_warn_task;
static struct ao_task ao_lco_igniter_status_task;

static void
ao_lco_monitor(void)
{
	uint16_t		delay;

	ao_add_task(&ao_lco_input_task, ao_lco_input, "lco input");
	ao_add_task(&ao_lco_arm_warn_task, ao_lco_arm_warn, "lco arm warn");
	ao_add_task(&ao_lco_igniter_status_task, ao_lco_igniter_status, "lco igniter status");
	ao_beep_for(AO_BEEP_MID, AO_MS_TO_TICKS(200));
	for (;;) {
		while (ao_lco_suspended)
			ao_sleep(&ao_lco_suspended);

		PRINTD(DEBUG_STATUS, "monitor armed %d firing %d\n",
		       ao_lco_armed, ao_lco_firing);

		if (ao_lco_armed && ao_lco_firing) {
			ao_lco_ignite();
		} else {
			ao_lco_get_channels();
			if (ao_lco_armed) {
				if (ao_lco_selected) {
					PRINTD(DEBUG_STATUS, "Arming pads %x\n",
					       ao_lco_selected);
					if (ao_lco_valid & AO_LCO_VALID_EVER) {
						ao_lco_arm(ao_lco_box, ao_lco_selected, ao_lco_tick_offset);
						ao_delay(AO_MS_TO_TICKS(10));
					}
				}
			}
		}
		if (ao_lco_armed && ao_lco_firing)
			delay = AO_MS_TO_TICKS(100);
		else {
			delay = AO_SEC_TO_TICKS(1);
		}
		ao_sleep_for(&ao_lco_armed, delay);
	}
}

#if DEBUG
void
ao_lco_set_debug(void)
{
	ao_cmd_decimal();
	if (ao_cmd_status == ao_cmd_success)
		ao_lco_debug = ao_cmd_lex_i;
}

__code struct ao_cmds ao_lco_cmds[] = {
	{ ao_lco_set_debug,	"D <0 off, 1 on>\0Debug" },
	{ 0, NULL }
};
#endif

void
ao_lco_init(void)
{
	ao_add_task(&ao_lco_monitor_task, ao_lco_monitor, "lco monitor");
#if DEBUG
	ao_cmd_register(&ao_lco_cmds[0]);
#endif
}
