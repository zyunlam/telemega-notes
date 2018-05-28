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

#include <ao.h>
#include <ao_lco.h>
#include <ao_event.h>
#include <ao_seven_segment.h>
#include <ao_quadrature.h>
#include <ao_lco_func.h>
#include <ao_radio_cmac.h>
#include <ao_adc_single.h>

#define DEBUG	1

#if DEBUG
static uint8_t	ao_lco_debug;
#define PRINTD(...) do { if (!ao_lco_debug) break; printf ("\r%5u %s: ", ao_tick_count, __func__); printf(__VA_ARGS__); flush(); } while(0)
#else
#define PRINTD(...) 
#endif

#define AO_LCO_PAD_DIGIT	0
#define AO_LCO_BOX_DIGIT_1	1
#define AO_LCO_BOX_DIGIT_10	2

#define AO_LCO_DRAG_RACE_START_TIME	AO_SEC_TO_TICKS(5)
#define AO_LCO_DRAG_RACE_STOP_TIME	AO_SEC_TO_TICKS(2)

#define AO_LCO_VALID_LAST	1
#define AO_LCO_VALID_EVER	2

static uint8_t	ao_lco_min_box, ao_lco_max_box;
static uint8_t	ao_lco_selected[AO_PAD_MAX_BOXES];
static uint8_t	ao_lco_valid[AO_PAD_MAX_BOXES];
static uint8_t	ao_lco_channels[AO_PAD_MAX_BOXES];
static uint16_t	ao_lco_tick_offset[AO_PAD_MAX_BOXES];

/* UI values */
static uint8_t	ao_lco_armed;
static uint8_t	ao_lco_firing;
static uint8_t	ao_lco_drag_race;
static uint8_t	ao_lco_pad;
static int16_t	ao_lco_box;
static uint8_t	ao_lco_select_mode;
#define AO_LCO_SELECT_PAD	0
#define AO_LCO_SELECT_BOX	1

static struct ao_pad_query	ao_pad_query;

static uint8_t	ao_lco_display_mutex;

static void
ao_lco_set_pad(uint8_t pad)
{
	ao_mutex_get(&ao_lco_display_mutex);
	ao_seven_segment_set(AO_LCO_PAD_DIGIT, pad | (ao_lco_drag_race << 4));
	ao_mutex_put(&ao_lco_display_mutex);
}

#define SEVEN_SEGMENT_d		((0 << 0) |	\
				 (0 << 1) |	\
				 (1 << 2) |	\
				 (1 << 3) |	\
				 (1 << 4) |	\
				 (1 << 5) |	\
				 (1 << 6))


#define SEVEN_SEGMENT_r		((0 << 0) |	\
				 (0 << 1) |	\
				 (0 << 2) |	\
				 (1 << 3) |	\
				 (1 << 4) |	\
				 (0 << 5) |	\
				 (0 << 6))

static void
ao_lco_set_box(uint16_t box)
{
	ao_mutex_get(&ao_lco_display_mutex);
	ao_seven_segment_set(AO_LCO_BOX_DIGIT_1, box % 10 | (ao_lco_drag_race << 4));
	ao_seven_segment_set(AO_LCO_BOX_DIGIT_10, box / 10 | (ao_lco_drag_race << 4));
	ao_mutex_put(&ao_lco_display_mutex);
}

static void
ao_lco_set_voltage(uint16_t decivolts)
{
	uint8_t	tens, ones, tenths;

	PRINTD("voltage %d\n", decivolts);
	tenths = decivolts % 10;
	ones = (decivolts / 10) % 10;
	tens = (decivolts / 100) % 10;
	ao_mutex_get(&ao_lco_display_mutex);
	ao_seven_segment_set(AO_LCO_PAD_DIGIT, tenths);
	ao_seven_segment_set(AO_LCO_BOX_DIGIT_1, ones | 0x10);
	ao_seven_segment_set(AO_LCO_BOX_DIGIT_10, tens);
	ao_mutex_put(&ao_lco_display_mutex);
}

static void
ao_lco_set_display(void)
{
	if (ao_lco_pad == 0) {
		ao_lco_set_voltage(ao_pad_query.battery);
	} else {
		ao_lco_set_pad(ao_lco_pad);
		ao_lco_set_box(ao_lco_box);
	}
}

#define MASK_SIZE(n)	(((n) + 7) >> 3)
#define MASK_ID(n)	((n) >> 3)
#define MASK_SHIFT(n)	((n) & 7)

static uint8_t	ao_lco_box_mask[MASK_SIZE(AO_PAD_MAX_BOXES)];

static uint8_t
ao_lco_box_present(uint16_t box)
{
	if (box >= AO_PAD_MAX_BOXES)
		return 0;
	return (ao_lco_box_mask[MASK_ID(box)] >> MASK_SHIFT(box)) & 1;
}

static uint8_t
ao_lco_pad_present(uint8_t box, uint8_t pad)
{
	/* voltage measurement is always valid */
	if (pad == 0)
		return 1;
	if (!ao_lco_channels[box])
		return 0;
	if (pad > AO_PAD_MAX_CHANNELS)
		return 0;
	return (ao_lco_channels[box] >> (pad - 1)) & 1;
}

static uint8_t
ao_lco_pad_first(uint8_t box)
{
	uint8_t	pad;

	for (pad = 1; pad <= AO_PAD_MAX_CHANNELS; pad++)
		if (ao_lco_pad_present(box, pad))
			return pad;
	return 0;
}

static void
ao_lco_set_select(void)
{
	if (ao_lco_armed) {
		ao_led_off(AO_LED_PAD);
		ao_led_off(AO_LED_BOX);
	} else {
		switch (ao_lco_select_mode) {
		case AO_LCO_SELECT_PAD:
			ao_led_off(AO_LED_BOX);
			ao_led_on(AO_LED_PAD);
			break;
		case AO_LCO_SELECT_BOX:
			ao_led_off(AO_LED_PAD);
			ao_led_on(AO_LED_BOX);
			break;
		default:
			break;
		}
	}
}

static struct ao_task	ao_lco_drag_task;
static uint8_t		ao_lco_drag_active;
static uint8_t		ao_lco_drag_beep_count;
static uint8_t		ao_lco_drag_beep_on;
static uint16_t		ao_lco_drag_beep_time;
static uint16_t		ao_lco_drag_warn_time;

#define AO_LCO_DRAG_BEEP_TIME	AO_MS_TO_TICKS(50)
#define AO_LCO_DRAG_WARN_TIME	AO_SEC_TO_TICKS(5)

static void
ao_lco_drag_beep_start(void)
{
	ao_beep(AO_BEEP_HIGH);
	PRINTD("beep start\n");
	ao_lco_drag_beep_on = 1;
	ao_lco_drag_beep_time = ao_time() + AO_LCO_DRAG_BEEP_TIME;
}

static void
ao_lco_drag_beep_stop(void)
{
	ao_beep(0);
	PRINTD("beep stop\n");
	ao_lco_drag_beep_on = 0;
	if (ao_lco_drag_beep_count) {
		--ao_lco_drag_beep_count;
		if (ao_lco_drag_beep_count)
			ao_lco_drag_beep_time = ao_time() + AO_LCO_DRAG_BEEP_TIME;
	}
}

static void
ao_lco_drag_beep(uint8_t beeps)
{
	PRINTD("beep %d\n", beeps);
	if (!ao_lco_drag_beep_count)
		ao_lco_drag_beep_start();
	ao_lco_drag_beep_count += beeps;
}

static uint16_t
ao_lco_drag_beep_check(uint16_t now, uint16_t delay)
{
	PRINTD("beep check count %d delta %d\n",
	       ao_lco_drag_beep_count,
	       (int16_t) (now - ao_lco_drag_beep_time));
	if (ao_lco_drag_beep_count) {
		if ((int16_t) (now - ao_lco_drag_beep_time) >= 0) {
			if (ao_lco_drag_beep_on)
				ao_lco_drag_beep_stop();
			else
				ao_lco_drag_beep_start();
		}
	}

	if (ao_lco_drag_beep_count) {
		if (delay > AO_LCO_DRAG_BEEP_TIME)
			delay = AO_LCO_DRAG_BEEP_TIME;
	}
	return delay;
}

static void
ao_lco_drag_enable(void)
{
	PRINTD("Drag enable\n");
	ao_lco_drag_race = 1;
	memset(ao_lco_selected, 0, sizeof (ao_lco_selected));
	ao_led_on(AO_LED_DRAG);
	ao_lco_drag_beep(5);
	ao_lco_set_display();
}

static void
ao_lco_drag_disable(void)
{
	PRINTD("Drag disable\n");
	ao_lco_drag_race = 0;
	ao_led_off(AO_LED_DRAG);
	memset(ao_lco_selected, 0, sizeof (ao_lco_selected));
	ao_lco_drag_beep(2);
	ao_lco_set_display();
}

static uint16_t
ao_lco_drag_warn_check(uint16_t now, uint16_t delay)
{
	uint16_t	warn_delay = ~0;

	if (ao_lco_drag_race) {
		if ((int16_t) (now - ao_lco_drag_warn_time) >= 0) {
			ao_lco_drag_beep(1);
			ao_lco_drag_warn_time = now + AO_LCO_DRAG_WARN_TIME;
		}
		warn_delay = ao_lco_drag_warn_time - now;
	}
	if (delay > warn_delay)
		delay = warn_delay;
	return delay;
}

static void
ao_lco_drag_monitor(void)
{
	uint16_t	delay = ~0;
	uint16_t	now;

	for (;;) {
		PRINTD("Drag monitor active %d delay %d\n", ao_lco_drag_active, delay);
		if (delay == (uint16_t) ~0)
			ao_sleep(&ao_lco_drag_active);
		else
			ao_sleep_for(&ao_lco_drag_active, delay);

		delay = ~0;
		if (!ao_lco_drag_active)
			continue;

		now = ao_time();
		delay = ao_lco_drag_warn_check(now, delay);
		delay = ao_lco_drag_beep_check(now, delay);

		/* check to see if there's anything left to do here */
		if (!ao_lco_drag_race && !ao_lco_drag_beep_count) {
			delay = ~0;
			ao_lco_drag_active = 0;
		}
	}
}

static void
ao_lco_input(void)
{
	static struct ao_event	event;
	int8_t		dir, new_pad;
	int16_t		new_box;

	ao_beep_for(AO_BEEP_MID, AO_MS_TO_TICKS(200));
	for (;;) {
		ao_event_get(&event);
		PRINTD("event type %d unit %d value %d\n",
		       event.type, event.unit, event.value);
		switch (event.type) {
		case AO_EVENT_QUADRATURE:
			switch (event.unit) {
			case AO_QUADRATURE_SELECT:
				if (!ao_lco_armed) {
					switch (ao_lco_select_mode) {
					case AO_LCO_SELECT_PAD:
						dir = (int8_t) event.value;
						new_pad = ao_lco_pad;
						do {
							new_pad += dir;
							if (new_pad > AO_PAD_MAX_CHANNELS)
								new_pad = 0;
							if (new_pad < 0)
								new_pad = AO_PAD_MAX_CHANNELS;
							if (new_pad == ao_lco_pad)
								break;
						} while (!ao_lco_pad_present(ao_lco_box, new_pad));
						if (new_pad != ao_lco_pad) {
							ao_lco_pad = new_pad;
							ao_lco_set_display();
						}
						break;
					case AO_LCO_SELECT_BOX:
						dir = (int8_t) event.value;
						new_box = ao_lco_box;
						do {
							new_box += dir;
							if (new_box > ao_lco_max_box)
								new_box = ao_lco_min_box;
							else if (new_box < ao_lco_min_box)
								new_box = ao_lco_max_box;
							if (new_box == ao_lco_box)
								break;
						} while (!ao_lco_box_present(new_box));
						if (ao_lco_box != new_box) {
							ao_lco_box = new_box;
							ao_lco_pad = 1;
							ao_lco_channels[ao_lco_box] = 0;
							ao_lco_set_display();
						}
						break;
					default:
						break;
					}
				}
				break;
			}
			break;
		case AO_EVENT_BUTTON:
			switch (event.unit) {
			case AO_BUTTON_ARM:
				ao_lco_armed = event.value;
				PRINTD("Armed %d\n", ao_lco_armed);
				if (ao_lco_armed) {
					if (ao_lco_drag_race) {
						uint8_t	box;

						for (box = ao_lco_min_box; box <= ao_lco_max_box; box++)
							if (ao_lco_selected[box])
								break;
						if (box > ao_lco_max_box)
							ao_lco_armed = 0;
					} else {
						memset(ao_lco_selected, 0, sizeof (ao_lco_selected));
						if (ao_lco_pad != 0)
							ao_lco_selected[ao_lco_box] = (1 << (ao_lco_pad - 1));
						else
							ao_lco_armed = 0;
					}
				}
				ao_lco_set_select();
				ao_wakeup(&ao_lco_armed);
				break;
			case AO_BUTTON_FIRE:
				if (ao_lco_armed) {
					ao_lco_firing = event.value;
					PRINTD("Firing %d\n", ao_lco_firing);
					ao_wakeup(&ao_lco_armed);
				}
				break;
			case AO_BUTTON_DRAG_SELECT:
				if (event.value && ao_lco_drag_race) {
					if (ao_lco_pad != 0) {
						ao_lco_selected[ao_lco_box] ^= (1 << (ao_lco_pad - 1));
						PRINTD("Toggle box %d pad %d (pads now %x) to drag race\n",
						       ao_lco_pad, ao_lco_box, ao_lco_selected[ao_lco_box]);
						ao_lco_drag_beep(ao_lco_pad);
					}
				}
				break;
			case AO_BUTTON_DRAG_MODE:
				if (event.value)
					ao_lco_drag_enable();
				else
					ao_lco_drag_disable();
				break;
			case AO_BUTTON_ENCODER_SELECT:
				if (event.value) {
					if (!ao_lco_armed) {
						ao_lco_select_mode = 1 - ao_lco_select_mode;
						ao_lco_set_select();
					}
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
ao_lco_get_channels(uint8_t box, struct ao_pad_query *query)
{
	int8_t			r;

	r = ao_lco_query(box, query, &ao_lco_tick_offset[box]);
	if (r == AO_RADIO_CMAC_OK) {
		ao_lco_channels[box] = query->channels;
		ao_lco_valid[box] = AO_LCO_VALID_LAST | AO_LCO_VALID_EVER;
	} else
		ao_lco_valid[box] &= ~AO_LCO_VALID_LAST;
	PRINTD("ao_lco_get_channels(%d) rssi %d valid %d ret %d offset %d\n", box, ao_radio_cmac_rssi, ao_lco_valid[box], r, ao_lco_tick_offset[box]);
	ao_wakeup(&ao_pad_query);
	return ao_lco_valid[box];
}

static void
ao_lco_update(void)
{
	uint8_t	previous_valid = ao_lco_valid[ao_lco_box];

	if (ao_lco_get_channels(ao_lco_box, &ao_pad_query) & AO_LCO_VALID_LAST) {
		if (!(previous_valid & AO_LCO_VALID_EVER)) {
			if (ao_lco_pad != 0)
				ao_lco_pad = ao_lco_pad_first(ao_lco_box);
			ao_lco_set_display();
		}
		if (ao_lco_pad == 0)
			ao_lco_set_display();
	}
}

static void
ao_lco_box_reset_present(void)
{
	ao_lco_min_box = 0xff;
	ao_lco_max_box = 0x00;
	memset(ao_lco_box_mask, 0, sizeof (ao_lco_box_mask));
}

static void
ao_lco_box_set_present(uint8_t box)
{
	if (box < ao_lco_min_box)
		ao_lco_min_box = box;
	if (box > ao_lco_max_box)
		ao_lco_max_box = box;
	if (box >= AO_PAD_MAX_BOXES)
		return;
	ao_lco_box_mask[MASK_ID(box)] |= 1 << MASK_SHIFT(box);
}

static void
ao_lco_search(void)
{
	int8_t		r;
	int8_t		try;
	uint8_t		box;
	uint8_t		boxes = 0;

	ao_lco_box_reset_present();
	ao_lco_set_pad(0);
	for (box = 0; box < AO_PAD_MAX_BOXES; box++) {
		if ((box % 10) == 0)
			ao_lco_set_box(box);
		for (try = 0; try < 3; try++) {
			ao_lco_tick_offset[box] = 0;
			r = ao_lco_query(box, &ao_pad_query, &ao_lco_tick_offset[box]);
			PRINTD("box %d result %d offset %d\n", box, r, ao_lco_tick_offset[box]);
			if (r == AO_RADIO_CMAC_OK) {
				++boxes;
				ao_lco_box_set_present(box);
				ao_lco_set_pad(boxes % 10);
				ao_delay(AO_MS_TO_TICKS(30));
				break;
			}
		}
	}
	if (ao_lco_min_box <= ao_lco_max_box)
		ao_lco_box = ao_lco_min_box;
	else
		ao_lco_min_box = ao_lco_max_box = ao_lco_box = 0;
	memset(ao_lco_valid, 0, sizeof (ao_lco_valid));
	memset(ao_lco_channels, 0, sizeof (ao_lco_channels));
	ao_lco_pad = 1;
	ao_lco_set_display();
}

static void
ao_lco_igniter_status(void)
{
	uint8_t		c;
	uint8_t		t = 0;

	for (;;) {
		ao_sleep(&ao_pad_query);
		PRINTD("RSSI %d VALID %d\n", ao_radio_cmac_rssi, ao_lco_valid[ao_lco_box]);
		if (!(ao_lco_valid[ao_lco_box] & AO_LCO_VALID_LAST)) {
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

			if (ao_lco_drag_race && (ao_lco_selected[ao_lco_box] & (1 << c))) {
				uint8_t	on = 0;
				if (status == AO_PAD_IGNITER_STATUS_GOOD_IGNITER_RELAY_OPEN) {
					if (t)
						on = 1;
				} else {
					if (t == 1)
						on = 1;
				}
				if (on)
					ao_led_on(continuity_led[c]);
				else
					ao_led_off(continuity_led[c]);
			} else {
				if (status == AO_PAD_IGNITER_STATUS_GOOD_IGNITER_RELAY_OPEN)
					ao_led_on(continuity_led[c]);
				else
					ao_led_off(continuity_led[c]);
			}
		}
		t = (t + 1) & 3;
	}
}

static void
ao_lco_arm_warn(void)
{
	for (;;) {
		while (!ao_lco_armed) {
			ao_led_off(AO_LED_FIRE);
			ao_sleep(&ao_lco_armed);
		}
		ao_led_on(AO_LED_FIRE);
		ao_beep_for(AO_BEEP_MID, AO_MS_TO_TICKS(200));
		ao_delay(AO_MS_TO_TICKS(200));
	}
}

static void
ao_lco_batt_voltage(void)
{
	struct ao_adc	packet;
	int16_t		decivolt;

	ao_adc_single_get(&packet);
	decivolt = ao_battery_decivolt(packet.v_batt);
	ao_lco_set_voltage(decivolt);
	ao_delay(AO_MS_TO_TICKS(1000));
}

static struct ao_task ao_lco_input_task;
static struct ao_task ao_lco_monitor_task;
static struct ao_task ao_lco_arm_warn_task;
static struct ao_task ao_lco_igniter_status_task;

static void
ao_lco_monitor(void)
{
	uint16_t		delay;
	uint8_t			box;

	ao_lco_batt_voltage();
	ao_lco_search();
	ao_add_task(&ao_lco_input_task, ao_lco_input, "lco input");
	ao_add_task(&ao_lco_arm_warn_task, ao_lco_arm_warn, "lco arm warn");
	ao_add_task(&ao_lco_igniter_status_task, ao_lco_igniter_status, "lco igniter status");
	ao_add_task(&ao_lco_drag_task, ao_lco_drag_monitor, "drag race");
	for (;;) {
		PRINTD("monitor armed %d firing %d\n",
		       ao_lco_armed, ao_lco_firing);

		if (ao_lco_armed && ao_lco_firing) {
			ao_lco_ignite(AO_PAD_FIRE);
		} else {
			ao_lco_update();
			if (ao_lco_armed) {
				for (box = ao_lco_min_box; box <= ao_lco_max_box; box++) {
					if (ao_lco_selected[box]) {
						PRINTD("Arming box %d pads %x\n",
						       box, ao_lco_selected[box]);
						if (ao_lco_valid[box] & AO_LCO_VALID_EVER) {
							ao_lco_arm(box, ao_lco_selected[box], ao_lco_tick_offset[box]);
							ao_delay(AO_MS_TO_TICKS(10));
						}
					}
				}
			}
		}
		if (ao_lco_armed && ao_lco_firing)
			delay = AO_MS_TO_TICKS(100);
		else
			delay = AO_SEC_TO_TICKS(1);
		ao_sleep_for(&ao_lco_armed, delay);
	}
}

#if DEBUG
void
ao_lco_set_debug(void)
{
	ao_cmd_decimal();
	if (ao_cmd_status == ao_cmd_success)
		ao_lco_debug = ao_cmd_lex_i != 0;
}

__code struct ao_cmds ao_lco_cmds[] = {
	{ ao_lco_set_debug,	"D <0 off, 1 on>\0Debug" },
	{ ao_lco_search,	"s\0Search for pad boxes" },
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
