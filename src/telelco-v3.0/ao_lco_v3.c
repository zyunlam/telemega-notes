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
#include <ao_quadrature.h>
#include <ao_radio_cmac.h>
#include <ao_st7565.h>
#include <ao_adc_single.h>

#define WIDTH	AO_ST7565_WIDTH
#define HEIGHT	AO_ST7565_HEIGHT
#define STRIDE	AO_BITMAP_STRIDE(WIDTH)

static uint32_t	image[STRIDE * HEIGHT];

static struct ao_bitmap fb = {
	.base = image,
	.stride = STRIDE,
	.width = WIDTH,
	.height = HEIGHT,
	.damage = AO_BOX_INIT,
};

static const struct ao_transform logo_transform = {
	.x_scale = 48, .x_off = 2,
	.y_scale = 48, .y_off = 0,
};

#define BIG_FONT BitstreamVeraSans_Roman_58_font
#define VOLT_FONT BitstreamVeraSans_Roman_58_font
#define CONTRAST_FONT BitstreamVeraSans_Roman_58_font
#define SMALL_FONT BitstreamVeraSans_Roman_12_font
#define TINY_FONT BitstreamVeraSans_Roman_10_font
#define LOGO_FONT BenguiatGothicStd_Bold_26_font

#define LABEL_Y		(int16_t) (SMALL_FONT.ascent)
#define VALUE_Y		(int16_t) (LABEL_Y + BIG_FONT.ascent + 5)
#define BOX_X		2
#define PAD_X		90
#define BOX_LABEL_X	30
#define VOLT_LABEL_X	25
#define RSSI_LABEL_X	15
#define PAD_LABEL_X	95
#define SEP_X		(PAD_X - 8)
#define SCAN_X		(WIDTH - 100) / 2
#define SCAN_Y		50
#define SCAN_HEIGHT	3
#define FOUND_Y		63
#define FOUND_X		6
#define FOUND_WIDTH	(WIDTH - 6)
#define CONTRAST_LABEL_X	37
#define CONTRAST_WIDTH	100
#define CONTRAST_X	(WIDTH - CONTRAST_WIDTH) / 2
#define CONTRAST_Y	20
#define CONTRAST_HEIGHT	20

#define AO_LCO_DRAG_RACE_START_TIME	AO_SEC_TO_TICKS(5)
#define AO_LCO_DRAG_RACE_STOP_TIME	AO_SEC_TO_TICKS(2)

/* UI values */
static uint8_t	ao_lco_select_mode;
static uint8_t	ao_lco_event_debug;

#define PRINTE(...) do { if (!ao_lco_debug && !ao_lco_event_debug) break; printf ("\r%5lu %s: ", (unsigned long) ao_tick_count, __func__); printf(__VA_ARGS__); flush(); } while(0)
#define AO_LCO_SELECT_PAD	0
#define AO_LCO_SELECT_BOX	1

static uint8_t	ao_lco_display_mutex;

static void
_ao_lco_show_pad(uint8_t pad)
{
	char	str[5];

	snprintf(str, sizeof(str), "%d", pad);
	ao_text(&fb, &BIG_FONT, PAD_X, VALUE_Y, str, AO_BLACK, AO_COPY);
	ao_text(&fb, &SMALL_FONT, PAD_LABEL_X, LABEL_Y, "Pad", AO_BLACK, AO_COPY);
}

static void
_ao_lco_show_box(int16_t box)
{
	char	str[7];

	snprintf(str, sizeof(str), "%2d", box);
	ao_text(&fb, &BIG_FONT, BOX_X, VALUE_Y, str, AO_BLACK, AO_COPY);
	ao_text(&fb, &SMALL_FONT, BOX_LABEL_X, LABEL_Y, "Box", AO_BLACK, AO_COPY);
}

static void
_ao_lco_show_voltage(uint16_t decivolts, const char *label)
{
	char	str[7];

	PRINTD("voltage %d\n", decivolts);
	snprintf(str, sizeof(str), "%2d.%d", decivolts / 10, decivolts % 10);
	ao_text(&fb, &VOLT_FONT, BOX_X, VALUE_Y, str, AO_BLACK, AO_COPY);
	ao_text(&fb, &SMALL_FONT, VOLT_LABEL_X, LABEL_Y, label, AO_BLACK, AO_COPY);
}

static void
_ao_lco_batt_voltage(void)
{
	struct ao_adc	packet;
	int16_t		decivolt;

	ao_adc_single_get(&packet);
	decivolt = ao_battery_decivolt(packet.v_batt);
	_ao_lco_show_voltage((uint16_t) decivolt, "LCO battery");
	ao_st7565_update(&fb);
}

static void
_ao_lco_show_contrast(void)
{
	uint8_t	brightness = ao_st7565_get_brightness();
	int16_t contrast = (int16_t) (brightness * CONTRAST_WIDTH / AO_LCO_MAX_CONTRAST);

	ao_text(&fb, &SMALL_FONT, CONTRAST_LABEL_X, LABEL_Y, "Contrast", AO_BLACK, AO_COPY);
	ao_rect(&fb, CONTRAST_X, CONTRAST_Y, contrast, CONTRAST_HEIGHT, AO_BLACK, AO_COPY);
}

void
ao_lco_show(void)
{
	ao_mutex_get(&ao_lco_display_mutex);
	ao_rect(&fb, 0, 0, WIDTH, HEIGHT, AO_WHITE, AO_COPY);
	if (ao_lco_box == AO_LCO_LCO_VOLTAGE) {
		_ao_lco_batt_voltage();
	} else if (ao_lco_box == AO_LCO_CONTRAST) {
		_ao_lco_show_contrast();
	} else if (ao_lco_pad == AO_LCO_PAD_VOLTAGE) {
		_ao_lco_show_voltage(ao_pad_query.battery, "Pad battery");
	} else {
		_ao_lco_show_pad(ao_lco_pad);
		_ao_lco_show_box(ao_lco_box);
		ao_rect(&fb, SEP_X, 0, 2, HEIGHT, AO_BLACK, AO_COPY);
	}
	ao_st7565_update(&fb);
	ao_mutex_put(&ao_lco_display_mutex);
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


void
ao_lco_set_contrast(int16_t contrast)
{
	ao_st7565_set_brightness((uint8_t) contrast);
}

int16_t
ao_lco_get_contrast(void)
{
	return (int16_t) ao_st7565_get_brightness();
}

static struct ao_task	ao_lco_drag_task;

static void
ao_lco_drag_monitor(void)
{
	AO_TICK_TYPE	delay = ~0UL;
	AO_TICK_TYPE	now;

	ao_beep_for(AO_BEEP_MID, AO_MS_TO_TICKS(200));
	for (;;) {
		PRINTD("Drag monitor count %d delay %lu\n", ao_lco_drag_beep_count, (unsigned long) delay);
		if (delay == (AO_TICK_TYPE) ~0)
			ao_sleep(&ao_lco_drag_beep_count);
		else
			ao_sleep_for(&ao_lco_drag_beep_count, delay);

		delay = ~0UL;
		now = ao_time();
		delay = ao_lco_drag_warn_check(now, delay);
		delay = ao_lco_drag_beep_check(now, delay);
	}
}

static void
ao_lco_input(void)
{
	static struct ao_event	event;

	for (;;) {
		ao_event_get(&event);
		PRINTE("event type %d unit %d value %ld\n",
		       event.type, event.unit, (long) event.value);
		switch (event.type) {
		case AO_EVENT_QUADRATURE:
			switch (event.unit) {
			case AO_QUADRATURE_SELECT:
				if (!ao_lco_armed) {
					switch (ao_lco_select_mode) {
					case AO_LCO_SELECT_PAD:
						ao_lco_step_pad((int8_t) event.value);
						break;
					case AO_LCO_SELECT_BOX:
						ao_lco_step_box((int8_t) event.value);
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
				ao_lco_set_armed((uint8_t) event.value);
				ao_lco_set_select();
				break;
			case AO_BUTTON_FIRE:
				if (ao_lco_armed)
					ao_lco_set_firing((uint8_t) event.value);
				break;
			case AO_BUTTON_DRAG_SELECT:
				if (event.value)
					ao_lco_toggle_drag();
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

/*
 * Light up everything for a second at power on to let the user
 * visually inspect the system for correct operation
 */
static void
ao_lco_display_test(void)
{
	ao_led_on(AO_LEDS_AVAILABLE);
	ao_rect(&fb, 0, 0, WIDTH, HEIGHT, AO_BLACK, AO_COPY);
	ao_st7565_update(&fb);
	ao_delay(AO_MS_TO_TICKS(250));
	ao_led_off(AO_LEDS_AVAILABLE);
}

static struct ao_task ao_lco_input_task;
static struct ao_task ao_lco_monitor_task;
static struct ao_task ao_lco_arm_warn_task;
static struct ao_task ao_lco_igniter_status_task;

static int16_t	found_x;

void
ao_lco_search_start(void)
{
	ao_rect(&fb, 0, 0, WIDTH, HEIGHT, AO_WHITE, AO_COPY);
	ao_logo(&fb, &logo_transform, &LOGO_FONT, AO_BLACK, AO_COPY);
	found_x = FOUND_X;
}

void
ao_lco_search_box_check(int16_t box)
{
	if (box > 0)
		ao_rect(&fb, SCAN_X, SCAN_Y, box, SCAN_HEIGHT, AO_BLACK, AO_COPY);
	ao_st7565_update(&fb);
}

void
ao_lco_search_box_present(int16_t box)
{
	char	str[8];
	if (found_x < FOUND_WIDTH)
	{
		snprintf(str, sizeof(str), "%s%02u", found_x ? ", " : "", box);
		found_x = ao_text(&fb, &TINY_FONT, found_x, FOUND_Y, str, AO_BLACK, AO_COPY);
	}
}

void
ao_lco_search_done(void)
{
	ao_st7565_update(&fb);
}

static void
ao_lco_main(void)
{
	ao_lco_display_test();
	ao_lco_search();
	ao_add_task(&ao_lco_input_task, ao_lco_input, "lco input");
	ao_add_task(&ao_lco_arm_warn_task, ao_lco_arm_warn, "lco arm warn");
	ao_add_task(&ao_lco_igniter_status_task, ao_lco_igniter_status, "lco igniter status");
	ao_add_task(&ao_lco_drag_task, ao_lco_drag_monitor, "drag race");
	ao_lco_monitor();
}

#if DEBUG
static void
ao_lco_set_debug(void)
{
	uint32_t r = ao_cmd_decimal();
	if (ao_cmd_status == ao_cmd_success){
		ao_lco_debug = r & 1;
		ao_lco_event_debug = (r & 2) >> 1;
	}
}

const struct ao_cmds ao_lco_cmds[] = {
	{ ao_lco_set_debug,	"D <0 off, 1 on>\0Debug" },
	{ ao_lco_search,	"s\0Search for pad boxes" },
	{ ao_lco_pretend,	"p\0Pretend there are lots of pad boxes" },
	{ 0, NULL }
};
#endif

void
ao_lco_init(void)
{
	ao_add_task(&ao_lco_monitor_task, ao_lco_main, "lco monitor");
#if DEBUG
	ao_cmd_register(&ao_lco_cmds[0]);
#endif
}
