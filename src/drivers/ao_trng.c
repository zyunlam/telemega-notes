/*
 * Copyright © 2015 Keith Packard <keithp@keithp.com>
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
#include <ao_adc_fast.h>
#include <ao_crc.h>
#include <ao_trng.h>

static struct ao_task ao_blink_green_task;
static uint8_t ao_blinking_green = 0;

static void
ao_blink_green(void)
{
	for (;;) {
		while (!ao_blinking_green)
			ao_sleep(&ao_blinking_green);
		while (ao_blinking_green) {
			ao_led_toggle(AO_LED_GREEN);
			ao_delay(AO_MS_TO_TICKS(1000));
		}
	}
}


static void
ao_blink_green_toggle(void)
{
	ao_blinking_green = !ao_blinking_green;
	if (!ao_blinking_green)
		ao_led_off(AO_LED_GREEN);
	ao_wakeup(&ao_blinking_green);
}

static struct ao_task ao_blink_red_task;
static int8_t ao_failed = -1; /* -1 need POST, 0 NOMINAL, 1 FAILED */

/* On handling failure, keithp said:
 We could disconnect from USB easily enough, or disconnect and come back
 with a different setup that the kernel driver could report as an
 error. Lots of options.
*/
void
ao_trng_failure(void)
{
	ao_failed = 1;
	ao_wakeup(&ao_failed);
}

static void
ao_trng_fetch(uint32_t kbytes)
{
	static uint16_t	*buffer[2];
	uint32_t	count;
	int		usb_buf_id;
	uint16_t	i;
	uint16_t	*buf;
	uint32_t        *rnd;
	uint32_t        prev, cur;

	if (!buffer[0]) {
		buffer[0] = ao_usb_alloc();
		buffer[1] = ao_usb_alloc();
		if (!buffer[0])
			return;
	}

	usb_buf_id = 0;
	count = kbytes * (1024/AO_USB_IN_SIZE);

	ao_crc_reset();

	ao_led_on(AO_LED_TRNG_READ);
	while (count--) {
		/* one 16-bit value per output byte */
		rnd = (uint32_t *) ao_adc_get(AO_USB_IN_SIZE + 2);
		buf = buffer[usb_buf_id];
		prev = *rnd++;
		for (i = 0; i < AO_USB_IN_SIZE / sizeof (uint16_t); i++) {
			cur = *rnd++;
			if (cur == prev)
				ao_trng_failure();
			*buf++ = ao_crc_in_32_out_16(cur);
			prev = cur;
		}
		ao_adc_ack(AO_USB_IN_SIZE);
		ao_led_toggle(AO_LED_TRNG_READ|AO_LED_TRNG_WRITE);
		ao_usb_write(buffer[usb_buf_id], AO_USB_IN_SIZE);
		ao_led_toggle(AO_LED_TRNG_READ|AO_LED_TRNG_WRITE);
		usb_buf_id = 1-usb_buf_id;
	}
	ao_led_off(AO_LED_TRNG_READ|AO_LED_TRNG_WRITE);
	flush();
}

static void
ao_trng_fetch_cmd(void)
{
	uint32_t kbytes = 1;

	ao_cmd_decimal();
	if (ao_cmd_status == ao_cmd_success)
		kbytes = ao_cmd_lex_u32;
	else
		ao_cmd_status = ao_cmd_success;
	ao_trng_fetch(kbytes);
}

/* NOTE: the reset function also functions as the Power On Self Test */
void
ao_trng_reset(void)
{
	printf("Power On Self Test\n"); /* DEBUGGING */
	ao_failed = 0;
	ao_led_off(AO_LED_RED);
	ao_wakeup(&ao_failed);
	/* get the first 1k bits and ensure there are no duplicates */
	ao_trng_fetch(1);
	if (ao_failed) { /* show failure */
		printf("FAILED self test\n");
	} else { /* show success */
		printf("PASS - operation NOMINAL\n");
		/* this blocks! */
		ao_led_on(AO_LED_GREEN);
		ao_delay(AO_MS_TO_TICKS(1000));
		ao_led_off(AO_LED_GREEN);
	}
}

static void
ao_blink_red(void)
{
	if (ao_failed < 0)
		ao_trng_reset(); /* lazy POST */
	for (;;) {
		while (!ao_failed)
			ao_sleep(&ao_failed);
		while (ao_failed) {
			ao_led_toggle(AO_LED_RED);
			ao_delay(AO_MS_TO_TICKS(500));
		}
	}
}

static void
ao_trng_status(void)
{
	if (ao_failed)
		printf("FAILED\n");
	else
		printf("NOMINAL\n");
}

static const struct ao_cmds ao_trng_cmds[] = {
	{ ao_trng_fetch_cmd, "f <kbytes>\0Fetch a block of numbers" },
	{ ao_trng_reset, "R\0Reset" },
	{ ao_blink_green_toggle, "G\0Toggle green LED blinking" },
	{ ao_trng_status, "s\0Show status" },
	{ ao_trng_failure, "z\0Simulate failure" },
	{ 0, NULL },
};

void
ao_trng_init(void)
{
	ao_add_task(&ao_blink_red_task, ao_blink_red, "blink_red");
	ao_add_task(&ao_blink_green_task, ao_blink_green, "blink_green");
	ao_cmd_register(ao_trng_cmds);
}
