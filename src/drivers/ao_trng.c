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
#include <ao_boot.h>
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

static struct ao_task ao_blink_red_task;
static uint8_t ao_failed = 0; /* 0 NOMINAL, 1 FAILED */
static uint8_t ao_post = 0; /* 0 POST needed, 1 powered up */

/* On handling failure, keithp said:
 We could disconnect from USB easily enough, or disconnect and come back
 with a different setup that the kernel driver could report as an
 error. Lots of options.
*/

void
ao_trng_failure()
{
	ao_failed = 1;
	ao_wakeup(&ao_failed);
}

#ifdef DEBUG_FIPS

static void
ao_trng_fetch(void)
{
	static uint16_t	*buffer[2];
	uint32_t	kbytes = 1;
	uint32_t	count;
	int		usb_buf_id;
	uint16_t	i;
	uint16_t	*buf;
	uint16_t	t;
	uint32_t	*rnd = (uint32_t *) ao_adc_ring;
	uint32_t	cur;
	uint32_t        prev = 0;
	uint8_t         prev_set = 0; /* prev has been set */

	if (!buffer[0]) {
		buffer[0] = ao_usb_alloc();
		buffer[1] = ao_usb_alloc();
		if (!buffer[0])
			return;
	}

	ao_cmd_decimal();
	if (ao_cmd_status == ao_cmd_success)
		kbytes = ao_cmd_lex_u32;
	else
		ao_cmd_status = ao_cmd_success;
	usb_buf_id = 0;
	count = kbytes * (1024/AO_USB_IN_SIZE);

	ao_crc_reset();

	ao_led_on(AO_LED_TRNG_READ);
	while (count--) {
		t = ao_adc_get(AO_USB_IN_SIZE) >> 1;	/* one 16-bit value per output byte */
		buf = buffer[usb_buf_id];
		for (i = 0; i < AO_USB_IN_SIZE / sizeof (uint16_t); i++) {
			cur = rnd[t];
			if (prev_set && (cur == prev))
				ao_trng_failure();
			*buf++ = ao_crc_in_32_out_16(cur);
			t = (t + 1) & ((AO_ADC_RING_SIZE>>1) - 1);
                        prev = cur;
			prev_set = 1;
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
	if (!ao_failed)
		ao_trng_fetch();
}

static void
ao_trng_status(void)
{
	if (ao_failed)
		printf("FAILED\n");
	else
		printf("NOMINAL\n");
}

void ao_trng_reset(void); /* forward declaration */

static void
ao_blink_green_toggle(void)
{
	ao_blinking_green = !ao_blinking_green;
	if (!ao_blinking_green)
		ao_led_off(AO_LED_GREEN);
	ao_wakeup(&ao_blinking_green);
}

static const struct ao_cmds ao_trng_cmds[] = {
	{ ao_trng_fetch_cmd, "f <kbytes>\0Fetch a block of numbers" },
	{ ao_trng_reset, "R\0Reset" },
	{ ao_blink_green_toggle, "G\0Toggle green LED blinking" },
	{ ao_trng_status, "s\0Show status" },
	{ ao_trng_failure, "z\0Simulate failure" },
	{ 0, NULL },
};

#else

static void
ao_trng_send(void)
{
	static uint16_t	*buffer[2];
	int		usb_buf_id;
	uint16_t	i;
	uint16_t	*buf;
	uint16_t	t;
	uint32_t	*rnd = (uint32_t *) ao_adc_ring;

	if (!buffer[0]) {
		buffer[0] = ao_usb_alloc();
		buffer[1] = ao_usb_alloc();
		if (!buffer[0])
			return;
	}

	usb_buf_id = 0;

	ao_crc_reset();

	for (;;) {
		ao_led_on(AO_LED_TRNG_ACTIVE);
		t = ao_adc_get(AO_USB_IN_SIZE) >> 1;	/* one 16-bit value per output byte */
		buf = buffer[usb_buf_id];
		for (i = 0; i < AO_USB_IN_SIZE / sizeof (uint16_t); i++) {
			*buf++ = ao_crc_in_32_out_16(rnd[t]);
			t = (t + 1) & ((AO_ADC_RING_SIZE>>1) - 1);
		}
		ao_adc_ack(AO_USB_IN_SIZE);
		ao_led_off(AO_LED_TRNG_ACTIVE);
		ao_usb_write(buffer[usb_buf_id], AO_USB_IN_SIZE);
		usb_buf_id = 1-usb_buf_id;
	}
}

static struct ao_task ao_trng_send_task;

static void
ao_bootloader_cmd(void)
{
	for (;;) {
		getchar(); /* any char will do */
                /* give feedback we are going into bootloader mode */
		ao_led_on(AO_LED_GREEN);
		ao_delay(AO_MS_TO_TICKS(500));
		ao_led_off(AO_LED_GREEN);
		ao_delay(AO_MS_TO_TICKS(500));
		ao_led_on(AO_LED_GREEN);
		ao_delay(AO_MS_TO_TICKS(500));
		ao_led_off(AO_LED_GREEN);
		ao_delay(AO_MS_TO_TICKS(500));
		ao_led_on(AO_LED_GREEN);
		ao_delay(AO_MS_TO_TICKS(500));
		ao_led_off(AO_LED_GREEN);
                ao_boot_loader();
	}
}

static struct ao_task ao_bootloader_cmd_task;

#endif


/* NOTE: the reset function also functions as the Power On Self Test */
void
ao_trng_reset(void)
{
	/* printf("Resetting...\n"); */
	ao_failed = 0;
	ao_led_off(AO_LED_RED);
	ao_wakeup(&ao_failed);
	/* get the first 1k bits and ensure there are no duplicates */
	/* FIXME ao_trng_fetch(); */
        if (!ao_failed) {
		ao_led_on(AO_LED_GREEN);
		ao_delay(AO_MS_TO_TICKS(1000));
		ao_led_off(AO_LED_GREEN);
        }
}

static void
ao_blink_red(void)
{
	if (!ao_post) {
		ao_trng_reset(); /* POST */
		ao_post = 1;
	}
	for (;;) {
		while (!ao_failed)
			ao_sleep(&ao_failed);
		while (ao_failed) {
			ao_led_toggle(AO_LED_RED);
			ao_delay(AO_MS_TO_TICKS(500));
		}
	}
}

void
ao_trng_init(void)
{
	ao_add_task(&ao_blink_red_task, ao_blink_red, "blink_red");
	ao_add_task(&ao_blink_green_task, ao_blink_green, "blink_green");
#ifdef DEBUG_FIPS
	ao_cmd_register(ao_trng_cmds);
#else
	ao_add_task(&ao_bootloader_cmd_task, ao_bootloader_cmd, "bootloader_cmd");
	ao_add_task(&ao_trng_send_task, ao_trng_send, "trng_send");
#endif
}
