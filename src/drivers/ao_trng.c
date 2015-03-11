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


static struct ao_task ao_blink_red_task;
static uint8_t ao_failed = 0;

static void
ao_blink_red(void)
{
	for (;;) {
		while (!ao_failed)
			ao_sleep(&ao_failed);
		while (ao_failed) {
			ao_led_toggle(AO_LED_RED);
			ao_delay(AO_MS_TO_TICKS(500));
		}
	}
}

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

#define ADC_WORDS (AO_USB_IN_SIZE / sizeof (uint16_t))
static uint16_t prev16 = 0;
static uint16_t	adc_values[ADC_WORDS];
static uint8_t	adc_i = ADC_WORDS;

static uint16_t
fetch16(uint8_t initialize)
{
	uint32_t	*rnd;
	uint8_t		i;

	if (ao_failed)
		return 0;
	if (initialize)
		adc_i = ADC_WORDS;
	if (adc_i == ADC_WORDS) {
		rnd = (uint32_t *) ao_adc_get(AO_USB_IN_SIZE);	/* one 16-bit value per output byte */
		adc_i = 0;
		for (i = 0; i < ADC_WORDS; i++) {
			adc_values[i] = ao_crc_in_32_out_16(*rnd++);
		}
	}
	if (initialize)
		prev16 = adc_values[adc_i++];
	if (adc_values[adc_i] == prev16) {
		/* FAILED: how do we prevent spewing bad data? */
		printf("FAILED value %d repeated\n", prev16);
		ao_failed = 1;
		ao_wakeup(&ao_failed);
		return 0;
	}
	prev16 = adc_values[adc_i];
	adc_i++;
	return prev16;
}

static void
ao_simulate_failure(void)
{
	if (adc_i == ADC_WORDS)
		fetch16(0);
	prev16 = adc_values[adc_i];
}

/* NOTE: the reset function also functions as the Power On Self Test */
void
ao_reset(void)
{
	uint32_t	count;

	printf("Power On Self Test\n"); /* DEBUGGING */
	ao_failed = 0;
	ao_led_off(AO_LED_RED);
	ao_wakeup(&ao_failed);
	fetch16(1);
	/* get the first 1k bits and ensure there are no duplicates */
	count = 1024 / sizeof (uint16_t);
	while (count-- && !ao_failed) {
		fetch16(0);
	}
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
ao_trng_fetch(void)
{
	static uint16_t	*buffer[2];
	uint32_t	kbytes = 1;
	uint32_t	count;
	int		usb_buf_id;
	uint16_t	i;
	uint16_t	*buf;

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
		buf = buffer[usb_buf_id];
		for (i = 0; i < AO_USB_IN_SIZE / sizeof (uint16_t); i++)
			*buf++ = fetch16(0);
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
ao_trng_word(void)
{
	printf("%d\n", fetch16(0));
}

static void
ao_status(void)
{
	if (ao_failed)
		printf("FAILED\n");
	else
		printf("NOMINAL\n");
}

static const struct ao_cmds ao_trng_cmds[] = {
	{ ao_trng_fetch, "f <kbytes>\0Fetch a block of numbers" },
	{ ao_trng_word, "F\0Fetch 16 bit unsigned int" },
	{ ao_reset, "R\0reset" },
	{ ao_blink_green_toggle, "G\0Toggle green LED blinking" },
	{ ao_status, "s\0show status" },
	{ ao_simulate_failure, "z\0simulate failure" },
	{ 0, NULL },
};

void
ao_trng_init(void)
{
	ao_add_task(&ao_blink_red_task, ao_blink_red, "blink_red");
	ao_add_task(&ao_blink_green_task, ao_blink_green, "blink_green");
	ao_cmd_register(ao_trng_cmds);
}
