/*
 * Copyright © 2014 Keith Packard <keithp@keithp.com>
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

static void
ao_trng_fetch(void)
{
	static uint16_t	*buffer[2];
	uint32_t	kbytes = 1;
	uint32_t	count;
	int		usb_buf_id;
	int		i;
	uint16_t	*buf;
	uint32_t	*rnd;

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

	ao_led_on(AO_LED_GREEN);
	while (count--) {
		buf = buffer[usb_buf_id];
//		printf ("before get: head %3d tail %3d running %d\n", ao_adc_ring_head, ao_adc_ring_tail, ao_adc_running); flush();
		rnd = (uint32_t *) ao_adc_get(AO_USB_IN_SIZE);	/* one 16-bit value per output byte */
//		printf ("after get: head %3d tail %3d running %d\n", ao_adc_ring_head, ao_adc_ring_tail, ao_adc_running); flush();
		for (i = 0; i < 32; i++)
			*buf++ = ao_crc_in_32_out_16(*rnd++);
		ao_adc_ack(AO_USB_IN_SIZE);
//		printf ("after ack: head %3d tail %3d running %d\n", ao_adc_ring_head, ao_adc_ring_tail, ao_adc_running); flush();
		ao_led_toggle(AO_LED_GREEN|AO_LED_RED);
		ao_usb_write(buffer[usb_buf_id], AO_USB_IN_SIZE);
		ao_led_toggle(AO_LED_GREEN|AO_LED_RED);
		usb_buf_id = 1-usb_buf_id;
	}
	ao_led_off(AO_LED_GREEN|AO_LED_RED);
	flush();
}

static const struct ao_cmds usbtrng_cmds[] = {
	{ ao_trng_fetch,	"f <kbytes>\0Fetch a block of numbers" },
	{ 0, NULL },
};

void main(void)
{
	ao_led_init(LEDS_AVAILABLE);
	ao_led_on(AO_LED_RED);
	ao_clock_init();
	ao_task_init();
	ao_timer_init();
	ao_dma_init();
	ao_adc_init();
	ao_crc_init();

	ao_cmd_init();

	ao_usb_init();

	ao_cmd_register(usbtrng_cmds);
	ao_led_off(AO_LED_RED);

	ao_start_scheduler();
}
