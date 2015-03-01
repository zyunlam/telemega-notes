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
#include <ao_trng_send.h>

static void
ao_trng_send(void)
{
	static uint16_t	*buffer[2];
	int		usb_buf_id;
	uint16_t	i;
	uint16_t	*buf;
	uint32_t	*rnd;

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
		rnd = (uint32_t *) ao_adc_get(AO_USB_IN_SIZE);	/* one 16-bit value per output byte */
		buf = buffer[usb_buf_id];
		for (i = 0; i < AO_USB_IN_SIZE / sizeof (uint16_t); i++)
			*buf++ = ao_crc_in_32_out_16(*rnd++);
		ao_adc_ack(AO_USB_IN_SIZE);
		ao_led_off(AO_LED_TRNG_ACTIVE);
		ao_usb_write(buffer[usb_buf_id], AO_USB_IN_SIZE);
		usb_buf_id = 1-usb_buf_id;
	}
}

static struct ao_task ao_trng_send_task;

void
ao_trng_send_init(void)
{
	ao_add_task(&ao_trng_send_task, ao_trng_send, "trng_send");
}
