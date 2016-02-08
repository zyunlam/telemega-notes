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
#include <ao_exti.h>
#include <ao_power.h>

static uint8_t		trng_running;
static AO_TICK_TYPE	trng_power_time;

/* Make sure there's at least 8 bits of variance in the samples */
#define MIN_VARIANCE		(128 * 128)

#define DECLARE_STATS	int32_t sum = 0, sum2 = 0

#define ADD_STATS(value) do {			\
		sum += (value);			\
		sum2 += (value) * (value);	\
	} while(0)

#define GOOD_STATS(i)	(((sum2 - (sum * sum) / i) / i) >= MIN_VARIANCE)

#define TRNG_ENABLE_DELAY	AO_MS_TO_TICKS(100)

static int
ao_trng_send_raw(uint16_t *buf)
{
	uint16_t	i;
	uint16_t	t;
	uint16_t	v;

	DECLARE_STATS;

	t = ao_adc_get(AO_USB_IN_SIZE>>1);	/* one 16-bit value per two output bytes */
	for (i = 0; i < AO_USB_IN_SIZE / sizeof (uint16_t); i++) {
		v = ao_adc_ring[t];
		*buf++ = v;
		t = (t + 1) & (AO_ADC_RING_SIZE - 1);

		ADD_STATS(v);
	}
	return GOOD_STATS(AO_USB_IN_SIZE / sizeof (uint16_t));
}

static int
ao_trng_send_cooked(uint16_t *buf)
{
	uint16_t	i;
	uint16_t	t;
	uint32_t	*rnd = (uint32_t *) ao_adc_ring;

	DECLARE_STATS;

	t = ao_adc_get(AO_USB_IN_SIZE) >> 1;		/* one 16-bit value per output byte */

	for (i = 0; i < AO_USB_IN_SIZE / sizeof (uint16_t); i++) {
		uint32_t	v;
		uint16_t	v1, v2;

		/* Fetch two values in one operation */
		v = rnd[t];
		t = (t + 1) & ((AO_ADC_RING_SIZE >> 1) - 1);

		*buf++ = ao_crc_in_32_out_16(v);

		v1 = v;
		v2 = v >> 16;

		ADD_STATS(v1);
		ADD_STATS(v2);
	}
	return GOOD_STATS(2 * AO_USB_IN_SIZE / sizeof (uint16_t));
}

static inline int
ao_send_raw(void)
{
	return !ao_gpio_get(AO_RAW_PORT, AO_RAW_BIT, AO_RAW_PIN);
}

static void
ao_trng_send(void)
{
	static uint16_t	*buffer[2];
	int	usb_buf_id;
	int	good_bits;
	int	failed = 0;

	if (!buffer[0]) {
		buffer[0] = ao_usb_alloc();
		buffer[1] = ao_usb_alloc();
		if (!buffer[0])
			return;
	}

	usb_buf_id = 0;

#ifdef AO_TRNG_ENABLE_PORT
	ao_gpio_set(AO_TRNG_ENABLE_PORT, AO_TRNG_ENABLE_BIT, AO_TRNG_ENABLE_PIN, 1);
#endif
	trng_power_time = ao_time();

	ao_crc_reset();

	for (;;) {
		if (!trng_running) {
			AO_TICK_TYPE	delay;

			delay = trng_power_time + TRNG_ENABLE_DELAY - ao_time();
			if (delay > TRNG_ENABLE_DELAY)
				delay = TRNG_ENABLE_DELAY;

			/* Delay long enough for the HV power supply
			 * to stabilize so that the first bits we read
			 * aren't of poor quality
			 */
			ao_delay(delay);
			trng_running = TRUE;
		}
		if (ao_send_raw()) {
			ao_led_on(AO_LED_TRNG_RAW);
			good_bits = ao_trng_send_raw(buffer[usb_buf_id]);
			ao_led_off(AO_LED_TRNG_RAW);
		} else {
			ao_led_on(AO_LED_TRNG_COOKED);
			good_bits = ao_trng_send_cooked(buffer[usb_buf_id]);
			ao_led_off(AO_LED_TRNG_COOKED);
		}
		ao_adc_ack(AO_USB_IN_SIZE);
		if (good_bits) {
			ao_usb_write(buffer[usb_buf_id], AO_USB_IN_SIZE);
			usb_buf_id = 1-usb_buf_id;
			failed = 0;
		} else {
			failed++;
			ao_delay(AO_MS_TO_TICKS(10));
			if (failed > 10) {
				ao_usb_disable();
				ao_panic(AO_PANIC_DMA);
			}
		}
	}
}

static struct ao_task ao_trng_send_task;

#if AO_POWER_MANAGEMENT

static void ao_trng_suspend(void *arg)
{
	(void) arg;
#ifdef AO_TRNG_ENABLE_PORT
	ao_gpio_set(AO_TRNG_ENABLE_PORT, AO_TRNG_ENABLE_BIT, AO_TRNG_ENABLE_PIN, 0);
#endif
	trng_running = FALSE;
}

static void ao_trng_resume(void *arg)
{
	(void) arg;
#ifdef AO_TRNG_ENABLE_PORT
	ao_gpio_set(AO_TRNG_ENABLE_PORT, AO_TRNG_ENABLE_BIT, AO_TRNG_ENABLE_PIN, 1);
#endif
	trng_power_time = ao_time();
}

static struct ao_power ao_trng_power = {
	.suspend = ao_trng_suspend,
	.resume = ao_trng_resume
};

#endif

void
ao_trng_send_init(void)
{
#ifdef AO_TRNG_ENABLE_PORT
	ao_enable_output(AO_TRNG_ENABLE_PORT, AO_TRNG_ENABLE_BIT, AO_TRNG_ENABLE_PIN, 0);
	ao_power_register(&ao_trng_power);
#endif
	ao_enable_input(AO_RAW_PORT, AO_RAW_BIT, AO_EXTI_MODE_PULL_UP);
	ao_add_task(&ao_trng_send_task, ao_trng_send, "trng_send");
}
