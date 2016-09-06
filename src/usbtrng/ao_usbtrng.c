/*
 * Copyright © 2014 Keith Packard <keithp@keithp.com>
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

#define AO_TRNG_SPI_BUS		1

static uint32_t			spi_speed = AO_SPI_SPEED_4MHz;

#define AO_TRNG_SPI_BUF		1024

#if 0

static uint8_t	*spi_buf;
static uint8_t	*spi_head, *spi_tail;
static uint8_t	spi_wakeup;

static void
ao_trng_run(void)
{
	int	this_time;
	uint8_t	*end;

	if (!spi_buf)
		spi_buf = ao_usb_alloc(AO_TRNG_SPI_BUF);
	flush();
	ao_spi_get(AO_TRNG_SPI_BUS, spi_speed);
	spi_tail = spi_buf;
	spi_head = spi_buf;
	ao_spi_recv_ring_start(spi_buf, AO_TRNG_SPI_BUF, &spi_head, &spi_tail, &spi_wakeup, AO_TRNG_SPI_BUS);
	while (!ao_usb_out_avail) {
		ao_arch_block_interrupts();
		while (spi_head == spi_tail) {
			spi_wakeup = 0;
			ao_sleep(&spi_wakeup);
		}
		ao_arch_release_interrupts();
		if (spi_tail > spi_head)
			end = spi_buf + AO_TRNG_SPI_BUF;
		else
			end = spi_head;
		this_time = end - spi_tail;
		if (this_time > 64)
			this_time = 64;
		ao_usb_write(spi_tail, this_time);
		spi_tail += this_time;
		if (spi_tail == spi_buf + AO_TRNG_SPI_BUF)
			spi_tail = spi_buf;
	}
	ao_spi_put(AO_TRNG_SPI_BUS);
	getchar();
}


static void
ao_trng_test(void)
{
	static uint8_t	random[32];
	uint8_t		i;

	ao_spi_get(AO_TRNG_SPI_BUS, spi_speed);
	ao_spi_recv(random, sizeof (random), AO_TRNG_SPI_BUS);
	ao_spi_put(AO_TRNG_SPI_BUS);
	for (i = 0; i < sizeof (random); i++)
		printf (" %02x", random[i]);
	printf ("\n");
}
#endif

#define ADC_RING_SIZE	512

static uint8_t *adc_ring;
static uint16_t	adc_head, adc_tail;
static uint16_t adc_wake;

void  lpc_adc_isr(void)
{
	uint16_t	avail;
	uint16_t	this, next;

	this = adc_head;
	next = (this + 1) & (ADC_RING_SIZE - 1);
	if  (next == adc_tail) {
		lpc_adc.inten = 0;
		return;
	}
	adc_ring[this] = lpc_adc.dr[6] >> 8;
	adc_head = next;

	/* If there are enough entries, wake up any waiters
	 */
	avail = (next - adc_tail) & (ADC_RING_SIZE - 1);
	if (avail >= adc_wake) {
		adc_wake = 0;
		ao_wakeup(&adc_wake);
	}
}

#define AO_ADC_CLKDIV	(AO_LPC_SYSCLK / 450000)

static void
ao_trng_adc_init(void)
{
	adc_ring = ao_usb_alloc(ADC_RING_SIZE);

	lpc_scb.sysahbclkctrl |= (1 << LPC_SCB_SYSAHBCLKCTRL_ADC);
	lpc_scb.pdruncfg &= ~(1 << LPC_SCB_PDRUNCFG_ADC_PD);

	/* Enable interrupt when AO_ADC_6 is complete */
	lpc_adc.inten = 0;

	lpc_nvic_set_enable(LPC_ISR_ADC_POS);
	lpc_nvic_set_priority(LPC_ISR_ADC_POS, AO_LPC_NVIC_CLOCK_PRIORITY);

#if AO_ADC_0
	ao_enable_analog(0, 11, 0);
#endif
#if AO_ADC_1
	ao_enable_analog(0, 12, 1);
#endif
#if AO_ADC_2
	ao_enable_analog(0, 13, 2);
#endif
#if AO_ADC_3
	ao_enable_analog(0, 14, 3);
#endif
#if AO_ADC_4
	ao_enable_analog(0, 15, 4);
#endif
#if AO_ADC_5
	ao_enable_analog(0, 16, 5);
#endif
#if AO_ADC_6
	ao_enable_analog(0, 22, 6);
#endif
#if AO_ADC_7
	ao_enable_analog(0, 23, 7);
#endif

	lpc_adc.cr = ((1 << (LPC_ADC_CR_SEL + 6)) |
		      (AO_ADC_CLKDIV << LPC_ADC_CR_CLKDIV) |
		      (1 << LPC_ADC_CR_BURST) |
		      (LPC_ADC_CR_CLKS_9 << LPC_ADC_CR_CLKS));
}

static void
ao_trng_adc_dump(void)
{
	int	i;

	while (((adc_head - adc_tail) & (ADC_RING_SIZE - 1)) < 16) {
		lpc_adc.inten = (1 << (LPC_ADC_INTEN_ADINTEN + 6));
		adc_wake = 16;
		ao_sleep(&adc_wake);
	}
	printf("adc_head %d tail %d\n", adc_head, adc_tail);

	for (i = 0; i < 16; i++) {
		printf(" %4d", adc_ring[adc_tail]);
		adc_tail = (adc_tail + 1) & (ADC_RING_SIZE - 1);
	}
	printf("\n");
	lpc_adc.inten = 0;
}

static void
ao_trng_run(void)
{
	uint16_t	this_time;
	flush();

	while (!ao_usb_out_avail) {
		ao_arch_block_interrupts();
		while (((adc_head - adc_tail) & (ADC_RING_SIZE - 1)) < 64) {
			lpc_adc.inten = (1 << (LPC_ADC_INTEN_ADINTEN + 6));
			adc_wake = 64;
			ao_sleep(&adc_wake);
		}
		ao_arch_release_interrupts();

		this_time = ADC_RING_SIZE - adc_tail;
		if (this_time > 64)
			this_time = 64;
		ao_usb_write(&adc_ring[adc_tail], this_time);
		adc_tail = (adc_tail + this_time) & (ADC_RING_SIZE - 1);
	}
	lpc_adc.inten = 0;
}

static void
ao_trng_speed(void)
{
	ao_cmd_decimal();

	if (ao_cmd_lex_u32 == 0 || ao_cmd_status != ao_cmd_success) {
		ao_cmd_status = ao_cmd_success;
		printf ("Current spi speed %d\n", spi_speed);
	} else {
		spi_speed = ao_cmd_lex_u32;
	}
}

static const struct ao_cmds ao_trng_cmds[] = {
//	{ ao_trng_test,	"R\0Dump some random numbers" },
	{ ao_trng_run,	"s\0Send random bits until char" },
	{ ao_trng_speed, "S <speed>\0Set SPI speed (48MHz/speed)" },
	{ ao_trng_adc_dump, "a\0Dump ADC data" },
	{ 0, NULL }
};

void
main(void)
{
	ao_clock_init();
	ao_task_init();
	ao_timer_init();

//	ao_spi_init();
	ao_usb_init();

	ao_trng_adc_init();

	ao_serial_init();

	ao_led_init(LEDS_AVAILABLE);

	ao_cmd_init();

	ao_cmd_register(ao_trng_cmds);

	ao_start_scheduler();
}
