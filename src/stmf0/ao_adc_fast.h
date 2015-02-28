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

#ifndef _AO_ADC_FAST_H_
#define _AO_ADC_FAST_H_

void
ao_adc_read(uint16_t *dest, int len);

void
ao_adc_init(void);

/* Total ring size in samples */
#define AO_ADC_RING_SIZE	256

extern uint16_t	ao_adc_ring[AO_ADC_RING_SIZE];

#define ao_adc_ring_step(pos,inc)	(((pos) + (inc)) & (AO_ADC_RING_SIZE - 1))

extern uint16_t	ao_adc_ring_head, ao_adc_ring_tail;
extern uint16_t	ao_adc_running;

void
_ao_adc_start(void);

static inline uint16_t
_ao_adc_remain(void)
{
	if (ao_adc_ring_tail > ao_adc_ring_head)
		return AO_ADC_RING_SIZE - ao_adc_ring_tail;
	return ao_adc_ring_head - ao_adc_ring_tail;
}

static inline uint16_t
_ao_adc_space(void)
{
	if (ao_adc_ring_head >= ao_adc_ring_tail)
		return AO_ADC_RING_SIZE - ao_adc_ring_head;
	return ao_adc_ring_tail - ao_adc_ring_head;
}

static inline uint16_t *
ao_adc_get(uint16_t n)
{
	if (ao_adc_ring_tail + n > AO_ADC_RING_SIZE)
		ao_panic(AO_PANIC_ADC);
	ao_arch_block_interrupts();
	while (_ao_adc_remain() < n) {
		if (!ao_adc_running)
			_ao_adc_start();
		ao_sleep(&ao_adc_ring_head);
	}
	ao_arch_release_interrupts();
	return &ao_adc_ring[ao_adc_ring_tail];
}

static inline void
ao_adc_ack(uint16_t n)
{
	if (ao_adc_ring_tail + n > AO_ADC_RING_SIZE)
		ao_panic(AO_PANIC_ADC);
	ao_arch_block_interrupts();
	ao_adc_ring_tail += n;
	if (ao_adc_ring_tail == AO_ADC_RING_SIZE)
		ao_adc_ring_tail = 0;
	if (!ao_adc_running)
		_ao_adc_start();
	ao_arch_release_interrupts();
}

#endif /* _AO_ADC_FAST_H_ */
