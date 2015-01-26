/*
 * Copyright © 2012 Keith Packard <keithp@keithp.com>
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
#include "stm32f0.h"
#include <string.h>
#include <ao_boot.h>

extern void main(void);
extern char __stack__;
extern char __text_start__, __text_end__;
extern char __data_start__, __data_end__;
extern char __bss_start__, __bss_end__;

/* Interrupt functions */

void stm_halt_isr(void)
{
	ao_panic(AO_PANIC_CRASH);
}

void stm_ignore_isr(void)
{
}

const void *stm_interrupt_vector[];

uint32_t
stm_flash_size(void) {
	uint16_t	dev_id = stm_dev_id();
	uint16_t	kbytes = 0;

	switch (dev_id) {
	case 0x445:
		kbytes = 32;	/* assume 32kB until we figure this out */
		break;
	}
	return (uint32_t) kbytes * 1024;
}

void start(void)
{
#if 0
#ifdef AO_BOOT_CHAIN
	if (ao_boot_check_chain()) {
#ifdef AO_BOOT_PIN
		ao_boot_check_pin();
#endif
	}
#endif
#endif
	memcpy(&__data_start__, &__text_end__, &__data_end__ - &__data_start__);
	memset(&__bss_start__, '\0', &__bss_end__ - &__bss_start__);
	main();
}

#define STRINGIFY(x) #x

#define isr(name) \
	void __attribute__ ((weak)) stm_ ## name ## _isr(void); \
	_Pragma(STRINGIFY(weak stm_ ## name ## _isr = stm_ignore_isr))

#define isr_halt(name) \
	void __attribute__ ((weak)) stm_ ## name ## _isr(void); \
	_Pragma(STRINGIFY(weak stm_ ## name ## _isr = stm_halt_isr))

isr(nmi)
isr_halt(hardfault)
isr_halt(memmanage)
isr_halt(busfault)
isr_halt(usagefault)
isr(svc)
isr(debugmon)
isr(pendsv)
isr(systick)
isr(wwdg)
isr(pvd)
isr(rtc)
isr(flash)
isr(rcc_crs)
isr(exti0_1)
isr(exti2_3)
isr(exti4_15)
isr(tsc)
isr(dma_ch1)
isr(dma_ch2_3_dma2_ch1_2)
isr(dma_ch4_5_6_7_dma2_ch3_4_5)
isr(adc_comp)
isr(tim1_brk_up_trg_com)
isr(tim1_cc)
isr(tim2)
isr(tim3)
isr(tim6_dac)
isr(tim7)
isr(tim14)
isr(tim15)
isr(tim16)
isr(tim17)
isr(i2c1)
isr(i2c2)
isr(spi1)
isr(spi2)
isr(usart1)
isr(usart2)
isr(usart3_4_5_6_7_8)
isr(cec_can)
isr(usb)

#define i(addr,name)	[(addr)/4] = stm_ ## name ## _isr

__attribute__ ((section(".interrupt")))
const void *stm_interrupt_vector[] = {
	[0] = &__stack__,
	[1] = start,
	i(0x08, nmi),
	i(0x0c, hardfault),
	i(0x2c, svc),
	i(0x30, debugmon),
	i(0x38, pendsv),
	i(0x3c, systick),
	i(0x40, wwdg),		/* IRQ0 */
	i(0x44, pvd),
	i(0x48, rtc),
	i(0x4c, flash),
	i(0x50, rcc_crs),
	i(0x54, exti0_1),
	i(0x58, exti2_3),
	i(0x5c, exti4_15),
	i(0x60, tsc),
	i(0x64, dma_ch1),
	i(0x68, dma_ch2_3_dma2_ch1_2),
	i(0x6c, dma_ch4_5_6_7_dma2_ch3_4_5),
	i(0x70, adc_comp),
	i(0x74, tim1_brk_up_trg_com),
	i(0x78, tim1_cc),
	i(0x7c, tim2),
	i(0x80, tim3),
	i(0x84, tim6_dac),
	i(0x88, tim7),
	i(0x8c, tim14),
	i(0x90, tim15),
	i(0x94, tim16),
	i(0x98, tim17),
	i(0x9c, i2c1),
	i(0xa0, i2c2),
	i(0xa4, spi1),
	i(0xa8, spi2),
	i(0xac, usart1),
	i(0xb0, usart2),
	i(0xb4, usart3_4_5_6_7_8),
	i(0xb8, cec_can),
	i(0xbc, usb),
};
