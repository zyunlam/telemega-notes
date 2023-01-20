/*
 * Copyright © 2019 Keith Packard <keithp@keithp.com>
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

#ifndef _AO_ARCH_FUNCS_H_
#define _AO_ARCH_FUNCS_H_

#define AO_MODE_PULL_NONE	0
#define AO_MODE_PULL_UP		1
#define AO_MODE_PULL_DOWN	2

static inline void ao_enable_port(struct samd21_port *port)
{
	(void) port;
	samd21_pm.apbbmask |= (1UL << SAMD21_PM_APBBMASK_PORT);
}

static inline void ao_disable_port(struct samd21_port *port)
{
	(void) port;
	samd21_pm.apbbmask &= ~(1UL << SAMD21_PM_APBBMASK_PORT);
}

static inline void
ao_gpio_set(struct samd21_port *port, uint8_t bit, uint8_t v)
{
	if (v)
		port->outset = (1 << bit);
	else
		port->outclr = (1 << bit);
}

static inline uint8_t
ao_gpio_get(struct samd21_port *port, uint8_t bit)
{
	return (port->in >> bit) & 1;
}

static inline void
ao_gpio_dir_set(struct samd21_port *port, uint8_t bit, bool output)
{
	if (output)
		port->dirset = (1 << bit);
	else
		port->dirclr = (1 << bit);
}

static inline void
ao_gpio_set_mode(struct samd21_port *port, uint8_t bit, uint32_t mode)
{
	uint8_t pincfg = 0;

	if (mode != AO_MODE_PULL_NONE) {
		pincfg |= (1 << SAMD21_PORT_PINCFG_PULLEN);
		ao_gpio_set(port, bit, mode == AO_MODE_PULL_UP);
	}

	samd21_port_pincfg_set(port, bit,
			       (0 << SAMD21_PORT_PINCFG_DRVSTR) |
			       (1 << SAMD21_PORT_PINCFG_PULLEN) |
			       (0 << SAMD21_PORT_PINCFG_INEN) |
			       (0 << SAMD21_PORT_PINCFG_PMUXEN),
			       pincfg);
}

static inline void
ao_enable_output(struct samd21_port *port, uint8_t pin, uint8_t v)
{
	ao_enable_port(port);
	ao_gpio_set(port, pin, v);
	samd21_port_dir_set(port, pin, SAMD21_PORT_DIR_OUT);
	samd21_port_pincfg_set(port, pin,
			       (1 << SAMD21_PORT_PINCFG_DRVSTR) |
			       (1 << SAMD21_PORT_PINCFG_PULLEN) |
			       (1 << SAMD21_PORT_PINCFG_INEN),
			       (0 << SAMD21_PORT_PINCFG_DRVSTR) |
			       (0 << SAMD21_PORT_PINCFG_PULLEN) |
			       (0 << SAMD21_PORT_PINCFG_INEN));
}

static inline void
ao_enable_input(struct samd21_port *port, uint8_t pin, uint32_t mode)
{
	ao_enable_port(port);
	samd21_port_dir_set(port, pin, SAMD21_PORT_DIR_IN);
	uint8_t	pincfg;

	pincfg = ((0 << SAMD21_PORT_PINCFG_DRVSTR) |
		  (0 << SAMD21_PORT_PINCFG_PULLEN) |
		  (1 << SAMD21_PORT_PINCFG_INEN) |
		  (0 << SAMD21_PORT_PINCFG_PMUXEN));

	if (mode != AO_MODE_PULL_NONE) {
		pincfg |= (1 << SAMD21_PORT_PINCFG_PULLEN);
		ao_gpio_set(port, pin, mode == AO_MODE_PULL_UP);
	}

	samd21_port_pincfg_set(port, pin,
			       (1 << SAMD21_PORT_PINCFG_DRVSTR) |
			       (1 << SAMD21_PORT_PINCFG_PULLEN) |
			       (1 << SAMD21_PORT_PINCFG_INEN) |
			       (1 << SAMD21_PORT_PINCFG_PMUXEN),
			       pincfg);
}

static inline void
ao_enable_cs(struct samd21_port *port, uint8_t pin)
{
	ao_enable_output(port, pin, 1);
}

/* ao_spi_samd21.c */

#define AO_SPI_INDEX_BIT	0
#define AO_SPI_INDEX_MASK	0x07

#define AO_SPI_CONFIG_BIT	4
#define AO_SPI_CONFIG_MASK	(3 << AO_SPI_CONFIG_BIT)

#define AO_SPI_CPOL_BIT		6
#define AO_SPI_CPHA_BIT		7

#define AO_SPI_DOPO_BIT		8
#define  AO_SPI_DOPO_MOSI_0_SCLK_1	(0 << AO_SPI_DOPO_BIT)
#define  AO_SPI_DOPO_MOSI_2_SCLK_3	(1 << AO_SPI_DOPO_BIT)
#define  AO_SPI_DOPO_MOSI_3_SCLK_1	(2 << AO_SPI_DOPO_BIT)
#define  AO_SPI_DOPO_MOSI_0_SCLK_3	(3 << AO_SPI_DOPO_BIT)
#define  AO_SPI_DOPO_MASK		(3 << AO_SPI_DOPO_BIT)

#define AO_SPI_DIPO_BIT		10
#define  AO_SPI_DIPO_MISO_0		(0 << AO_SPI_DIPO_BIT)
#define  AO_SPI_DIPO_MISO_1		(1 << AO_SPI_DIPO_BIT)
#define  AO_SPI_DIPO_MISO_2		(2 << AO_SPI_DIPO_BIT)
#define  AO_SPI_DIPO_MISO_3		(3 << AO_SPI_DIPO_BIT)
#define  AO_SPI_DIPO_MASK		(3 << AO_SPI_DIPO_MASK)

#define AO_SPI_CONFIG_0		(0 << AO_SPI_CONFIG_BIT)
#define AO_SPI_CONFIG_1		(1 << AO_SPI_CONFIG_BIT)
#define AO_SPI_CONFIG_2		(2 << AO_SPI_CONFIG_BIT)
#define AO_SPI_CONFIG_3		(3 << AO_SPI_CONFIG_BIT)

#define AO_SPI_INDEX(id)	((uint8_t) ((id) & AO_SPI_INDEX_MASK))
#define AO_SPI_CONFIG(id)	((id) & AO_SPI_CONFIG_MASK)
#define AO_SPI_PIN_CONFIG(id)	((id) & (AO_SPI_INDEX_MASK | AO_SPI_CONFIG_MASK))
#define AO_SPI_CPOL(id)		((uint32_t) (((id) >> AO_SPI_CPOL_BIT) & 1))
#define AO_SPI_CPHA(id)		((uint32_t) (((id) >> AO_SPI_CPHA_BIT) & 1))
#define AO_SPI_DOPO(id)		((uint32_t) (((id) >> AO_SPI_DOPO_BIT) & 3))
#define AO_SPI_DIPO(id)		((uint32_t) (((id) >> AO_SPI_DIPO_BIT) & 3))

#define AO_SPI_MAKE_MODE(pol,pha)	(((pol) << AO_SPI_CPOL_BIT) | ((pha) << AO_SPI_CPHA_BIT))
#define AO_SPI_MODE_0		AO_SPI_MAKE_MODE(0,0)
#define AO_SPI_MODE_1		AO_SPI_MAKE_MODE(0,1)
#define AO_SPI_MODE_2		AO_SPI_MAKE_MODE(1,0)
#define AO_SPI_MODE_3		AO_SPI_MAKE_MODE(1,1)

#if HAS_SPI_0
/*
 * PA08 SERCOM0.0 -> MOSI	(DOPO 0)
 * PA09 SERCOM0.1 -> SCLK	(DOPO 0)
 * PA10 SERCOM0.2 -> MISO	(DIPO 2)
 */
#define AO_SPI_0_PA08_PA09_PA10	(0 | AO_SPI_CONFIG_0 |		\
				 AO_SPI_DOPO_MOSI_0_SCLK_1 |	\
				 AO_SPI_DIPO_MISO_2)
/*
 * PA04 SERCOM0.0 -> MOSI	(DOPO 0)
 * PA05 SERCOM0.1 -> SCLK	(DOPO 0)
 * PA06 SERCOM0.2 -> MISO	(DIPO 2)
 */
#define AO_SPI_0_PA04_PA05_PA06	(0 | AO_SPI_CONFIG_1 |		\
				 AO_SPI_DOPO_MOSI_0_SCLK_1 |	\
				 AO_SPI_DIPO_MISO_2)
#endif /* HAS_SPI_0 */

#if HAS_SPI_3
/*
 * PA22 SERCOM3.0 -> MOSI	(DOPO 0)
 * PA23 SERCOM3.1 -> SCLK	(DOPO 0)
 * PA20 SERCOM3.2 -> MISO	(DIPO 2)
 */
#define AO_SPI_3_PA22_PA23_PA20	(3 | AO_SPI_CONFIG_0 |		\
				 AO_SPI_DOPO_MOSI_0_SCLK_1 |	\
				 AO_SPI_DIPO_MISO_2)
#endif /* HAS_SPI_3 */

#if HAS_SPI_4
/*
 * PA04 SERCOM0.0 -> MOSI	(DOPO 0)
 * PA05 SERCOM0.1 -> SCLK	(DOPO 0)
 * PA16 SERCOM0.2 -> MISO	(DIPO 2)
 */
#define AO_SPI_CONFIG_PA04_PA05_PA06	(0 | AO_SPI_CONFIG_1 |		\
					 AO_SPI_DOPO_MOSI_0_SCLK_1 |	\
					 AO_SPI_DIPO_MISO_2)

/*
 * PB10 SERCOM4.2 -> MOSI	(DOPO 1)
 * PB11 SERCOM4.3 -> SCLK	(DOPO 1)
 * PA12	SERCOM4.0 -> MISO	(DIPO 0)
 */
#define AO_SPI_4_PB10_PB11_PA12	(4 | AO_SPI_CONFIG_0 |		\
				 AO_SPI_DOPO_MOSI_2_SCLK_3 |	\
				 AO_SPI_DIPO_MISO_0)
#endif /* HAS_SPI_4 */

#if HAS_SPI_5
/*
 * PB22 SERCOM5.2 -> MOSI	(DOPO 1)
 * PB23 SERCOM5.3 -> SCLK	(DOPO 1)
 * PB03 SERCOM5.1 -> MISO	(DIPO 1)
 */
#define AO_SPI_5_PB22_PB23_PB03	(5 | AO_SPI_CONFIG_0 |		\
				 AO_SPI_DOPO_MOSI_2_SCLK_3 |	\
				 AO_SPI_DIPO_MISO_1)
#endif /* HAS_SPI_5 */

void
ao_spi_send(const void *block, uint16_t len, uint16_t spi_index);

void
ao_spi_send_fixed(uint8_t data, uint16_t len, uint16_t spi_index);

void
ao_spi_recv(void *block, uint16_t len, uint16_t spi_index);

void
ao_spi_duplex(const void *out, void *in, uint16_t len, uint16_t spi_index);

void
ao_spi_get(uint16_t spi_index, uint32_t speed);

void
ao_spi_put(uint16_t spi_index);

void
ao_spi_init(void);

#define ao_spi_set_cs(reg,mask) do {		\
		reg->outclr = mask;		\
	} while(0)

#define ao_spi_clr_cs(reg,mask) do {		\
		reg->outset = mask;		\
	} while(0)

#define ao_spi_get_mask(reg,mask,spi_index, speed) do {		\
		ao_spi_get(spi_index, speed);				\
		ao_spi_set_cs(reg,mask);			\
	} while (0)

#define ao_spi_put_mask(reg,mask,spi_index) do {	\
		ao_spi_clr_cs(reg,mask);	\
		ao_spi_put(spi_index);		\
	} while (0)

static inline void
ao_spi_get_bit(struct samd21_port *port, uint8_t bit, uint16_t spi_index, uint32_t speed)
{
	ao_spi_get(spi_index, speed);
	ao_gpio_set(port, bit, 0);
}

static inline void
ao_spi_put_bit(struct samd21_port *port, uint8_t bit, uint16_t spi_index)
{
	ao_gpio_set(port, bit, 1);
	ao_spi_put(spi_index);
}

static inline uint8_t
ao_spi_speed(uint32_t hz)
{
	int32_t	baud = (int32_t) (AO_SYSCLK / (2 * hz)) - 1;

	if (baud < 1)
		baud = 1;
	if (baud > 255)
		baud = 255;
	return (uint8_t) baud;
}

#define ao_spi_init_cs(port, mask) do {					\
		uint8_t __bit__;					\
		for (__bit__ = 0; __bit__ < 32; __bit__++) {		\
			if (mask & (1 << __bit__))			\
				ao_enable_output(port, __bit__, 1); \
		}							\
	} while (0)

#define ARM_PUSH32(stack, val)	(*(--(stack)) = (val))

typedef uint32_t	ao_arch_irq_t;

static inline uint32_t
ao_arch_irqsave(void) {
	uint32_t	primask;
	asm("mrs %0,primask" : "=&r" (primask));
	ao_arch_block_interrupts();
	return primask;
}

static inline void
ao_arch_irqrestore(uint32_t primask) {
	asm("msr primask,%0" : : "r" (primask));
}

static inline void
ao_arch_memory_barrier(void) {
	asm volatile("" ::: "memory");
}

#if HAS_TASK
static inline void
ao_arch_init_stack(struct ao_task *task, uint32_t *sp, void *start)
{
	uint32_t	a = (uint32_t) start;
	int		i;

	/* Return address (goes into LR) */
	ARM_PUSH32(sp, a);

	/* Clear register values r0-r7 */
	i = 8;
	while (i--)
		ARM_PUSH32(sp, 0);

	/* APSR */
	ARM_PUSH32(sp, 0);

	/* PRIMASK with interrupts enabled */
	ARM_PUSH32(sp, 0);

	task->sp32 = sp;
}

static inline void ao_arch_save_regs(void) {
	/* Save general registers */
	asm("push {r0-r7,lr}\n");

	/* Save APSR */
	asm("mrs r0,apsr");
	asm("push {r0}");

	/* Save PRIMASK */
	asm("mrs r0,primask");
	asm("push {r0}");
}

static inline void ao_arch_save_stack(void) {
	uint32_t	*sp;
	asm("mov %0,sp" : "=&r" (sp) );
	ao_cur_task->sp32 = (sp);
	if (sp < &ao_cur_task->stack32[0])
		ao_panic (AO_PANIC_STACK);
}

static inline void ao_arch_restore_stack(void) {
	/* Switch stacks */
	asm("mov sp, %0" : : "r" (ao_cur_task->sp32) );

	/* Restore PRIMASK */
	asm("pop {r0}");
	asm("msr primask,r0");

	/* Restore APSR */
	asm("pop {r0}");
	asm("msr apsr_nczvq,r0");

	/* Restore general registers */
	asm("pop {r0-r7,pc}\n");
}

#ifndef HAS_SAMPLE_PROFILE
#define HAS_SAMPLE_PROFILE 0
#endif

#if !HAS_SAMPLE_PROFILE
#define HAS_ARCH_START_SCHEDULER	1

static inline void ao_arch_start_scheduler(void) {
	uint32_t	sp;
	uint32_t	control;

	asm("mrs %0,msp" : "=&r" (sp));
	asm("msr psp,%0" : : "r" (sp));
	asm("mrs %0,control" : "=&r" (control));
	control |= (1 << 1);
	asm("msr control,%0" : : "r" (control));
	asm("isb");
}
#endif

#define ao_arch_isr_stack()

#endif

#define ao_arch_wait_interrupt() do {				\
		asm("\twfi\n");					\
		ao_arch_release_interrupts();			\
		asm(".global ao_idle_loc\nao_idle_loc:");	\
		ao_arch_block_interrupts();			\
	} while (0)

#define ao_arch_critical(b) do {			\
		uint32_t __mask = ao_arch_irqsave();	\
		do { b } while (0);			\
		ao_arch_irqrestore(__mask);		\
	} while (0)

/* ao_serial_samd21.c */

#if USE_SERIAL_0_FLOW && USE_SERIAL_0_SW_FLOW || USE_SERIAL_1_FLOW && USE_SERIAL_1_SW_FLOW
#define HAS_SERIAL_SW_FLOW	1
#else
#define HAS_SERIAL_SW_FLOW	0
#endif

#if USE_SERIAL_1_FLOW && !USE_SERIAL_1_SW_FLOW
#define USE_SERIAL_1_HW_FLOW	1
#endif

#if USE_SERIAL_0_FLOW && !USE_SERIAL_0_SW_FLOW
#define USE_SERIAL_0_HW_FLOW	1
#endif

#if USE_SERIAL_0_HW_FLOW || USE_SERIAL_1_HW_FLOW
#define HAS_SERIAL_HW_FLOW	1
#else
#define HAS_SERIAL_HW_FLOW	0
#endif

struct ao_samd21_usart {
	struct ao_fifo		rx_fifo;
	struct ao_fifo		tx_fifo;
	struct samd21_sercom	*reg;
	uint8_t			tx_running;
	uint8_t			draining;
#if HAS_SERIAL_SW_FLOW
	/* RTS - 0 if we have FIFO space, 1 if not
	 * CTS - 0 if we can send, 0 if not
	 */
	struct samd21_port	*gpio_rts;
	struct samd21_port	*gpio_cts;
	uint8_t			pin_rts;
	uint8_t			pin_cts;
	uint8_t			rts;
#endif
};

#if HAS_USART_0
extern struct ao_samd21_usart	ao_samd21_usart0;
#endif

void
ao_serial_init(void);

/* ao_usb_samd21.c */

#if AO_USB_OUT_HOOK
void
ao_usb_out_hook(uint8_t *buffer, uint16_t count);
#endif

void start(void);

#endif /* _AO_ARCH_FUNCS_H_ */
