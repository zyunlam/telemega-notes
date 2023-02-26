/*
 * Copyright © 2023 Keith Packard <keithp@keithp.com>
 *
 * This program is free software; you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation, either version 2 of the License, or
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

#define AO_EXTI_MODE_RISING	1
#define AO_EXTI_MODE_FALLING	2
#define AO_EXTI_MODE_PULL_NONE	0
#define AO_EXTI_MODE_PULL_UP	4
#define AO_EXTI_MODE_PULL_DOWN	8
#define AO_EXTI_PRIORITY_LOW	16
#define AO_EXTI_PRIORITY_MED	0
#define AO_EXTI_PRIORITY_HIGH	32
#define AO_EXTI_PIN_NOCONFIGURE	64

static inline void
ao_enable_port(struct stm_gpio *port)
{
	if ((port) == &stm_gpioa)
		stm_rcc.apb2enr |= (1 << STM_RCC_APB2ENR_IOPAEN);
	else if ((port) == &stm_gpiob)
		stm_rcc.apb2enr |= (1 << STM_RCC_APB2ENR_IOPBEN);
	else if ((port) == &stm_gpioc)
		stm_rcc.apb2enr |= (1 << STM_RCC_APB2ENR_IOPCEN);
	else if ((port) == &stm_gpiod)
		stm_rcc.apb2enr |= (1 << STM_RCC_APB2ENR_IOPDEN);
	else if ((port) == &stm_gpioe)
		stm_rcc.apb2enr |= (1 << STM_RCC_APB2ENR_IOPEEN);
}

static inline void
ao_disable_port(struct stm_gpio *port)
{
	if ((port) == &stm_gpioa)
		stm_rcc.apb2enr &= ~(1UL << STM_RCC_APB2ENR_IOPAEN);
	else if ((port) == &stm_gpiob)
		stm_rcc.apb2enr &= ~(1UL << STM_RCC_APB2ENR_IOPBEN);
	else if ((port) == &stm_gpioc)
		stm_rcc.apb2enr &= ~(1UL << STM_RCC_APB2ENR_IOPCEN);
	else if ((port) == &stm_gpiod)
		stm_rcc.apb2enr &= ~(1UL << STM_RCC_APB2ENR_IOPDEN);
	else if ((port) == &stm_gpioe)
		stm_rcc.apb2enr &= ~(1UL << STM_RCC_APB2ENR_IOPEEN);
}

#define ao_gpio_set(port, bit, v) stm_gpio_set(port, bit, v)

#define ao_gpio_get(port, bit) stm_gpio_get(port, bit)

#define ao_gpio_set_bits(port, bits) stm_gpio_set_bits(port, bits)

#define ao_gpio_set_mask(port, bits, mask) stm_gpio_set_mask(port, bits, mask)

#define ao_gpio_clr_bits(port, bits) stm_gpio_clr_bits(port, bits);

#define ao_gpio_get_all(port) stm_gpio_get_all(port)

static inline void
ao_enable_output(struct stm_gpio *port, int bit, uint8_t v)
{
	ao_enable_port(port);
	ao_gpio_set(port, bit, v);
	stm_gpio_conf(port, bit,
		      STM_GPIO_CR_MODE_OUTPUT_10MHZ,
		      STM_GPIO_CR_CNF_OUTPUT_PUSH_PULL);
}

static inline void
ao_gpio_set_mode(struct stm_gpio *port, int bit, int mode)
{
	uint8_t cnf;

	if (mode == AO_EXTI_MODE_PULL_NONE)
		cnf = STM_GPIO_CR_CNF_INPUT_FLOATING;
	else {
		cnf = STM_GPIO_CR_CNF_INPUT_PULL;
		ao_gpio_set(port, bit, mode == AO_EXTI_MODE_PULL_UP);
	}
	stm_gpio_conf(port, bit,
		      STM_GPIO_CR_MODE_INPUT,
		      cnf);
}

static inline void
ao_enable_input(struct stm_gpio *port, int bit, int mode)
{
	ao_enable_port(port);
	ao_gpio_set_mode(port, bit, mode);
}

#if USE_SERIAL_1_SW_FLOW || USE_SERIAL_2_SW_FLOW || USE_SERIAL_3_SW_FLOW
#define HAS_SERIAL_SW_FLOW 1
#else
#define HAS_SERIAL_SW_FLOW 0
#endif

#if USE_SERIAL_1_FLOW && !USE_SERIAL_1_SW_FLOW || USE_SERIAL_2_FLOW && !USE_SERIAL_2_SW_FLOW || USE_SERIAL_3_FLOW && !USE_SERIAL_3_SW_FLOW
#define HAS_SERIAL_HW_FLOW 1
#else
#define HAS_SERIAL_HW_FLOW 0
#endif

/* ao_serial_stm.c */
struct ao_stm_usart {
	struct ao_fifo		rx_fifo;
	struct ao_fifo		tx_fifo;
	struct stm_usart	*reg;
	uint8_t			tx_running;
	uint8_t			draining;
#if HAS_SERIAL_SW_FLOW
	/* RTS - 0 if we have FIFO space, 1 if not
	 * CTS - 0 if we can send, 0 if not
	 */
	struct stm_gpio		*gpio_rts;
	struct stm_gpio		*gpio_cts;
	uint8_t			pin_rts;
	uint8_t			pin_cts;
	uint8_t			rts;
#endif
};

void
ao_debug_out(char c);

#if HAS_SERIAL_1
extern struct ao_stm_usart	ao_stm_usart1;
#endif

#if HAS_SERIAL_2
extern struct ao_stm_usart	ao_stm_usart2;
#endif

#if HAS_SERIAL_3
extern struct ao_stm_usart	ao_stm_usart3;
#endif

#define ARM_PUSH32(stack, val)	(*(--(stack)) = (val))

typedef uint32_t	ao_arch_irq_t;

static inline void
ao_arch_block_interrupts(void) {
#ifdef AO_NONMASK_INTERRUPTS
	asm("msr basepri,%0" : : "r" (AO_STM_NVIC_BASEPRI_MASK));
#else
	asm("cpsid i");
#endif
}

static inline void
ao_arch_release_interrupts(void) {
#ifdef AO_NONMASK_INTERRUPTS
	asm("msr basepri,%0" : : "r" (0x0));
#else
	asm("cpsie i");
#endif
}

static inline uint32_t
ao_arch_irqsave(void) {
	uint32_t	val;
#ifdef AO_NONMASK_INTERRUPTS
	asm("mrs %0,basepri" : "=r" (val));
#else
	asm("mrs %0,primask" : "=r" (val));
#endif
	ao_arch_block_interrupts();
	return val;
}

static inline void
ao_arch_irqrestore(uint32_t basepri) {
#ifdef AO_NONMASK_INTERRUPTS
	asm("msr basepri,%0" : : "r" (basepri));
#else
	asm("msr primask,%0" : : "r" (basepri));
#endif
}

static inline void
ao_arch_memory_barrier(void) {
	asm volatile("" ::: "memory");
}

static inline void
ao_arch_irq_check(void) {
#ifdef AO_NONMASK_INTERRUPTS
	uint32_t	basepri;
	asm("mrs %0,basepri" : "=r" (basepri));
	if (basepri == 0)
		ao_panic(AO_PANIC_IRQ);
#else
	uint32_t	primask;
	asm("mrs %0,primask" : "=r" (primask));
	if ((primask & 1) == 0)
		ao_panic(AO_PANIC_IRQ);
#endif
}

#if HAS_TASK
static inline void
ao_arch_init_stack(struct ao_task *task, uint32_t *sp, void *start)
{
	uint32_t	a = (uint32_t) start;
	int		i;

	/* Return address (goes into LR) */
	ARM_PUSH32(sp, a);

	/* Clear register values r0-r12 */
	i = 13;
	while (i--)
		ARM_PUSH32(sp, 0);

	/* APSR */
	ARM_PUSH32(sp, 0);

	/* BASEPRI with interrupts enabled */
	ARM_PUSH32(sp, 0);

	task->sp32 = sp;
}

static inline void ao_arch_save_regs(void) {
	/* Save general registers */
	asm("push {r0-r12,lr}\n");

	/* Save APSR */
	asm("mrs r0,apsr");
	asm("push {r0}");

#ifdef AO_NONMASK_INTERRUPTS
	/* Save BASEPRI */
	asm("mrs r0,basepri");
#else
	/* Save PRIMASK */
	asm("mrs r0,primask");
#endif
	asm("push {r0}");
}

static inline void ao_arch_save_stack(void) {
	uint32_t	*sp;
	asm("mov %0,sp" : "=&r" (sp) );
	ao_cur_task->sp32 = (sp);
}

static inline void ao_arch_restore_stack(void) {
	/* Switch stacks */
	asm("mov sp, %0" : : "r" (ao_cur_task->sp32) );

#ifdef AO_NONMASK_INTERRUPTS
	/* Restore BASEPRI */
	asm("pop {r0}");
	asm("msr basepri,r0");
#else
	/* Restore PRIMASK */
	asm("pop {r0}");
	asm("msr primask,r0");
#endif

	/* Restore APSR */
	asm("pop {r0}");
	asm("msr apsr_nczvq,r0");

	/* Restore general registers */
	asm("pop {r0-r12,lr}\n");

	/* Return to calling function */
	asm("bx lr");
}
#define HAS_ARCH_START_SCHEDULER	1

static inline void ao_arch_start_scheduler(void) {
	uint32_t	sp;
	uint32_t	control;

	asm("mrs %0,msp" : "=&r" (sp));
	asm("msr psp,%0" : : "r" (sp));
	asm("mrs %0,control" : "=r" (control));
	control |= (1 << 1);
	asm("msr control,%0" : : "r" (control));
	asm("isb");
}

#define ao_arch_isr_stack()

#endif /* HAS_TASK */

static inline void
ao_arch_wait_interrupt(void) {
#ifdef AO_NONMASK_INTERRUPTS
	asm(
	    "dsb\n"			/* Serialize data */
	    "isb\n"			/* Serialize instructions */
	    "cpsid i\n"			/* Block all interrupts */
	    "msr basepri,%0\n"		/* Allow all interrupts through basepri */
	    "wfi\n"			/* Wait for an interrupt */
	    "cpsie i\n"			/* Allow all interrupts */
	    "msr basepri,%1\n"		/* Block interrupts through basepri */
	    : : "r" (0), "r" (AO_STM_NVIC_BASEPRI_MASK));
#else
	asm("\twfi\n");
	ao_arch_release_interrupts();
	ao_arch_block_interrupts();
#endif
}

#define ao_arch_critical(b) do {			\
		uint32_t __mask = ao_arch_irqsave();	\
		do { b } while (0);			\
		ao_arch_irqrestore(__mask);		\
	} while (0)

#define ao_arch_reboot() \
	(stm_scb.aircr = ((STM_SCB_AIRCR_VECTKEY_KEY << STM_SCB_AIRCR_VECTKEY) | \
			  (1 << STM_SCB_AIRCR_SYSRESETREQ)))

#endif /* _AO_ARCH_FUNCS_H_ */
