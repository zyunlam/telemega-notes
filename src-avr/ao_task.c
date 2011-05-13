/*
 * Copyright © 2009 Keith Packard <keithp@keithp.com>
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

#include "ao.h"

#define AO_NO_TASK_INDEX	0xff

__xdata struct ao_task * __xdata ao_tasks[AO_NUM_TASKS];
__data uint8_t ao_num_tasks;
__data uint8_t ao_cur_task_index;
__xdata struct ao_task *__data ao_cur_task;

#ifdef AVR

#define PUSH8(stack, val)	(*((stack)--) = (val))

static void
ao_init_stack(__xdata struct ao_task *task, void (*start)(void))
{
	uint8_t		*stack = task->stack + AO_STACK_SIZE - 1;
	uint16_t	a = (uint16_t) start;
	int		i;

	/* Return address */
	PUSH8(stack, a);
	PUSH8(stack, (a >> 8));

	/* Clear register values */
	i = 32;
	while (i--)
		PUSH8(stack, 0);

	/* SREG with interrupts enabled */
	PUSH8(stack, 0x80);
	task->stack_count = stack - task->stack;
}
#else
static void
ao_init_stack(__xdata struct ao_task *task, void (*start)(void))
{
	uint8_t	__xdata *stack = task->stack;
	/*
	 * Construct a stack frame so that it will 'return'
	 * to the start of the task
	 */

	*stack++ = ((uint16_t) start);
	*stack++ = ((uint16_t) start) >> 8;

	/* and the stuff saved by ao_switch */
	*stack++ = 0;		/* acc */
	*stack++ = 0x80;	/* IE */
	*stack++ = 0;		/* DPL */
	*stack++ = 0;		/* DPH */
	*stack++ = 0;		/* B */
	*stack++ = 0;		/* R2 */
	*stack++ = 0;		/* R3 */
	*stack++ = 0;		/* R4 */
	*stack++ = 0;		/* R5 */
	*stack++ = 0;		/* R6 */
	*stack++ = 0;		/* R7 */
	*stack++ = 0;		/* R0 */
	*stack++ = 0;		/* R1 */
	*stack++ = 0;		/* PSW */
	*stack++ = 0;		/* BP */
	task->stack_count = stack - task->stack;
}
#endif

void
ao_add_task(__xdata struct ao_task * task, void (*start)(void), __code char *name) __reentrant
{
	uint8_t task_id;
	uint8_t t;
	if (ao_num_tasks == AO_NUM_TASKS)
		ao_panic(AO_PANIC_NO_TASK);
	for (task_id = 1; task_id != 0; task_id++) {
		for (t = 0; t < ao_num_tasks; t++)
			if (ao_tasks[t]->task_id == task_id)
				break;
		if (t == ao_num_tasks)
			break;
	}
	ao_tasks[ao_num_tasks++] = task;
	task->task_id = task_id;
	task->name = name;
	task->wchan = NULL;
	ao_init_stack(task, start);
}

/* Task switching function. This must not use any stack variables */
void
ao_yield(void) __naked
{
#ifdef AVR
	asm("push r31" "\n\t" "push r30");
	asm("push r29" "\n\t" "push r28" "\n\t" "push r27" "\n\t" "push r26" "\n\t" "push r25");
	asm("push r24" "\n\t" "push r23" "\n\t" "push r22" "\n\t" "push r21" "\n\t" "push r20");
	asm("push r19" "\n\t" "push r18" "\n\t" "push r17" "\n\t" "push r16" "\n\t" "push r15");
	asm("push r14" "\n\t" "push r13" "\n\t" "push r12" "\n\t" "push r11" "\n\t" "push r10");
	asm("push r9" "\n\t" "push r8" "\n\t" "push r7" "\n\t" "push r6" "\n\t" "push r5");
	asm("push r4" "\n\t" "push r3" "\n\t" "push r2" "\n\t" "push r1" "\n\t" "push r0");
	asm("in r0, __SREG__" "\n\t" "push r0");
	sei();
#else
	/* Save current context */
	_asm
		/* Push ACC first, as when restoring the context it must be restored
		 * last (it is used to set the IE register). */
		push	ACC
		/* Store the IE register then enable interrupts. */
		push	_IEN0
		setb	_EA
		push	DPL
		push	DPH
		push	b
		push	ar2
		push	ar3
		push	ar4
		push	ar5
		push	ar6
		push	ar7
		push	ar0
		push	ar1
		push	PSW
	_endasm;
	PSW = 0;
	_asm
		push	_bp
	_endasm;
#endif

	if (ao_cur_task_index == AO_NO_TASK_INDEX)
		ao_cur_task_index = ao_num_tasks-1;
	else
	{
#ifdef AVR
		uint8_t	sp_l, sp_h;
		uint8_t *sp;
		asm("in %0,__SP_L__" : "=&r" (sp_l) );
		asm("in %0,__SP_H__" : "=&r" (sp_h) );
		sp = (uint8_t *) ((uint16_t) sp_l | ((uint16_t) sp_h << 8));
		ao_cur_task->stack_count = sp - ao_cur_task->stack;
#else
		uint8_t stack_len;
		__data uint8_t *stack_ptr;
		__xdata uint8_t *save_ptr;
		/* Save the current stack */
		stack_len = SP - (AO_STACK_START - 1);
		ao_cur_task->stack_count = stack_len;
		stack_ptr = (uint8_t __data *) AO_STACK_START;
		save_ptr = (uint8_t __xdata *) ao_cur_task->stack;
		do
			*save_ptr++ = *stack_ptr++;
		while (--stack_len);
#endif
	}

#ifndef AVR
	/* Empty the stack; might as well let interrupts have the whole thing */
	SP = AO_STACK_START - 1;
#endif

	/* Find a task to run. If there isn't any runnable task,
	 * this loop will run forever, which is just fine
	 */
	{
		__pdata uint8_t	ao_next_task_index = ao_cur_task_index;
		for (;;) {
			++ao_next_task_index;
			if (ao_next_task_index == ao_num_tasks)
				ao_next_task_index = 0;

			ao_cur_task = ao_tasks[ao_next_task_index];
			if (ao_cur_task->wchan == NULL) {
				ao_cur_task_index = ao_next_task_index;
				break;
			}

			/* Check if the alarm is set for a time which has passed */
			if (ao_cur_task->alarm &&
			    (int16_t) (ao_time() - ao_cur_task->alarm) >= 0) {
				ao_cur_task_index = ao_next_task_index;
				break;
			}

#ifdef AVR
#else
			/* Enter lower power mode when there isn't anything to do */
			if (ao_next_task_index == ao_cur_task_index)
				PCON = PCON_IDLE;
#endif
		}
	}

#ifdef AVR
	{
		uint8_t	*sp = (ao_cur_task->stack + ao_cur_task->stack_count);
		uint8_t	sp_l, sp_h;
		sp_l = (uint16_t) sp;
		sp_h = ((uint16_t) sp) >> 8;
		asm("out __SP_H__,%0" : : "r" (sp_h) );
		asm("out __SP_L__,%0" : : "r" (sp_l) );
		asm("pop r0"	"\n\t"
		    "out __SREG__, r0");
		asm("pop r0" "\n\t" "pop r1" "\n\t" "pop r2" "\n\t" "pop r3" "\n\t" "pop r4");
		asm("pop r5" "\n\t" "pop r6" "\n\t" "pop r7" "\n\t" "pop r8" "\n\t" "pop r9");
		asm("pop r10" "\n\t" "pop r11" "\n\t" "pop r12" "\n\t" "pop r13" "\n\t" "pop r14");
		asm("pop r15" "\n\t" "pop r16" "\n\t" "pop r17" "\n\t" "pop r18" "\n\t" "pop r19");
		asm("pop r20" "\n\t" "pop r21" "\n\t" "pop r22" "\n\t" "pop r23" "\n\t" "pop r24");
		asm("pop r25" "\n\t" "pop r26" "\n\t" "pop r27" "\n\t" "pop r28" "\n\t" "pop r29");
		asm("pop r30" "\n\t" "pop r31");
		asm("ret");
	}
#else
	{
		uint8_t stack_len;
		__data uint8_t *stack_ptr;
		__xdata uint8_t *save_ptr;

		/* Restore the old stack */
		stack_len = ao_cur_task->stack_count;
		SP = AO_STACK_START - 1 + stack_len;

		stack_ptr = (uint8_t __data *) AO_STACK_START;
		save_ptr = (uint8_t __xdata *) ao_cur_task->stack;
		do
			*stack_ptr++ = *save_ptr++;
		while (--stack_len);
	}

	_asm
		pop		_bp
		pop		PSW
		pop		ar1
		pop		ar0
		pop		ar7
		pop		ar6
		pop		ar5
		pop		ar4
		pop		ar3
		pop		ar2
		pop		b
		pop		DPH
		pop		DPL
		/* The next byte of the stack is the IE register.  Only the global
		enable bit forms part of the task context.  Pop off the IE then set
		the global enable bit to match that of the stored IE register. */
		pop		ACC
		JB		ACC.7,0098$
		CLR		_EA
		LJMP	0099$
	0098$:
		SETB		_EA
	0099$:
		/* Finally pop off the ACC, which was the first register saved. */
		pop		ACC
		ret
	_endasm;
#endif
}

uint8_t
ao_sleep(__xdata void *wchan)
{
	__critical {
		ao_cur_task->wchan = wchan;
	}
	ao_yield();
	ao_cur_task->alarm = 0;
	if (ao_cur_task->wchan) {
		ao_cur_task->wchan = NULL;
		return 1;
	}
	return 0;
}

void
ao_wakeup(__xdata void *wchan)
{
	uint8_t	i;

	for (i = 0; i < ao_num_tasks; i++)
		if (ao_tasks[i]->wchan == wchan)
			ao_tasks[i]->wchan = NULL;
}

void
ao_alarm(uint16_t delay)
{
	/* Make sure we sleep *at least* delay ticks, which means adding
	 * one to account for the fact that we may be close to the next tick
	 */
	if (!(ao_cur_task->alarm = ao_time() + delay + 1))
		ao_cur_task->alarm = 1;
}

void
ao_exit(void) __critical
{
	uint8_t	i;
	ao_num_tasks--;
	for (i = ao_cur_task_index; i < ao_num_tasks; i++)
		ao_tasks[i] = ao_tasks[i+1];
	ao_cur_task_index = AO_NO_TASK_INDEX;
	ao_yield();
	/* we'll never get back here */
}

void
ao_task_info(void)
{
	uint8_t	i;
	uint8_t pc_loc;
	__xdata struct ao_task *task;

	for (i = 0; i < ao_num_tasks; i++) {
		task = ao_tasks[i];
		pc_loc = task->stack_count - 17;
		printf("%12s: wchan %04x pc %04x\n",
		       task->name,
		       (int16_t) task->wchan,
		       (task->stack[pc_loc]) | (task->stack[pc_loc+1] << 8));
	}
}

void
ao_start_scheduler(void)
{
	ao_cur_task_index = AO_NO_TASK_INDEX;
	ao_cur_task = NULL;
	ao_yield();
}
