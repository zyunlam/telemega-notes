/*
 * Copyright © 2009 Keith Packard <keithp@keithp.com>
 *
 * This program is free software; you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation; version 2 of the License
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

#ifndef _AO_H_
#define _AO_H_

#include <stdint.h>
#include <stdio.h>
#include <string.h>
#include "cc1111.h"

/* Convert a __data pointer into an __xdata pointer */
#define DATA_TO_XDATA(a)	((void __xdata *) ((uint8_t) (a) | 0xff00))

/* Stack runs from above the allocated __data space to 0xfe, which avoids
 * writing to 0xff as that triggers the stack overflow indicator
 */
#define AO_STACK_START	0x7f
#define AO_STACK_END	0xfe
#define AO_STACK_SIZE	(AO_STACK_END - AO_STACK_START + 1)

/* An AltOS task */
struct ao_task {
	__xdata void *wchan;		/* current wait channel (NULL if running) */
	uint8_t	stack_count;		/* amount of saved stack */
	uint8_t task_id;		/* index in the task array */
	uint8_t	stack[AO_STACK_SIZE];	/* saved stack */
};

extern __xdata struct ao_task *__data ao_cur_task;

#define AO_NUM_TASKS		10	/* maximum number of tasks */
#define AO_NO_TASK		0	/* no task id */

/*
 ao_task.c
 */

/* Suspend the current task until wchan is awoken */
int
ao_sleep(__xdata void *wchan);

/* Wake all tasks sleeping on wchan */
int
ao_wakeup(__xdata void *wchan);

/* Yield the processor to another task */
void
ao_yield(void) _naked;

/* Add a task to the run queue */
void
ao_add_task(__xdata struct ao_task * task, void (*start)(void));

/* Start the scheduler. This will not return */
void
ao_start_scheduler(void);

/*
 * ao_panic.c
 */

#define AO_PANIC_NO_TASK	1	/* AO_NUM_TASKS is not large enough */
#define AO_PANIC_DMA		2	/* Attempt to start DMA while active */
#define AO_PANIC_MUTEX		3	/* Mis-using mutex API */
#define AO_PANIC_EE		4	/* Mis-using eeprom API */
#define AO_PANIC_LOG		5	/* Failing to read/write log data */

/* Stop the operating system, beeping and blinking the reason */
void
ao_panic(uint8_t reason);

/*
 * ao_timer.c
 */

/* Our timer runs at 100Hz */
#define AO_MS_TO_TICKS(ms)	((ms) / 10)
#define AO_SEC_TO_TICKS(s)	((s) * 100)

/* Returns the current time in ticks */
uint16_t
ao_time(void);

/* Suspend the current task until ticks time has passed */
void
ao_delay(uint16_t ticks);

/* Timer interrupt */
void
ao_timer_isr(void) interrupt 9;

/* Initialize the timer */
void
ao_timer_init(void);

/*
 * ao_adc.c
 */

#define AO_ADC_RING	128

/*
 * One set of samples read from the A/D converter
 */
struct ao_adc {
	uint16_t	tick;		/* tick when the sample was read */
	int16_t		accel;		/* accelerometer */
	int16_t		pres;		/* pressure sensor */
	int16_t		temp;		/* temperature sensor */
	int16_t		v_batt;		/* battery voltage */
	int16_t		sense_d;	/* drogue continuity sense */
	int16_t		sense_m;	/* main continuity sense */
};

/*
 * A/D data is stored in a ring, with the next sample to be written
 * at ao_adc_head
 */
extern volatile __xdata struct ao_adc	ao_adc_ring[AO_ADC_RING];
extern volatile __data uint8_t		ao_adc_head;

/* Trigger a conversion sequence (called from the timer interrupt) */
void
ao_adc_poll(void);
 
/* Suspend the current task until another A/D sample is converted */
void
ao_adc_sleep(void);

/* Get a copy of the last complete A/D sample set */
void
ao_adc_get(__xdata struct ao_adc *packet);

/* The A/D interrupt handler */
void
ao_adc_isr(void) interrupt 1;

/* Initialize the A/D converter */
void
ao_adc_init(void);

/*
 * ao_beep.c
 */

/*
 * Various pre-defined beep frequencies
 *
 * frequency = 1/2 (24e6/32) / beep
 */

#define AO_BEEP_LOW	150	/* 2500Hz */
#define AO_BEEP_MID	94	/* 3989Hz */
#define AO_BEEP_HIGH	75	/* 5000Hz */
#define AO_BEEP_OFF	0	/* off */

#define AO_BEEP_g	240	/* 1562.5Hz */
#define AO_BEEP_gs	227	/* 1652Hz (1655Hz) */
#define AO_BEEP_aa	214	/* 1752Hz (1754Hz) */
#define AO_BEEP_bbf	202	/* 1856Hz (1858Hz) */
#define AO_BEEP_bb	190	/* 1974Hz (1969Hz) */
#define AO_BEEP_cc	180	/* 2083Hz (2086Hz) */
#define AO_BEEP_ccs	170	/* 2205Hz (2210Hz) */
#define AO_BEEP_dd	160	/* 2344Hz (2341Hz) */
#define AO_BEEP_eef	151	/* 2483Hz (2480Hz) */
#define AO_BEEP_ee	143	/* 2622Hz (2628Hz) */
#define AO_BEEP_ff	135	/* 2778Hz (2784Hz) */
#define AO_BEEP_ffs	127	/* 2953Hz (2950Hz) */
#define AO_BEEP_gg	120	/* 3125Hz */
#define AO_BEEP_ggs	113	/* 3319Hz (3311Hz) */
#define AO_BEEP_aaa	107	/* 3504Hz (3508Hz) */
#define AO_BEEP_bbbf	101	/* 3713Hz (3716Hz) */
#define AO_BEEP_bbb	95	/* 3947Hz (3937Hz) */
#define AO_BEEP_ccc	90	/* 4167Hz (4171Hz) */
#define AO_BEEP_cccs	85	/* 4412Hz (4419Hz) */
#define AO_BEEP_ddd	80	/* 4688Hz (4682Hz) */
#define AO_BEEP_eeef	76	/* 4934Hz (4961Hz) */
#define AO_BEEP_eee	71	/* 5282Hz (5256Hz) */
#define AO_BEEP_fff	67	/* 5597Hz (5568Hz) */
#define AO_BEEP_fffs	64	/* 5859Hz (5899Hz) */
#define AO_BEEP_ggg	60	/* 6250Hz */

/* Set the beeper to the specified tone */
void
ao_beep(uint8_t beep);

/* Turn on the beeper for the specified time */
void
ao_beep_for(uint8_t beep, uint16_t ticks);

/* Initialize the beeper */
void
ao_beep_init(void);

/*
 * ao_led.c
 */

#define AO_LED_NONE	0
#define AO_LED_GREEN	1
#define AO_LED_RED	2

/* Turn on the specified LEDs */
void
ao_led_on(uint8_t colors);

/* Turn off the specified LEDs */
void
ao_led_off(uint8_t colors);

/* Set all of the LEDs to the specified state */
void
ao_led_set(uint8_t colors);

/* Turn on the specified LEDs for the indicated interval */
void
ao_led_for(uint8_t colors, uint16_t ticks);

/* Initialize the LEDs */
void
ao_led_init(void);

/*
 * ao_usb.c
 */

/* Put one character to the USB output queue */
void
ao_usb_putchar(uint8_t c);

/* Get one character from the USB input queue */
uint8_t
ao_usb_getchar(void);

/* Flush the USB output queue */
void
ao_usb_flush(void);

/* USB interrupt handler */
void
ao_usb_isr(void) interrupt 6;

/* Initialize the USB system */
void
ao_usb_init(void);

/*
 * ao_cmd.c
 */
void
ao_cmd_init(void);

/*
 * ao_dma.c
 */

/* Allocate a DMA channel. the 'done' parameter will be set to 1
 * when the dma is finished and will be used to wakeup any waiters 
 */
uint8_t
ao_dma_alloc(__xdata uint8_t * done);

/* Setup a DMA channel */
void
ao_dma_set_transfer(uint8_t id,
		    void __xdata *srcaddr,
		    void __xdata *dstaddr,
		    uint16_t count,
		    uint8_t cfg0,
		    uint8_t cfg1);

/* Start a DMA channel */
void
ao_dma_start(uint8_t id);

/* Manually trigger a DMA channel */
void
ao_dma_trigger(uint8_t id);

/* Abort a running DMA transfer */
void
ao_dma_abort(uint8_t id);

/* DMA interrupt routine */
void
ao_dma_isr(void) interrupt 8;

/*
 * ao_mutex.c
 */

void
ao_mutex_get(__xdata uint8_t *ao_mutex) __reentrant;

void
ao_mutex_put(__xdata uint8_t *ao_mutex) __reentrant;

/*
 * ao_ee.c
 */

/*
 * We reserve the last block on the device for
 * configuration space. Writes and reads in this
 * area return errors.
 */

#define AO_EE_BLOCK_SIZE	((uint16_t) (256))
#define AO_EE_DEVICE_SIZE	((uint32_t) 128 * (uint32_t) 1024)
#define AO_EE_DATA_SIZE		(AO_EE_DEVICE_SIZE - (uint32_t) AO_EE_BLOCK_SIZE)
#define AO_EE_CONFIG_BLOCK	((uint16_t) (AO_EE_DATA_SIZE / AO_EE_BLOCK_SIZE))

void
ao_ee_flush(void) __reentrant;

/* Write to the eeprom */
uint8_t
ao_ee_write(uint32_t pos, uint8_t *buf, uint16_t len) __reentrant;

/* Read from the eeprom */
uint8_t
ao_ee_read(uint32_t pos, uint8_t *buf, uint16_t len) __reentrant;

/* Write the config block (at the end of the eeprom) */
uint8_t
ao_ee_write_config(uint8_t *buf, uint16_t len) __reentrant;

/* Read the config block (at the end of the eeprom) */
uint8_t
ao_ee_read_config(uint8_t *buf, uint16_t len) __reentrant;

/* Initialize the EEPROM code */
void
ao_ee_init(void);

/*
 * ao_log.c
 */

/*
 * The data log is recorded in the eeprom as a sequence
 * of data packets.
 *
 * Each packet starts with a 4-byte header that has the
 * packet type, the packet checksum and the tick count. Then
 * they all contain 2 16 bit values which hold packet-specific
 * data.
 * 
 * For each flight, the first packet
 * is FLIGHT packet, indicating the serial number of the
 * device and a unique number marking the number of flights
 * recorded by this device.
 *
 * During flight, data from the accelerometer and barometer
 * are recorded in SENSOR packets, using the raw 16-bit values
 * read from the A/D converter.
 *
 * Also during flight, but at a lower rate, the deployment
 * sensors are recorded in DEPLOY packets. The goal here is to
 * detect failure in the deployment circuits.
 *
 * STATE packets hold state transitions as the flight computer
 * transitions through different stages of the flight.
 */
#define AO_LOG_FLIGHT		'F'
#define AO_LOG_SENSOR		'A'
#define AO_LOG_TEMP_VOLT	'T'
#define AO_LOG_DEPLOY		'D'
#define AO_LOG_STATE		'S'

#define AO_LOG_POS_NONE		(~0UL)

struct ao_log_record {
	uint8_t			type;
	uint8_t			csum;
	uint16_t		tick;
	union {
		struct {
			uint16_t	serial;
			uint16_t	flight;
		} flight;
		struct {
			int16_t		accel;
			int16_t		pres;
		} sensor;
		struct {
			int16_t		temp;
			int16_t		v_batt;
		} temp_volt;
		struct {
			int16_t		drogue;
			int16_t		main;
		} deploy;
		struct {
			uint16_t	state;
			uint16_t	reason;
		} state;
		struct {
			uint16_t	d0;
			uint16_t	d1;
		} anon;
	} u;
};

/* Write a record to the eeprom log */
void
ao_log_data(struct ao_log_record *log);

/* Flush the log */
void
ao_log_flush(void);

/* Log dumping API:
 * ao_log_dump_first() - get first log record
 * ao_log_dump_next()  - get next log record
 */
extern __xdata struct ao_log_record ao_log_dump;

/* Retrieve first log record for the current flight */
uint8_t
ao_log_dump_first(void);

/* return next log record for the current flight */
uint8_t
ao_log_dump_next(void);

/* Logging thread main routine */
void
ao_log(void);

/* Start logging to eeprom */
void
ao_log_start(void);

/* Initialize the logging system */
void
ao_log_init(void);

/*
 * ao_flight.c
 */

enum ao_flight_state {
	ao_flight_startup,
	ao_flight_idle,
	ao_flight_launchpad,
	ao_flight_boost,
	ao_flight_coast,
	ao_flight_apogee,
	ao_flight_drogue,
	ao_flight_main,
	ao_flight_landed,
	ao_flight_invalid
};

extern __xdata struct ao_adc	ao_flight_data;
extern __data enum flight_state	ao_flight_state;
extern __data uint16_t			ao_flight_state_tick;
extern __data int16_t			ao_flight_accel;
extern __data int16_t			ao_flight_pres;
extern __data int16_t			ao_ground_pres;
extern __data int16_t			ao_ground_accel;
extern __data int16_t			ao_min_pres;
extern __data uint16_t			ao_launch_time;

/* Flight thread */
void
ao_flight(void);

/* Initialize flight thread */
void
ao_flight_init(void);

/*
 * ao_report.c
 */

void
ao_report_notify(void);

void
ao_report_init(void);

#endif /* _AO_H_ */
