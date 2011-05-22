/*
 * Copyright © 2011 Keith Packard <keithp@keithp.com>
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

static struct ao_task ao_log_task;

uint8_t ao_log_running;
uint8_t	ao_log_mutex;
uint32_t	ao_log_begin_pos, ao_log_end_pos;
uint32_t	ao_log_current_pos;

#define AO_LOG_TELESCIENCE_START	'b'
#define AO_LOG_TELESCIENCE_STOP		'e'
#define AO_LOG_TELESCIENCE_DATA		'd'

struct ao_log_telescience {
	uint8_t		type;
	uint8_t		csum;
	uint16_t	tick;
	union {
		uint8_t		bytes[28];
		uint16_t	adc[NUM_ADC];
	} u;
};

static struct ao_log_telescience log;
static uint8_t	ao_log_adc_pos;

static uint8_t
ao_log_csum(__xdata uint8_t *b) __reentrant
{
	uint8_t	sum = 0x5a;
	uint8_t	i;

	for (i = 0; i < sizeof (struct ao_log_telescience); i++)
		sum += *b++;
	return -sum;
}

uint8_t
ao_log_telescience_data(struct ao_log_telescience *log)
{
	uint8_t wrote = 0;

	log->csum = 0;
	log->csum = ao_log_csum((__xdata uint8_t *) log);
	ao_mutex_get(&ao_log_mutex); {
		if (ao_log_current_pos >= ao_log_end_pos && ao_log_running)
			ao_log_stop();
		if (ao_log_running) {
			wrote = 1;
			ao_storage_write(ao_log_current_pos,
					 log,
					 sizeof (struct ao_log_telescience));
			ao_log_current_pos += sizeof (struct ao_log_telescience);
		}
	} ao_mutex_put(&ao_log_mutex);
	return wrote;
}

void
ao_log_telescience(void)
{
	ao_storage_setup();

	ao_log_current_pos = 0;
	ao_log_end_pos = ao_storage_config;
	for (;;) {
		while (!ao_log_running)
			ao_sleep(&ao_log_running);

		memset(&log, '\0', sizeof (struct ao_log_telescience));
		log.type = AO_LOG_TELESCIENCE_START;
		log.tick = ao_time();
		ao_log_telescience_data(&log);
		/* Write the whole contents of the ring to the log
		 * when starting up.
		 */
		ao_log_adc_pos = ao_adc_ring_next(ao_adc_head);
		log.type = AO_LOG_TELESCIENCE_DATA;
		while (ao_log_running) {
			/* Write samples to EEPROM */
			while (ao_log_adc_pos != ao_adc_head) {
				log.tick = ao_adc_ring[ao_log_adc_pos].tick;
				memcpy(&log.u.adc, (void *) ao_adc_ring[ao_log_adc_pos].adc,
				       NUM_ADC * sizeof (uint16_t));
				ao_log_telescience_data(&log);
				ao_log_adc_pos = ao_adc_ring_next(ao_log_adc_pos);
			}
			/* Wait for more ADC data to arrive */
			ao_sleep((void *) &ao_adc_head);
		}
		memset(&log, '\0', sizeof (struct ao_log_telescience));
		log.type = AO_LOG_TELESCIENCE_STOP;
		log.tick = ao_time();
		ao_log_telescience_data(&log);
		ao_storage_flush();
	}
}

void
ao_log_start(void)
{
	ao_log_running = 1;
	ao_wakeup(&ao_log_running);
}

void
ao_log_stop(void)
{
	ao_log_running = 0;
	ao_wakeup((void *) &ao_adc_head);
}

void
ao_log_set(void)
{
	ao_cmd_hex();
	if (ao_cmd_status == ao_cmd_success) {
		if (ao_cmd_lex_i)
			ao_log_start();
		else
			ao_log_stop();
	}
}

const struct ao_cmds ao_log_cmds[] = {
	{ ao_log_set,	"L <0 off, 1 on>\0Set logging mode" },
	{ 0,	NULL },
};

void
ao_log_init(void)
{
	ao_log_running = 0;

	ao_cmd_register(&ao_log_cmds[0]);

	ao_add_task(&ao_log_task, ao_log_telescience, "log");
}
