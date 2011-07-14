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
#include "ao_product.h"

static struct ao_task ao_log_task;
static struct ao_task ao_spi_task;

uint8_t 	ao_log_running;
uint8_t		ao_log_mutex;
uint32_t	ao_log_start_pos;
uint32_t	ao_log_end_pos;
uint32_t	ao_log_current_pos;

#define AO_LOG_TELESCIENCE_START	((uint8_t) 's')
#define AO_LOG_TELESCIENCE_DATA		((uint8_t) 'd')

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

static uint8_t
ao_log_valid(struct ao_log_telescience *log)
{
	uint8_t	*d;
	uint8_t	i;
	d = (uint8_t *) log;
	for (i = 0; i < sizeof (struct ao_log_telescience); i++)
		if (d[i] != 0xff)
			return 1;
	return 0;
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
ao_log_check_pin(void)
{
	if (PINB & (1 << PINB0))
		ao_log_stop();
	else
		ao_log_start();
}

void
ao_log_telescience(void)
{
	ao_storage_setup();

	/* Find end of data */
	while (ao_log_start_pos < ao_log_end_pos) {
		if (!(ao_storage_read(ao_log_start_pos, &log, sizeof (struct ao_log_telescience))))
			break;
		if (!ao_log_valid(&log))
			break;
	}

	/*
	 * Wait for the other side to settle down
	 */
	ao_delay(AO_SEC_TO_TICKS(5));

	ao_log_check_pin();

	ao_log_current_pos = ao_log_start_pos;
	ao_log_end_pos = ao_storage_config;
	for (;;) {
		while (!ao_log_running)
			ao_sleep(&ao_log_running);

		flush();
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
		flush();
	}
}

void
ao_log_set(void)
{
	printf("Logging currently %s\n", ao_log_running ? "on" : "off");
	ao_cmd_hex();
	if (ao_cmd_status == ao_cmd_success) {
		if (ao_cmd_lex_i) {
			printf("Logging from %ld to %ld\n", ao_log_current_pos, ao_log_end_pos);
			ao_log_start();
		} else {
			printf ("Log stopped at %ld\n", ao_log_current_pos);
			ao_log_stop();
		}
	}
	ao_cmd_status = ao_cmd_success;
}

void
ao_log_list(void)
{
	uint32_t	pos;
	uint32_t	start = 0;
	uint8_t		flight = 0;

	for (pos = 0; ; pos += sizeof (struct ao_log_telescience)) {
		if (!ao_storage_read(pos, &log, sizeof (struct ao_log_telescience)))
			break;
		if (!ao_log_valid(&log) || log.type == AO_LOG_TELESCIENCE_START) {
			if (pos != start) {
				printf("flight %d start %x end %x\n",
				       flight,
				       (uint16_t) (start >> 8),
				       (uint16_t) ((pos + 0xff) >> 8));
			}
			if (!ao_log_valid(&log))
				break;
			start = pos;
			flight++;
		}
	}
	printf ("done\n");
}

void
ao_log_delete(void)
{
	uint32_t	pos;

	ao_cmd_hex();
	if (ao_cmd_status != ao_cmd_success)
		return;
	if (ao_cmd_lex_i != 1) {
		ao_cmd_status = ao_cmd_syntax_error;
		printf("No such flight: %d\n", ao_cmd_lex_i);
		return;
	}
	ao_log_stop();
	for (pos = 0; pos < ao_storage_config; pos += ao_storage_block) {
		if (!ao_storage_read(pos, &log, sizeof (struct ao_log_telescience)))
			break;
		if (!ao_log_valid(&log))
			break;
		ao_storage_erase(pos);
	}
	ao_log_current_pos = ao_log_start_pos = 0;
	if (pos == 0)
		printf("No such flight: %d\n", ao_cmd_lex_i);
	else
		printf ("Erased\n");
}

void
ao_spi_telescience(void)
{
	static union {
		struct ao_companion_command	command;
		struct ao_companion_setup	setup;
		struct ao_adc			data;
	} u;

	for (;;) {
		ao_spi_slave_read((uint8_t *) &u.command, sizeof (struct ao_companion_command));
		switch (u.command.command) {
		case AO_COMPANION_SETUP:
			u.setup.board_id = AO_idProduct_NUMBER;
			u.setup.board_id_inverse = ~AO_idProduct_NUMBER;
			u.setup.update_period = 10;
			u.setup.channels = NUM_ADC;
			ao_spi_slave_write((uint8_t *) &u.setup,
					   sizeof (struct ao_companion_setup));
			break;
		case AO_COMPANION_FETCH:
			ao_adc_get(&u.data);
			ao_spi_slave_write((uint8_t *) &u.data.adc,
					   NUM_ADC * sizeof (uint16_t));
			if (u.command.flight_state >= ao_flight_boost &&
			    u.command.flight_state < ao_flight_landed) {
				if (!ao_log_running)
					ao_log_start();
			} else {
				if (ao_log_running)
					ao_log_stop();
			}
			break;
		}
	}
}

const struct ao_cmds ao_log_cmds[] = {
	{ ao_log_set,	"L <0 off, 1 on>\0Set logging mode" },
	{ ao_log_list,	"l\0List stored flight logs" },
	{ ao_log_delete, "d 1\0Delete all stored flights" },
	{ 0,	NULL },
};

void
ao_log_init(void)
{
	ao_log_running = 0;

	ao_cmd_register(&ao_log_cmds[0]);

	ao_add_task(&ao_log_task, ao_log_telescience, "log");
	ao_add_task(&ao_spi_task, ao_spi_telescience, "spi");
}
