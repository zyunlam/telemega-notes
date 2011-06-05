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

#define LCD_PORT	PORTB
#define LCD_DDR		DDRB

#define PIN_RS		4
#define PIN_E		5
#define PIN_RW		6

void
ao_lcd_set_bits(uint8_t bits)
{
#if 0
	printf("\tLCD data %x RS %d R/W %d E %d\n",
	       bits & 0xf,
	       (bits & (1 << PIN_RS)) ? 1 : 0,
	       (bits & (1 << PIN_RW)) ? 1 : 0,
	       (bits & (1 << PIN_E)) ? 1 : 0);
#endif
	LCD_PORT = bits;
#if 0
	ao_delay(1);
	if (bits & (1 << PIN_RW))
		printf("\tLCD input %x\n", PINB);
#endif
}

uint8_t
ao_lcd_get_nibble(uint8_t rs)
{
	uint8_t	data = (rs ? (1 << PIN_RS) : 0) | (1 << PIN_RW);
	uint8_t n;

	DDRB = (1 << PIN_RS) | (1 << PIN_E) | (1 << PIN_RW);
	ao_lcd_set_bits(data);
	ao_lcd_set_bits(data | (1 << PIN_E));
	n = PINB & 0xf;
	ao_lcd_set_bits(data);
	return n;
}

uint8_t
ao_lcd_get_status(void)
{
	uint8_t	high, low;
	uint8_t data;

	high = ao_lcd_get_nibble(0);
	low = ao_lcd_get_nibble(0);
	data = (high << 4) | low;
	printf ("\tLCD status %02x\n", data);
	return data;
}

uint8_t
ao_lcd_get_data(void)
{
	uint8_t	high, low;
	uint8_t data;

	high = ao_lcd_get_nibble(1);
	low = ao_lcd_get_nibble(1);
	data = (high << 4) | low;
	printf ("\tLCD data %02x\n", data);
	return data;
}

void
ao_lcd_wait_idle(void)
{
	uint8_t	status;
	uint8_t	count = 0;

	do {
		status = ao_lcd_get_status();
		count++;
		if (count > 100) {
			printf("idle timeout\n");
			break;
		}
	} while (0);	/* status & 0x80); */
}

void
ao_lcd_send_nibble(uint8_t rs, uint8_t data)
{
	data = (data & 0xf) | (rs ? (1 << PIN_RS) : 0);
	DDRB = (0xf) | (1 << PIN_RS) | (1 << PIN_E) | (1 << PIN_RW);
	ao_lcd_set_bits(data);
	ao_lcd_set_bits(data | (1 << PIN_E));
	ao_lcd_set_bits(data);
}

void
ao_lcd_send_ins(uint8_t data)
{
	printf("send ins %02x\n", data);
	ao_lcd_wait_idle();
	ao_lcd_send_nibble(0, data >> 4);
	ao_lcd_send_nibble(0, data & 0xf);
}

void
ao_lcd_send_data(uint8_t data)
{
	printf ("send data %02x\n", data);
	ao_lcd_wait_idle();
	ao_lcd_send_nibble(1, data >> 4);
	ao_lcd_send_nibble(1, data & 0x0f);
}

void
ao_lcd_send_string(char *string)
{
	uint8_t	c;

	while ((c = (uint8_t) *string++))
		ao_lcd_send_data(c);
}

#define AO_LCD_POWER_CONTROL	0x54

void
ao_lcd_contrast_set(uint8_t contrast)
{
	ao_lcd_send_ins(AO_LCD_POWER_CONTROL | ((contrast >> 4) & 0x3));
	ao_lcd_send_ins(0x70 | (contrast & 0xf));
}

void
ao_lcd_clear(void)
{
	ao_lcd_send_ins(0x01);
	ao_delay(1);
	/* Entry mode */
	ao_lcd_send_ins(0x04 | 0x02);
}

void
ao_lcd_start(void)
{
	/* get to 4bit mode */
	ao_lcd_send_nibble(0, 0x3);
	ao_lcd_send_nibble(0, 0x3);
	ao_lcd_send_nibble(0, 0x3);
	ao_lcd_send_nibble(0, 0x2);

	/* function set */
	ao_lcd_send_ins(0x28);
	/* function set, instruction table 1 */
	ao_lcd_send_ins(0x29);

	/* freq set */
	ao_lcd_send_ins(0x14);

	/* Power/icon/contrast control*/
	ao_lcd_send_ins(AO_LCD_POWER_CONTROL);

	/* Follower control */
	ao_lcd_send_ins(0x6d);
	ao_delay(AO_MS_TO_TICKS(200));

	/* contrast set */
	ao_lcd_contrast_set(0x18);

	/* Display on */
	ao_lcd_send_ins(0x08 | 0x04);

	/* Clear */
	ao_lcd_clear();

}

void
ao_lcd_contrast(void)
{
	ao_cmd_hex();
	if (ao_cmd_status == ao_cmd_success) {
		printf("setting contrast to %02x\n", ao_cmd_lex_i);
		ao_lcd_contrast_set(ao_cmd_lex_i & 0x3f);
	}
}

void
ao_lcd_string(void)
{
	ao_lcd_clear();
	ao_cmd_white();
	while (ao_cmd_lex_c != '\n') {
		ao_lcd_send_data(ao_cmd_lex_c);
		ao_cmd_lex();
	}
}

__code struct ao_cmds ao_lcd_cmds[] = {
	{ ao_lcd_start, "S\0Start LCD" },
	{ ao_lcd_contrast, "C\0Set LCD contrast" },
	{ ao_lcd_string, "s\0Send string to LCD" },
	{ 0, NULL },
};

void
ao_lcd_init(void)
{
	DDRB = (1 << PIN_RS) | (1 << PIN_E) | (1 << PIN_RW);
	PORTB = 0;
	ao_cmd_register(&ao_lcd_cmds[0]);
}
