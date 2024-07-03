/*
 * Copyright © 2024 Bdale Garbee <bdale@gag.com>
 * GPLv3
 */

#include <ao.h>
#include <ao_serial.h>

static struct ao_task ao_console_read_task;

static void
ao_console_read(void)
{
        int     c;
        for (;;) {
                ao_arch_block_interrupts();
                c = _ao_serial0_pollchar();
                ao_arch_release_interrupts();
                if (c == AO_READ_AGAIN) {
                        flush();
                        c = ao_serial0_getchar();
                }
                ao_usb_putchar((char) c);
        }
}

int
main(void)
{
	ao_clock_init();
	ao_task_init();
	ao_timer_init();

	ao_adc_init();

	ao_usb_init();

	ao_serial_init();
        ao_serial0_set_speed(AO_SERIAL_SPEED_115200);

	ao_cmd_init();

	ao_add_task(&ao_console_read_task, ao_console_read, "console_read");

	ao_start_scheduler();
	return 0;
}
