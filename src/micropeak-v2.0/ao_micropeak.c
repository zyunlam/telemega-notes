/*
 * Copyright © 2017 Keith Packard <keithp@keithp.com>
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

#include <ao.h>
#include <ao_exti.h>
#include <ao_micropeak.h>
#include <ao_report_micro.h>
#include <ao_log_micro.h>
#include <ao_storage.h>

static struct ao_ms5607_value	value;

alt_t		ground_alt, max_alt;
alt_t		ao_max_height;

void
ao_pa_get(void)
{
	ao_ms5607_sample(&ao_ms5607_current);
	ao_ms5607_convert(&ao_ms5607_current, &value);
	pa = value.pres;
}

static void
ao_compute_height(void)
{
	ground_alt = ao_pa_to_altitude(pa_ground);
	max_alt = ao_pa_to_altitude(pa_min);
	ao_max_height = max_alt - ground_alt;
}

static void
ao_pips(void)
{
	uint8_t	i;
	for (i = 0; i < 10; i++) {
		ao_led_toggle(AO_LED_REPORT);
		ao_delay(AO_MS_TO_TICKS(80));
	}
	ao_delay(AO_MS_TO_TICKS(200));
}

void
ao_delay_until(uint16_t target) {
	int16_t	delay = target - ao_time();
	if (delay > 0) {
		ao_sleep_for(ao_delay_until, delay);
	}
}

static struct ao_task mp_task;

static void
ao_battery_disable(void)
{
	/* Disable */
	if (stm_adc.cr & (1 << STM_ADC_CR_ADEN)) {
		stm_adc.cr |= (1 << STM_ADC_CR_ADDIS);
		while (stm_adc.cr & (1 << STM_ADC_CR_ADDIS))
			;
	}

	/* Turn off everything */
	stm_adc.cr &= ~((1 << STM_ADC_CR_ADCAL) |
			(1 << STM_ADC_CR_ADSTP) |
			(1 << STM_ADC_CR_ADSTART) |
			(1 << STM_ADC_CR_ADEN));
}

static void
ao_battery_init(void)
{
	stm_rcc.apb2enr |= (1 << STM_RCC_APB2ENR_ADCEN);

	ao_battery_disable();

	/* Configure */
	stm_adc.cfgr1 = ((0 << STM_ADC_CFGR1_AWDCH) |				  /* analog watchdog channel 0 */
			 (0 << STM_ADC_CFGR1_AWDEN) |				  /* Disable analog watchdog */
			 (0 << STM_ADC_CFGR1_AWDSGL) |				  /* analog watchdog on all channels */
			 (0 << STM_ADC_CFGR1_DISCEN) |				  /* Not discontinuous mode. All channels converted with one trigger */
			 (0 << STM_ADC_CFGR1_AUTOOFF) |				  /* Leave ADC running */
			 (1 << STM_ADC_CFGR1_WAIT) |				  /* Wait for data to be read before next conversion */
			 (0 << STM_ADC_CFGR1_CONT) |				  /* only one set of conversions per trigger */
			 (1 << STM_ADC_CFGR1_OVRMOD) |				  /* overwrite on overrun */
			 (STM_ADC_CFGR1_EXTEN_DISABLE << STM_ADC_CFGR1_EXTEN) |	  /* SW trigger */
			 (0 << STM_ADC_CFGR1_ALIGN) |				  /* Align to LSB */
			 (STM_ADC_CFGR1_RES_12 << STM_ADC_CFGR1_RES) |		  /* 12 bit resolution */
			 (STM_ADC_CFGR1_SCANDIR_UP << STM_ADC_CFGR1_SCANDIR) |	  /* scan 0 .. n */
			 (STM_ADC_CFGR1_DMACFG_ONESHOT << STM_ADC_CFGR1_DMACFG) | /* one set of conversions then stop */
			 (1 << STM_ADC_CFGR1_DMAEN));				  /* enable DMA */

	/* Set the clock */
	stm_adc.cfgr2 = STM_ADC_CFGR2_CKMODE_PCLK_2 << STM_ADC_CFGR2_CKMODE;

	/* Shortest sample time */
	stm_adc.smpr = STM_ADC_SMPR_SMP_71_5 << STM_ADC_SMPR_SMP;

	/* Select Vref */
	stm_adc.chselr = 1 << 17;

	stm_adc.ccr = ((0 << STM_ADC_CCR_VBATEN) |
		       (0 << STM_ADC_CCR_TSEN) |
		       (1 << STM_ADC_CCR_VREFEN));

	/* Calibrate */
	stm_adc.cr |= (1 << STM_ADC_CR_ADCAL);
	while ((stm_adc.cr & (1 << STM_ADC_CR_ADCAL)) != 0)
		;

	/* Enable */
	stm_adc.cr |= (1 << STM_ADC_CR_ADEN);
	while ((stm_adc.isr & (1 << STM_ADC_ISR_ADRDY)) == 0)
		;

	/* Clear any stale status bits */
	stm_adc.isr = 0;

	/* Turn on syscfg */
	stm_rcc.apb2enr |= (1 << STM_RCC_APB2ENR_SYSCFGCOMPEN);
}

static void
ao_battery_fini(void)
{
	/* Disable */
	ao_battery_disable();

	/* Power down */
	stm_rcc.apb2enr &= ~(1 << STM_RCC_APB2ENR_ADCEN);
}

static uint16_t
ao_battery_voltage(void)
{
	uint16_t	vrefint;

	ao_battery_init();

	stm_adc.cr |= (1 << STM_ADC_CR_ADSTART);

	while ((stm_adc.isr & (1 << STM_ADC_ISR_EOC)) == 0)
		ao_arch_nop();

	vrefint = stm_adc.dr;

	ao_battery_fini();

	return 330 * stm_cal.vrefint_cal / vrefint;
}


uint8_t	ao_on_battery;

static void
ao_micropeak(void)
{
	ao_ms5607_setup();
	ao_storage_setup();

	/* Give the person a second to get their finger out of the way */
	ao_delay(AO_MS_TO_TICKS(1000));

	ao_pips();

	ao_log_micro_restore();
	ao_compute_height();
	ao_report_altitude();
	ao_log_micro_dump();

#if BOOST_DELAY
	ao_delay(BOOST_DELAY);
#endif

	ao_microflight();

	ao_log_micro_save();
	ao_compute_height();
	ao_report_altitude();

	ao_sleep(&ao_on_battery);
}

static void
ao_show_bat(void)
{
	printf("battery: %u\n", ao_battery_voltage());
}

static struct ao_cmds mp_cmd[] = {
	{ ao_show_bat, "b\0Show battery voltage" },
	{ 0 }
};

static void
ao_hsi_init(void)
{
	uint32_t	cfgr;

	/* Disable all interrupts */
	stm_rcc.cir = 0;

	/* Enable prefetch */
	stm_flash.acr |= (1 << STM_FLASH_ACR_PRFTBE);

	/* Enable power interface clock */
	stm_rcc.apb1enr |= (1 << STM_RCC_APB1ENR_PWREN);

	/* HCLK to 48MHz -> AHB prescaler = /1 */
	cfgr = stm_rcc.cfgr;
	cfgr &= ~(STM_RCC_CFGR_HPRE_MASK << STM_RCC_CFGR_HPRE);
	cfgr |= (AO_RCC_CFGR_HPRE_DIV << STM_RCC_CFGR_HPRE);
	stm_rcc.cfgr = cfgr;
	while ((stm_rcc.cfgr & (STM_RCC_CFGR_HPRE_MASK << STM_RCC_CFGR_HPRE)) !=
	       (AO_RCC_CFGR_HPRE_DIV << STM_RCC_CFGR_HPRE))
		ao_arch_nop();

	/* APB Prescaler = AO_APB_PRESCALER */
	cfgr = stm_rcc.cfgr;
	cfgr &= ~(STM_RCC_CFGR_PPRE_MASK << STM_RCC_CFGR_PPRE);
	cfgr |= (AO_RCC_CFGR_PPRE_DIV << STM_RCC_CFGR_PPRE);
	stm_rcc.cfgr = cfgr;

	/* Clear reset flags */
	stm_rcc.csr |= (1 << STM_RCC_CSR_RMVF);
}

void
main(void)
{
	if (ao_battery_voltage() < 320)
		ao_on_battery = 1;

	/* Leave the system running on the HSI if we're on battery */
	if (!ao_on_battery)
		ao_clock_init();
	else
		ao_hsi_init();

	ao_led_init();
	ao_task_init();
	ao_timer_init();
	ao_serial_init();
	stm_moder_set(&stm_gpioa, 2, STM_MODER_OUTPUT);

	ao_dma_init();
	ao_spi_init();
	ao_exti_init();

	/* Leave USB disabled on battery */
	if (!ao_on_battery) {
		ao_usb_init();
		ao_cmd_init();
	}

	ao_ms5607_init();

	ao_storage_init();

	ao_add_task(&mp_task, ao_micropeak, "micropeak");
	ao_cmd_register(mp_cmd);
	ao_start_scheduler();
}
