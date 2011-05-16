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

struct ao_task __xdata ao_usb_task;

static __xdata uint16_t	ao_usb_in_bytes;
static __xdata uint16_t ao_usb_in_bytes_last;
static __xdata uint16_t	ao_usb_out_bytes;
static __xdata uint8_t	ao_usb_iif;
static __xdata uint8_t	ao_usb_running;

static void
ao_usb_set_interrupts(void)
{
	/* IN interrupts on the control an IN endpoints */
	USBIIE = (1 << AO_USB_CONTROL_EP) | (1 << AO_USB_IN_EP);

	/* OUT interrupts on the OUT endpoint */
	USBOIE = (1 << AO_USB_OUT_EP);

	/* Only care about reset */
	USBCIE = USBCIE_RSTIE;
}

void
ao_usb_set_address(uint8_t address)
{
	UDADD = (0 << ADDEN) | address;
	ao_usb_running = 1;
}

#define EP_SIZE(s)	((s) == 64 ? 0x30 :	\
			((s) == 32 ? 0x20 :	\
			((s) == 16 ? 0x10 :	\
			             0x00)))

static void
ao_usb_set_ep0(void)
{
	/* Set the CONTROL max packet size, single buffered */
	UENUM = 0;
	UECONX = (1 << EPEN);					/* Enable */

	UECFG0X = ((0 << EPTYPE0) |				/* Control */
		   (0 << EPDIR));				/* Out (ish) */

	UECFG1X = ((EP_SIZE(AO_USB_CONTROL_SIZE) << EPSIZE0) |	/* Size */
		   (0 << EPBK));				/* Single bank */

	UEIENX = ((1 << RXSTPE) |	/* Enable SETUP interrupt */
		  (1 << RXOUTE) |	/* Enable OUT interrupt */
		  (1 << TXINE));	/* Enable IN complete interrupt */
}

static void
ao_usb_set_configuration(void)
{
	/* Set the IN max packet size, double buffered */
	UENUM = AO_USB_IN_EP;
	UECONX = (1 << EPEN);					/* Enable */

	UECFG0X = ((2 << EPTYPE0) |				/* Bulk */
		   (1 << EPDIR));				/* In */

	UECFG1X = ((EP_SIZE(AO_USB_IN_SIZE) << EPSIZE0) |	/* Size */
		   (1 << EPBK) |				/* Double bank */
		   (1 << ALLOC));				/* Allocate */

	/* Set the OUT max packet size, double buffered */
	UENUM = AO_USB_OUT_EP;
	UECONX |= (1 << EPEN);					/* Enable */

	UECFG0X = ((2 << EPTYPE0) |				/* Bulk */
		   (0 << EPDIR));				/* Out */

	UECFG1X = ((EP_SIZE(AO_USB_OUT_SIZE) << EPSIZE0) |	/* Size */
		   (1 << EPBK) |				/* Double bank */
		   (1 << ALLOC));				/* Allocate */
}

ISR(USB_GEN_vect)
{
	uint8_t intbits;

        intbits = UDINT;
        UDINT = 0;
        if (intbits & (1<<EORSTI)) {
		ao_usb_set_ep0();
		usb_configuration = 0;
        }
	if (intbits & (1<<SOFI)) {
		if (usb_configuration) {
			t = transmit_flush_timer;
			if (t) {
				transmit_flush_timer = --t;
				if (!t) {
					UENUM = CDC_TX_ENDPOINT;
					UEINTX = 0x3A;
				}
			}
		}
	}
}

struct ao_usb_setup {
	uint8_t		dir_type_recip;
	uint8_t		request;
	uint16_t	value;
	uint16_t	index;
	uint16_t	length;
} __xdata ao_usb_setup;

__xdata uint8_t ao_usb_ep0_state;
uint8_t * __xdata ao_usb_ep0_in_data;
__xdata uint8_t ao_usb_ep0_in_len;
__xdata uint8_t	ao_usb_ep0_in_buf[2];
__xdata uint8_t ao_usb_ep0_out_len;
__xdata uint8_t *__xdata ao_usb_ep0_out_data;
__xdata uint8_t ao_usb_configuration;

/* Send an IN data packet */
static void
ao_usb_ep0_flush(void)
{
	__xdata uint8_t this_len;
	__xdata uint8_t	cs0;

	/* If the IN packet hasn't been picked up, just return */
	UENUM = 0;
	if (!(UEINTX & (1 << TXINI)))
		return;

	this_len = ao_usb_ep0_in_len;
	if (this_len > AO_USB_CONTROL_SIZE)
		this_len = AO_USB_CONTROL_SIZE;

	ao_usb_ep0_in_len -= this_len;
	while (this_len--)
		UEDATX = *ao_usb_ep0_in_data++;

	/* Clear the TXINI bit to send the packet */
	UEINTX &= ~(1 << TXINI);
}

__xdata static struct ao_usb_line_coding ao_usb_line_coding = {115200, 0, 0, 8};

/* Walk through the list of descriptors and find a match
 */
static void
ao_usb_get_descriptor(uint16_t value)
{
	const uint8_t		*__xdata descriptor;
	__xdata uint8_t		type = value >> 8;
	__xdata uint8_t		index = value;

	descriptor = ao_usb_descriptors;
	while (descriptor[0] != 0) {
		if (descriptor[1] == type && index-- == 0) {
			if (type == AO_USB_DESC_CONFIGURATION)
				ao_usb_ep0_in_len = descriptor[2];
			else
				ao_usb_ep0_in_len = descriptor[0];
			ao_usb_ep0_in_data = descriptor;
			break;
		}
		descriptor += descriptor[0];
	}
}

/* Read data from the ep0 OUT fifo
 */
static void
ao_usb_ep0_fill(void)
{
	__xdata uint8_t	len;

	UENUM = 0;
	len = UEBCLX;	/* read length */
	if (len > ao_usb_ep0_out_len)
		len = ao_usb_ep0_out_len;
	ao_usb_ep0_out_len -= len;

	/* Pull all of the data out of the packet */
	while (len--)
		*ao_usb_ep0_out_data++ = UEDATX;
}

void
ao_usb_ep0_queue_byte(uint8_t a)
{
	ao_usb_ep0_in_buf[ao_usb_ep0_in_len++] = a;
}

static void
ao_usb_ep0_setup(void)
{
	/* Pull the setup packet out of the fifo */
	ao_usb_ep0_out_data = (__xdata uint8_t *) &ao_usb_setup;
	ao_usb_ep0_out_len = 8;
	ao_usb_ep0_fill();
	UENUM = 0;
	UEINTX &= ~(1 << RXSTPI);
	if (ao_usb_ep0_out_len != 0)
		return;

	/* Figure out how to ACK the setup packet */
	if (ao_usb_setup.dir_type_recip & AO_USB_DIR_IN) {
		if (ao_usb_setup.length)
			ao_usb_ep0_state = AO_USB_EP0_DATA_IN;
		else
			ao_usb_ep0_state = AO_USB_EP0_IDLE;
	} else {
		if (ao_usb_setup.length)
			ao_usb_ep0_state = AO_USB_EP0_DATA_OUT;
		else
			ao_usb_ep0_state = AO_USB_EP0_IDLE;
	}
	UENUM = 0;
/*
	if (ao_usb_ep0_state == AO_USB_EP0_IDLE)
		USBCS0 = USBCS0_CLR_OUTPKT_RDY | USBCS0_DATA_END;
	else
		USBCS0 = USBCS0_CLR_OUTPKT_RDY;
*/

	ao_usb_ep0_in_data = ao_usb_ep0_in_buf;
	ao_usb_ep0_in_len = 0;
	switch(ao_usb_setup.dir_type_recip & AO_USB_SETUP_TYPE_MASK) {
	case AO_USB_TYPE_STANDARD:
		switch(ao_usb_setup.dir_type_recip & AO_USB_SETUP_RECIP_MASK) {
		case AO_USB_RECIP_DEVICE:
			switch(ao_usb_setup.request) {
			case AO_USB_REQ_GET_STATUS:
				ao_usb_ep0_queue_byte(0);
				ao_usb_ep0_queue_byte(0);
				break;
			case AO_USB_REQ_SET_ADDRESS:
				ao_usb_set_address(ao_usb_setup.value);
				break;
			case AO_USB_REQ_GET_DESCRIPTOR:
				ao_usb_get_descriptor(ao_usb_setup.value);
				break;
			case AO_USB_REQ_GET_CONFIGURATION:
				ao_usb_ep0_queue_byte(ao_usb_configuration);
				break;
			case AO_USB_REQ_SET_CONFIGURATION:
				ao_usb_configuration = ao_usb_setup.value;
				ao_usb_set_configuration();
				break;
			}
			break;
		case AO_USB_RECIP_INTERFACE:
			#pragma disable_warning 110
			switch(ao_usb_setup.request) {
			case AO_USB_REQ_GET_STATUS:
				ao_usb_ep0_queue_byte(0);
				ao_usb_ep0_queue_byte(0);
				break;
			case AO_USB_REQ_GET_INTERFACE:
				ao_usb_ep0_queue_byte(0);
				break;
			case AO_USB_REQ_SET_INTERFACE:
				break;
			}
			break;
		case AO_USB_RECIP_ENDPOINT:
			switch(ao_usb_setup.request) {
			case AO_USB_REQ_GET_STATUS:
				ao_usb_ep0_queue_byte(0);
				ao_usb_ep0_queue_byte(0);
				break;
			}
			break;
		}
		break;
	case AO_USB_TYPE_CLASS:
		switch (ao_usb_setup.request) {
		case SET_LINE_CODING:
			ao_usb_ep0_out_len = 7;
			ao_usb_ep0_out_data = (__xdata uint8_t *) &ao_usb_line_coding;
			break;
		case GET_LINE_CODING:
			ao_usb_ep0_in_len = 7;
			ao_usb_ep0_in_data = (uint8_t *) &ao_usb_line_coding;
			break;
		case SET_CONTROL_LINE_STATE:
			break;
		}
		break;
	}
	if (ao_usb_ep0_state != AO_USB_EP0_DATA_OUT) {
		if (ao_usb_setup.length < ao_usb_ep0_in_len)
			ao_usb_ep0_in_len = ao_usb_setup.length;
		ao_usb_ep0_flush();
	}
}

/* End point 0 receives all of the control messages. */
static void
ao_usb_ep0(void)
{
	__xdata uint8_t	intx;

	ao_usb_ep0_state = AO_USB_EP0_IDLE;
	for (;;) {
		cli();
		for (;;) {
			UENUM = 0;
			intx = UEINTX;
			if (intx & ((1 << RXOUTI) | (1 <<RXSTPI)))
			    break;
			ao_sleep(&ao_usb_task);
		}
		sei();
		if (intx & (1 << RXSTPI)) {
			ao_usb_ep0_setup();
			UENUM = 0;
			UEINTX &= ~(1 << RXSTPI);
		}
		if (intx & (1 << RXOUTI)) {
			ao_usb_ep0_fill();
			UENUM = 0;
			UEINTX &= ~(1 << RXOUTI);
		}
	}
}

/* Wait for a free IN buffer */
static void
ao_usb_in_wait(void)
{
	for (;;) {
		UENUM = AO_USB_IN_EP;
		if ((UEINTX & (1 << RWAL)))
			break;
		ao_sleep(&ao_usb_in_bytes);
	}
}

void
ao_usb_flush(void) __critical
{
	if (!ao_usb_running)
		return;

	/* If there are pending bytes, or if the last packet was full,
	 * send another IN packet
	 */
	if (ao_usb_in_bytes || (ao_usb_in_bytes_last == AO_USB_IN_SIZE)) {
		ao_usb_in_wait();
		ao_usb_in_send();
	}
}

void
ao_usb_putchar(char c) __critical __reentrant
{
	if (!ao_usb_running)
		return;

	ao_usb_in_wait();

	/* Queue a byte, sending the packet when full */
	UENUM = AO_USB_IN_EP;
	UEDATX = c;
	if (++ao_usb_in_bytes == AO_USB_IN_SIZE)
		ao_usb_in_send();
}

char
ao_usb_pollchar(void) __critical
{
	char c;
	if (ao_usb_out_bytes == 0) {
		/* Check to see if a packet has arrived */
		UENUM = AO_USB_OUT_EP;
		if ((UEINTX & (1 << RXOUTI)) == 0)
			return AO_READ_AGAIN;
		ao_usb_out_bytes = (UEBCHX << 8) | UEBCLX;
		if (ao_usb_out_bytes == 0) {
			UEINTX &= ~(1 << RXOUTI);
			return AO_READ_AGAIN;
		}
	}
	--ao_usb_out_bytes;
	UENUM = AO_USB_OUT_EP;
	c = UEDATX;
	if (ao_usb_out_bytes == 0) {
		UENUM = AO_USB_OUT_EP;
		UEINTX &= ~(1 << RXOUTI);
	}
	return c;
}

char
ao_usb_getchar(void) __critical
{
	char	c;

	while ((c = ao_usb_pollchar()) == AO_READ_AGAIN)
		ao_sleep(&ao_stdin_ready);
	return c;
}

// Misc functions to wait for ready and send/receive packets
static inline void usb_wait_in_ready(void)
{
	while (!(UEINTX & (1<<TXINI))) ;
}
static inline void usb_send_in(void)
{
	UEINTX = ~(1<<TXINI);
}
static inline void usb_wait_receive_out(void)
{
	while (!(UEINTX & (1<<RXOUTI))) ;
}
static inline void usb_ack_out(void)
{
	UEINTX = ~(1<<RXOUTI);
}

/* Endpoint 0 interrupt */

ISR(USB_COM_vect)
{
	ao_wakeup(&ao_usb_task);
}

#if AVR_VCC_5V
#define AO_PAD_REGULATOR_INIT	(1 << UVREGE)	/* Turn on pad regulator */
#endif
#if AVR_VCC_3V3
#define AO_PAD_REGULATOR_INIT	0		/* Turn off pad regulator */
#endif

#if AVR_CLOCK == 16000000UL
#define AO_USB_PLL_INPUT_PRESCALER	(1 << PINDIV)	/* Divide 16MHz clock by 2 */
#endif
#if AVR_CLOCK == 8000000UL
#define AO_USB_PLL_INPUT_PRESCALER	0		/* Don't divide clock */
#endif

void
ao_usb_disable(void)
{
	/* Unplug from the bus */
	UDCON = (1 << DETACH);
	ao_usb_detatch_interface();

	/* Disable the interface */
	USBCON = 0;

	/* Disable the PLL */
	PLLCSR = 0;

	/* Turn off the pad regulator */
	UHWCON = 0;
}

#define AO_USB_CON ((1 << USBE) |	/* USB enable */ \
		    (0 << RSTCPU) |	/* do not reset CPU */	\
		    (0 << LSM) |	/* Full speed mode */	\
		    (0 << RMWKUP))	/* no remote wake-up */ \

void
ao_usb_enable(void)
{
	/* Configure pad regulator */
	UHWCON = AO_PAD_REGULATOR_INIT;

	/* Enable USB device, but freeze the clocks until initialized */
	USBCON = AO_USB_CON | (1 <<FRZCLK);

	/* Enable PLL with appropriate divider */
	PLLCSR = AO_USB_PLL_INPUT_PRESCALER | (1 << PLLE);

	/* Wait for PLL to lock */
	loop_until_bit_is_set(PLLCSR, (1 << PLOCK));

	/* Enable USB, enable the VBUS pad */
	USBCON = AO_USB_CON | (1 << OTGPADE);

	UDCON = (0 << DETACH);	/* Clear the DETACH bit to plug into the bus */

	usb_configuration = 0;
}

void
ao_usb_init(void)
{
	ao_usb_enable();

	ao_add_task(&ao_usb_task, ao_usb_ep0, "usb");
	ao_add_stdio(ao_usb_pollchar, ao_usb_putchar, ao_usb_flush);
}
