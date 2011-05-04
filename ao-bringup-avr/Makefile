MCU=atmega32u4
DUDECPUTYPE=m32u4
#PROGRAMMER=stk500v2 -P usb
PROGRAMMER=usbtiny
LOADCMD=avrdude
LOADARG=-p $(DUDECPUTYPE) -c $(PROGRAMMER) -e -U flash:w:
CC=avr-gcc
OBJCOPY=avr-objcopy
CFLAGS=-g -mmcu=$(MCU) -Wall -Wstrict-prototypes -Os -mcall-prologues

all: ao-blink.hex

ao-blink.hex: ao-blink
	avr-size ao-blink
	$(OBJCOPY) -R .eeprom -O ihex ao-blink $@

ao-blink: ao-blink.o
	$(CC) $(CFLAGS) -o $@ ao-blink.o

load: ao-blink.hex
	$(LOADCMD) $(LOADARG)ao-blink.hex