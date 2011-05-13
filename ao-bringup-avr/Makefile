MCU=atmega32u4
DUDECPUTYPE=m32u4
#PROGRAMMER=stk500v2 -P usb
PROGRAMMER=usbtiny
LOADCMD=avrdude
LOADARG=-p $(DUDECPUTYPE) -c $(PROGRAMMER) -e -U flash:w:
CC=avr-gcc
OBJCOPY=avr-objcopy
CFLAGS=-g -mmcu=$(MCU) -Wall -Wstrict-prototypes -Os -mcall-prologues

all: ao-blink.hex ao-switch.hex

ao-blink.hex: ao-blink
	avr-size ao-blink
	$(OBJCOPY) -R .eeprom -O ihex ao-blink $@

ao-blink: ao-blink.o
	$(CC) $(CFLAGS) -o $@ ao-blink.o

ao-switch.hex: ao-switch
	avr-size ao-switch
	$(OBJCOPY) -R .eeprom -O ihex ao-switch $@

ao-switch: ao-switch.o
	$(CC) $(CFLAGS) -o $@ ao-switch.o

ao-switch.s: ao-switch.c
	$(CC) $(CFLAGS) -S -o $@ ao-switch.c

load-blink: ao-blink.hex
	$(LOADCMD) $(LOADARG)ao-blink.hex

load-switch: ao-switch.hex
	$(LOADCMD) $(LOADARG)ao-switch.hex
