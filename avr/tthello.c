#include <inttypes.h>
#include <avr/io.h>
#include <avr/interrupt.h>
#include <avr/sleep.h>

void wait_until_key_pressed(void)
{
    unsigned char temp1, temp2;
    unsigned int i;
    
    do {
        temp1 = PINC;                  // read input
        for(i=0;i<65535;i++);
        temp2 = PINC;                  // read input
        temp1 = (temp1 & temp2);       // debounce input
    } while ( temp1 & _BV(PINC4) );
    
    loop_until_bit_is_set(PINC,PINC4); /* wait until key is released */
}

int main (void)
{
	// configure to read center press on joy switch
	DDRC &=~ (1 << PC4);        /* Pin PC4 input              */
	PORTC |= (1 << PC4);        /* Pin PC4 pull-up enabled    */

	for (;;) {                           /* loop forever */
		// turn on LCD backlight LED
		PORTD = 0xff;
		wait_until_key_pressed();
		PORTD = 0;
		wait_until_key_pressed();
	}
        return 0;
}
