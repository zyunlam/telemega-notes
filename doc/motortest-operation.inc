== Operation

	Operating an EasyMotor board is pretty easy. Turn the power on
	before launch, typically during the usual pre-flight electronics
	checklist after the rocket is installed on a launch rail.

	The board will beep out a Morse code “P” every few seconds
	indicating that it's in pad mode and ready to detect the start of
	the motor burn.  Recording starts when chamber pressure rises by
	at least 50psi, then the board logs pressure and acceleration
	data 100 times per second until chamber pressure goes low again and
	remains stable for at least 10 seconds.

	After flight, attach a USB data cable to the board before powering
	it on so that it goes in to 'idle mode'.  Then AltosUI can be used 
	to download the flight data, view a quick graph of acceleration and 
	pressure with pan and
	zoom capabilities, then export it to a comma separated values 
	(CSV) file. Such a file can easily be loaded into a spreadsheet 
	for analysis. 
