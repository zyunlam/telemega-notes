== Installation

	[WARNING]
	Firmware versions prior to 1.9.13 used acceleration to detect
	launch.  Starting with 1.9.13, the firmware switched to using
	pressure to trigger recording. Altus Metrum strongly recommends 
	upgrading all EasyMotor boards to current firmware, the
	behavior of which is described here.

	EasyMotor needs to be rigidly attached in the airframe, in any
	convenient orientation.

	=== Trigger to Start Recording

	Starting with firmware version 1.9.13, a pressure sensor must
	be attached, and pressure change is used to initiate and conclude
	data recording.  Pressure and 3 axes of acceleration are recorded
	for later analysis.

	EasyMotor looks for a 50 psi rise in pressure from ambient to
	start recording.  Recording stops when the pressure drops 
	below 50 psi and shows variation of less than 30 psi for at 
	least 10 seconds. These conditions are intended to capture all
	actual burn data including anomalies such as "chuffs".

	=== Power Switch and Battery

		In addition to the circuit board itself, EasyMotor needs
		a power switch and battery to operate.  Like many Altus 
		Metrum products, EasyMotor requires a single-cell LiPo 
		battery, and has an on-board charging circuit.  The 5V
		needed for inexpensive pressure sensors is generated 
		onboard using a switching regulator to boost the voltage.

		Because he often mounts EasyMotor to the motor's forward
		bulkhead instead of to the airframe itself, Bdale often
		uses a length of “shooter wire” from an e-match or used
		motor igniter as a power switch, routing the wire out of
		the typical fin can vent hole and using “twist and tape”
		to power up the board. Whatever works!

	=== Pressure Sensor

		The primary motivation for designing EasyMotor was to have
		a reliable way of recording motor chamber pressure during
		flight. To that end, EasyMotor supports attachment of a
		low-cost analog pressure sensor. The board provides 5V
		to power the sensor, and an input for measuring and
		logging the output voltage from the sensor.

		The kind of sensor EasyMotor is designed to work with
		takes 5V in and has a linear analog output that ranges
		from 0.5V at 0 to 4.5V at the maximum pressure supported
		by the sensor. Very inexpensive sensors that have a
		“1/8 NPT” threaded input, a “Buick-style” 3-pin connector,
		and typically ship with a short cable and mating
		connector, are readily available through various vendors
		including Amazon, eBay, and AliExpress.

		To log in-flight chamber pressure, a typical approach
		might be to drill a 1/8" sampling hole all the way
		through the center of the motor's forward closure, then
		drill and tap partially through the closure with a “1/8
		NPT” pipe tap. Fill the touch hole with grease, screw in
		the pressure sensor, and attach the sensor leads to
		EasyMotor.

		If you aren't up for machining closures yourself, 
		link:http://lokiresearch.com[Loki Research] sells 
		54mm and 75mm "experimental bulkheads" with threaded
		sensor ports that can be used with snap ring cases.
