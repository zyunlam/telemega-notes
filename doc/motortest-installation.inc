== Installation

	EasyMotor needs to be rigidly attached in the airframe, and the
	long axis of the circuit board needs to be aligned with the axis
	of flight. By default, the round beeper on the board should be
	“up” towards the nose cone, and the screw terminal strips should
	be “down” towards the fins and motor nozzle end of the rocket.

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
