[appendix]
== TeleGPS System Operation

	=== GFSK Telemetry

		TeleGPS's native telemetry system doesn't use a
		'normal packet radio' mode like APRS because it's not
		very efficient.  The GFSK modulation we use is FSK
		with the base-band pulses passed through a Gaussian
		filter before they go into the modulator to limit the
		transmitted bandwidth.  When combined with forward
		error correction and interleaving, this allows us to
		have a very robust 19.2 kilobit data link with only
		10-40 milliwatts of transmit power, a whip antenna in
		the rocket, and a hand-held Yagi on the ground.  We've
		had flights to above 21k feet AGL with great
		reception, and calculations suggest we should be good
		to well over 40k feet AGL with a 5-element yagi on the
		ground with our 10mW units and over 100k feet AGL with
		the 40mW devices.
      
	=== APRS

		TeleGPS can send APRS if desired, and the interval
		between APRS packets can be configured. As each APRS
		packet takes a full second to transmit, we recommend
		an interval of at least 5 seconds to avoid consuming
		too much battery power or radio channel bandwidth. You
		can configure the APRS interval; that
		process is described in the Configure TeleGPS
		section of the TeleGPS Application chapter.

		AltOS uses the APRS compressed position report data
		format, which provides for higher position precision
		and shorter packets than the original APRS format. It
		also includes altitude data, which is invaluable when
		tracking rockets. We haven't found a receiver which
		doesn't handle compressed positions, but it's just
		possible that you have one, so if you have an older
		device that can receive the raw packets but isn't
		displaying position information, it's possible that
		this is the cause.

		The APRS packet format includes a comment field that
		can have arbitrary text in it. AltOS uses this to send
		status information about the flight computer. It sends
		four fields as shown in the following table.
      
		.TeleGPS APRS Comments
		[options="header",cols="1,1,3"]
		|====
		|Field	      |Example	      |Description

		|1
		|L
		|GPS Status U for unlocked, L for locked

		|2
		|6
		|Number of Satellites in View

		|3
		|B4.0
		|Altimeter Battery Voltage

		|4
		|1286
		|Device Serial Number
		|====

		Here's an example of an APRS comment showing GPS lock with 6
		satellites in view and a battery at 4.0V from device 1286.

		....
		L6 B4.0 1286
		....

		Make sure your battery is above 3.8V GPS is locked
		with at least 5 or 6 satellites in view before
		flying. If GPS is switching between L and U regularly,
		then it doesn't have a good lock and you should wait
		until it becomes stable.

		If the GPS receiver loses lock, the APRS data
		transmitted will contain the last position for which
		GPS lock was available. You can tell that this has
		happened by noticing that the GPS status character
		switches from 'L' to 'U'. Before GPS has locked, APRS
		will transmit zero for latitude, longitude and
		altitude.

	=== Configurable Parameters

        Configuring TeleGPS is very
        simple; the few configurable parameters can all be set
        using the TeleGPS application over USB. Read
	the Configure TeleGPS section in the TeleGPS Software chapter below
	for more information.
      
		==== Radio Frequency

			Altus Metrum boards support radio frequencies in the 70cm
			band. By default, the configuration interface provides a
			list of 10 “standard” frequencies in 100kHz channels starting at
			434.550MHz.  However, the firmware supports use of
			any 50kHz multiple within the 70cm band. At any given
			launch, we highly recommend coordinating when and by whom each
			frequency will be used to avoid interference.  And of course, both
			TeleGPS and the receiver must be configured to the same
			frequency to successfully communicate with each other.

		==== Callsign

			This sets the callsign used for telemetry and APRS to
			identify the device.

		==== Telemetry/RDF/APRS Enable

			You can completely disable the radio, if necessary, leaving
			TeleGPS only logging data to internal memory.

		==== APRS Interval

			This selects how often APRS packets are transmitted. Set
			this to zero to disable APRS without also disabling the
			regular telemetry and RDF transmissions. As APRS takes a
			full second to transmit a single position report, we
			recommend sending packets no more than once every 5 seconds.

		==== Maximum Flight Log

			Changing this value will set the maximum amount of flight
			log storage that an individual flight will use. The
			available storage is divided into as many flights of the
			specified size as can fit in the available space. You can
			download and erase individual flight logs. If you fill up
			the available storage, future flights will not get logged
			until you erase some of the stored ones.

		==== Logging Trigger Motion

			If TeleGPS moves less than this distance over a long period
			of time, it will not log that location, saving storage space.

		==== Position Reporting Interval

			This sets how often TeleGPS reports position information via
			telemetry and to the on-board log. Reducing this value will
			save power and logging memory consumption.
