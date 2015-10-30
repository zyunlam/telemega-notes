[appendix]
== System Operation

	=== Firmware Modes

		The AltOS firmware build for the altimeters has two
		fundamental modes, “idle” and “flight”.  Which of these modes
		the firmware operates in is determined at start up time. For
		TeleMetrum, TeleMega and EasyMega, which have accelerometers, the mode is
		controlled by the orientation of the
		rocket (well, actually the board, of course...) at the time
		power is switched on.  If the rocket is “nose up”, then
		the flight computer assumes it's on a rail or rod being prepared for
		launch, so the firmware chooses flight mode.  However, if the
		rocket is more or less horizontal, the firmware instead enters
		idle mode.  Since TeleMini v2.0 and EasyMini don't have an
		accelerometer we can use to determine orientation, “idle” mode
		is selected if the board is connected via USB to a computer,
		otherwise the board enters “flight” mode. TeleMini v1.0
		selects “idle” mode if it receives a command packet within the
		first five seconds of operation.

		At power on, the altimeter will beep out the battery voltage
		to the nearest tenth of a volt.  Each digit is represented by
		a sequence of short “dit” beeps, with a pause between
		digits. A zero digit is represented with one long “dah”
		beep. Then there will be a short pause while the altimeter
		completes initialization and self test, and decides which mode
		to enter next.

		Here's a short summary of all of the modes and the beeping (or
		flashing, in the case of TeleMini v1) that accompanies each
		mode. In the description of the beeping pattern, “dit” means a
		short beep while "dah" means a long beep (three times as
		long). “Brap” means a long dissonant tone.

		.AltOS Modes
		[options="border",cols="1,1,2,2"]
		|====
		|Mode Name
		|Abbreviation
		|Beeps
		|Description

		|Startup
		|S
		|battery voltage in decivolts
		|Calibrating sensors, detecting orientation.

		|Idle
		|I
		|dit dit
		|Ready to accept commands over USB or radio link.

		|Pad
		|P
		|dit dah dah dit
		|Waiting for launch. Not listening for commands.

		|Boost
		|B
		|dah dit dit dit
		|Accelerating upwards.

		|Fast
		|F
		|dit dit dah dit
		|Decelerating, but moving faster than 200m/s.

		|Coast
		|C
		|dah dit dah dit
		|Decelerating, moving slower than 200m/s

		|Drogue
		|D
		|dah dit dit
		|Descending after apogee. Above main height.

		|Main
		|M
		|dah dah
		|Descending. Below main height.

		|Landed
		|L
		|dit dah dit dit
		|Stable altitude for at least ten seconds.


		|Sensor error
		|X
		|dah dit dit dah
		|Error detected during sensor calibration.
		|====

		In flight or “pad” mode, the altimeter engages the flight
		state machine, goes into transmit-only mode to send telemetry,
		and waits for launch to be detected.  Flight mode is indicated
		by an “di-dah-dah-dit” (“P” for pad) on the beeper or lights,
		followed by beeps or flashes indicating the state of the
		pyrotechnic igniter continuity.  One beep/flash indicates
		apogee continuity, two beeps/flashes indicate main continuity,
		three beeps/flashes indicate both apogee and main continuity,
		and one longer “brap” sound which is made by rapidly
		alternating between two tones indicates no continuity.  For a
		dual deploy flight, make sure you're getting three beeps or
		flashes before launching!  For apogee-only or motor eject
		flights, do what makes sense.

		If idle mode is entered, you will hear an audible “di-dit” or
		see two short flashes (“I” for idle), and the flight state
		machine is disengaged, thus no ejection charges will fire.
		The altimeters also listen for the radio link when in idle
		mode for requests sent via TeleDongle.  Commands can be issued
		in idle mode over either USB or the radio link
		equivalently. TeleMini v1.0 only has the radio link.  Idle
		mode is useful for configuring the altimeter, for extracting
		data from the on-board storage chip after flight, and for
		ground testing pyro charges.

		In “Idle” and “Pad” modes, once the mode indication
		beeps/flashes and continuity indication has been sent, if
		there is no space available to log the flight in on-board
		memory, the flight computer will emit a warbling tone (much
		slower than the “no continuity tone”)

		Here's a summary of all of the “pad” and “idle” mode indications.

		.Pad/Idle Indications
		[options="header",cols="1,1,3"]
		|====
		|Name		|Beeps		|Description

		|Neither
		|brap
		|No continuity detected on either apogee or main igniters.

		|Apogee
		|dit
		|Continuity detected only on apogee igniter.

		|Main
		|dit dit
		|Continuity detected only on main igniter.


		|Both
		|dit dit dit
		|Continuity detected on both igniters.


		|Storage Full
		|warble
		|On-board data logging storage is full. This will
		 not prevent the flight computer from safely
		 controlling the flight or transmitting telemetry
		 signals, but no record of the flight will be
		 stored in on-board flash.
		|====

		Once landed, the flight computer will signal that by emitting
		the “Landed” sound described above, after which it will beep
		out the apogee height (in meters). Each digit is represented
		by a sequence of short “dit” beeps, with a pause between
		digits. A zero digit is represented with one long “dah”
		beep. The flight computer will continue to report landed mode
		and beep out the maximum height until turned off.

		One “neat trick” of particular value when TeleMetrum, TeleMega
		or EasyMega are used with
		very large air-frames, is that you can power the board up while the
		rocket is horizontal, such that it comes up in idle mode.  Then you can
		raise the air-frame to launch position, and issue a 'reset' command
		via TeleDongle over the radio link to cause the altimeter to reboot and
		come up in flight mode.  This is much safer than standing on the top
		step of a rickety step-ladder or hanging off the side of a launch
		tower with a screw-driver trying to turn on your avionics before
		installing igniters!

		TeleMini v1.0 is configured solely via the radio link. Of course, that
		means you need to know the TeleMini radio configuration values
		or you won't be able to communicate with it. For situations
		when you don't have the radio configuration values, TeleMini v1.0
		offers an 'emergency recovery' mode. In this mode, TeleMini is
		configured as follows:


		 * Sets the radio frequency to 434.550MHz
		 * Sets the radio calibration back to the factory value.
		 * Sets the callsign to N0CALL
		 * Does not go to 'pad' mode after five seconds.

		To get into 'emergency recovery' mode, first find the row of
		four small holes opposite the switch wiring. Using a short
		piece of small gauge wire, connect the outer two holes
		together, then power TeleMini up. Once the red LED is lit,
		disconnect the wire and the board should signal that it's in
		'idle' mode after the initial five second startup period.

    	=== GPS

		TeleMetrum and TeleMega include a complete GPS receiver.  A
		complete explanation of how GPS works is beyond the scope of
		this manual, but the bottom line is that the GPS receiver
		needs to lock onto at least four satellites to obtain a solid
		3 dimensional position fix and know what time it is.

		The flight computers provide backup power to the GPS chip any time a
		battery is connected.  This allows the receiver to “warm start” on
		the launch rail much faster than if every power-on were a GPS
		“cold start”.  In typical operations, powering up
		on the flight line in idle mode while performing final air-frame
		preparation will be sufficient to allow the GPS receiver to cold
		start and acquire lock.  Then the board can be powered down during
		RSO review and installation on a launch rod or rail.  When the board
		is turned back on, the GPS system should lock very quickly, typically
		long before igniter installation and return to the flight line are
		complete.

    	=== Controlling An Altimeter Over The Radio Link

		One of the unique features of the Altus Metrum system is the
		ability to create a two way command link between TeleDongle
		and an altimeter using the digital radio transceivers
		built into each device. This allows you to interact with the
		altimeter from afar, as if it were directly connected to the
		computer.

		Any operation which can be performed with a flight computer can
		either be done with the device directly connected to the
		computer via the USB cable, or through the radio
		link. TeleMini v1.0 doesn't provide a USB connector and so it is
		always communicated with over radio.  Select the appropriate
		TeleDongle device when the list of devices is presented and
		AltosUI will interact with an altimeter over the radio link.

		One oddity in the current interface is how AltosUI selects the
		frequency for radio communications. Instead of providing
		an interface to specifically configure the frequency, it uses
		whatever frequency was most recently selected for the target
		TeleDongle device in Monitor Flight mode. If you haven't ever
		used that mode with the TeleDongle in question, select the
		Monitor Flight button from the top level UI, and pick the
		appropriate TeleDongle device.  Once the flight monitoring
		window is open, select the desired frequency and then close it
		down again. All radio communications will now use that frequency.

		 * Save Flight Data—Recover flight data from the
	           rocket without opening it up.

		 * Configure altimeter apogee delays, main deploy
		   heights and additional pyro event conditions to
		   respond to changing launch conditions. You can also
		   'reboot' the altimeter. Use this to remotely enable
		   the flight computer by turning TeleMetrum or
		   TeleMega on in “idle” mode, then once the air-frame
		   is oriented for launch, you can reboot the
		   altimeter and have it restart in pad mode without
		   having to climb the scary ladder.

		 * Fire Igniters—Test your deployment charges without snaking
		   wires out through holes in the air-frame. Simply assemble the
		   rocket as if for flight with the apogee and main charges
		   loaded, then remotely command the altimeter to fire the
		   igniters.

		Operation over the radio link for configuring an
		altimeter, ground testing igniters, and so forth uses
		the same RF frequencies as flight telemetry.  To
		configure the desired TeleDongle frequency, select the
		monitor flight tab, then use the frequency selector
		and close the window before performing other desired
		radio operations.

		The flight computers only enable radio commanding in
		'idle' mode.  TeleMetrum and TeleMega use the
		accelerometer to detect which orientation they start
		up in, so make sure you have the flight computer lying
		horizontally when you turn it on. Otherwise, it will
		start in 'pad' mode ready for flight, and will not be
		listening for command packets from TeleDongle.

		TeleMini listens for a command packet for five seconds
		after first being turned on, if it doesn't hear
		anything, it enters 'pad' mode, ready for flight and
		will no longer listen for command packets. The easiest
		way to connect to TeleMini is to initiate the command
		and select the TeleDongle device. At this point, the
		TeleDongle will be attempting to communicate with the
		TeleMini. Now turn TeleMini on, and it should
		immediately start communicating with the TeleDongle
		and the desired operation can be performed.

		You can monitor the operation of the radio link by watching the
		lights on the devices. The red LED will flash each time a packet
		is transmitted, while the green LED will light up on TeleDongle when
		it is waiting to receive a packet from the altimeter.

    	=== Ground Testing

		An important aspect of preparing a rocket using electronic deployment
		for flight is ground testing the recovery system.  Thanks
		to the bi-directional radio link central to the Altus Metrum system,
		this can be accomplished in a TeleMega, TeleMetrum or TeleMini equipped rocket
		with less work than you may be accustomed to with other systems.  It
		can even be fun!

		Just prep the rocket for flight, then power up the altimeter
		in “idle” mode (placing air-frame horizontal for TeleMetrum or TeleMega, or
		selecting the Configure Altimeter tab for TeleMini).  This will cause
		the firmware to go into “idle” mode, in which the normal flight
		state machine is disabled and charges will not fire without
		manual command.  You can now command the altimeter to fire the apogee
		or main charges from a safe distance using your computer and
		TeleDongle and the Fire Igniter tab to complete ejection testing.

    	=== Radio Link

		TeleMetrum, TeleMini and TeleMega all incorporate an RF transceiver, but
		it's not a full duplex system... each end can only be transmitting or
		receiving at any given moment.  So we had to decide how to manage the
		link.

		By design, the altimeter firmware listens for the radio link when
		it's in “idle mode”, which
		allows us to use the radio link to configure the rocket, do things like
		ejection tests, and extract data after a flight without having to
		crack open the air-frame.  However, when the board is in “flight
		mode”, the altimeter only
		transmits and doesn't listen at all.  That's because we want to put
		ultimate priority on event detection and getting telemetry out of
		the rocket through
		the radio in case the rocket crashes and we aren't able to extract
		data later...

		We don't generally use a 'normal packet radio' mode like APRS
		because they're just too inefficient.  The GFSK modulation we
		use is FSK with the base-band pulses passed through a Gaussian
		filter before they go into the modulator to limit the
		transmitted bandwidth.  When combined with forward error
		correction and interleaving, this allows us to have a very
		robust 19.2 kilobit data link with only 10-40 milliwatts of
		transmit power, a whip antenna in the rocket, and a hand-held
		Yagi on the ground.  We've had flights to above 21k feet AGL
		with great reception, and calculations suggest we should be
		good to well over 40k feet AGL with a 5-element yagi on the
		ground with our 10mW units and over 100k feet AGL with the
		40mW devices.  We hope to fly boards to higher altitudes over
		time, and would of course appreciate customer feedback on
		performance in higher altitude flights!

	=== APRS

		TeleMetrum v2.0 and TeleMega can send APRS if desired, and the
		interval between APRS packets can be configured. As each APRS
		packet takes a full second to transmit, we recommend an
		interval of at least 5 seconds to avoid consuming too much
		battery power or radio channel bandwidth. You can configure
		the APRS interval using AltosUI; that process is described in
		the Configure Altimeter section of the AltosUI chapter.

		AltOS uses the APRS compressed position report data format,
		which provides for higher position precision and shorter
		packets than the original APRS format. It also includes
		altitude data, which is invaluable when tracking rockets. We
		haven't found a receiver which doesn't handle compressed
		positions, but it's just possible that you have one, so if you
		have an older device that can receive the raw packets but
		isn't displaying position information, it's possible that this
		is the cause.

		APRS packets include an SSID (Secondary Station Identifier)
		field that allows one operator to have multiple
		transmitters. AltOS allows you to set this to a single digit
		from 0 to 9, allowing you to fly multiple transmitters at the
		same time while keeping the identify of each one separate in
		the receiver. By default, the SSID is set to the last digit of
		the device serial number.

		The APRS packet format includes a comment field that can have
		arbitrary text in it. AltOS uses this to send status
		information about the flight computer. It sends four fields as
		shown in the following table.

		.Altus Metrum APRS Comments
		[options="header",cols="1,1,1"]
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
		|A3.7
		|Apogee Igniter Voltage

		|5
		|M3.7
		|Main Igniter Voltage

		|6
		|1286
		|Device Serial Number
		|====

		Here's an example of an APRS comment showing GPS lock with 6
		satellites in view, a primary battery at 4.0V, and
		apogee and main igniters both at 3.7V from device 1286.

		....
		L6 B4.0 A3.7 M3.7 1286
		....

		Make sure your primary battery is above 3.8V, any
		connected igniters are above 3.5V and GPS is locked
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

		Configuring an Altus Metrum altimeter for flight is
		very simple.  Even on our baro-only TeleMini and
		EasyMini boards, the use of a Kalman filter means
		there is no need to set a “mach delay”.  The few
		configurable parameters can all be set using AltosUI
		over USB or or radio link via TeleDongle. Read the
		Configure Altimeter section in the AltosUI chapter
		below for more information.

		==== Radio Frequency

			Altus Metrum boards support radio frequencies
			in the 70cm band. By default, the
			configuration interface provides a list of 10
			“standard” frequencies in 100kHz channels
			starting at 434.550MHz.  However, the firmware
			supports use of any 50kHz multiple within the
			70cm band. At any given launch, we highly
			recommend coordinating when and by whom each
			frequency will be used to avoid interference.
			And of course, both altimeter and TeleDongle
			must be configured to the same frequency to
			successfully communicate with each other.

		==== Callsign

			This sets the callsign used for telemetry,
			APRS and the packet link. For telemetry and
			APRS, this is used to identify the device. For
			the packet link, the callsign must match that
			configured in AltosUI or the link will not
			work. This is to prevent accidental
			configuration of another Altus Metrum flight
			computer operating on the same frequency
			nearby.

		==== Telemetry/RDF/APRS Enable

			You can completely disable the radio while in
			flight, if necessary. This doesn't disable the
			packet link in idle mode.

		==== Telemetry baud rate

			This sets the modulation bit rate for data
			transmission for both telemetry and packet
			link mode. Lower bit rates will increase range
			while reducing the amount of data that can be
			sent and increasing battery consumption. All
			telemetry is done using a rate 1/2 constraint
			4 convolution code, so the actual data
			transmission rate is 1/2 of the modulation bit
			rate specified here.

		==== APRS Interval

			This selects how often APRS packets are
			transmitted. Set this to zero to disable APRS
			without also disabling the regular telemetry
			and RDF transmissions. As APRS takes a full
			second to transmit a single position report,
			we recommend sending packets no more than once
			every 5 seconds.

		==== APRS SSID

			This selects the SSID reported in APRS
			packets. By default, it is set to the last
			digit of the serial number, but you can change
			this to any value from 0 to 9.

		==== Apogee Delay

			Apogee delay is the number of seconds after
			the altimeter detects flight apogee that the
			drogue charge should be fired.  In most cases,
			this should be left at the default of 0.
			However, if you are flying redundant
			electronics such as for an L3 certification,
			you may wish to set one of your altimeters to
			a positive delay so that both primary and
			backup pyrotechnic charges do not fire
			simultaneously.

			The Altus Metrum apogee detection algorithm
			fires exactly at apogee.  If you are also
			flying an altimeter like the PerfectFlite
			MAWD, which only supports selecting 0 or 1
			seconds of apogee delay, you may wish to set
			the MAWD to 0 seconds delay and set the
			TeleMetrum to fire your backup 2 or 3 seconds
			later to avoid any chance of both charges
			firing simultaneously.  We've flown several
			air-frames this way quite happily, including
			Keith's successful L3 cert.

		==== Apogee Lockout

			Apogee lockout is the number of seconds after
			boost where the flight computer will not fire
			the apogee charge, even if the rocket appears
			to be at apogee. This is often called 'Mach
			Delay', as it is intended to prevent a flight
			computer from unintentionally firing apogee
			charges due to the pressure spike that occurrs
			across a mach transition. Altus Metrum flight
			computers include a Kalman filter which is not
			fooled by this sharp pressure increase, and so
			this setting should be left at the default
			value of zero to disable it.

		==== Main Deployment Altitude

			By default, the altimeter will fire the main
			deployment charge at an elevation of 250
			meters (about 820 feet) above ground.  We
			think this is a good elevation for most
			air-frames, but feel free to change this to
			suit.  In particular, if you are flying two
			altimeters, you may wish to set the deployment
			elevation for the backup altimeter to be
			something lower than the primary so that both
			pyrotechnic charges don't fire simultaneously.

		==== Maximum Flight Log

			Changing this value will set the maximum
			amount of flight log storage that an
			individual flight will use. The available
			storage is divided into as many flights of the
			specified size as can fit in the available
			space. You can download and erase individual
			flight logs. If you fill up the available
			storage, future flights will not get logged
			until you erase some of the stored ones.

			Even though our flight computers (except TeleMini v1.0) can store
			multiple flights, we strongly recommend downloading and saving
			flight data after each flight.

		==== Ignite Mode

			Instead of firing one charge at apogee and
			another charge at a fixed height above the
			ground, you can configure the altimeter to
			fire both at apogee or both during
			descent. This was added to support an airframe
			Bdale designed that had two altimeters, one in
			the fin can and one in the nose.

			Providing the ability to use both igniters for
			apogee or main allows some level of redundancy
			without needing two flight computers.  In
			Redundant Apogee or Redundant Main mode, the
			two charges will be fired two seconds apart.

		==== Pad Orientation

			TeleMetrum, TeleMega and EasyMega measure
			acceleration along the axis of the
			board. Which way the board is oriented affects
			the sign of the acceleration value. Instead of
			trying to guess which way the board is mounted
			in the air frame, the altimeter must be
			explicitly configured for either Antenna Up or
			Antenna Down. The default, Antenna Up, expects
			the end of the board connected to the 70cm
			antenna to be nearest the nose of the rocket,
			with the end containing the screw terminals
			nearest the tail.

		==== Configurable Pyro Channels

			In addition to the usual Apogee and Main pyro
			channels, TeleMega and EasyMega have four
			additional channels that can be configured to
			activate when various flight conditions are
			satisfied. You can select as many conditions
			as necessary; all of them must be met in order
			to activate the channel. The conditions
			available are:

			include::pyro-channels.raw[]

