== TeleMini

	.TeleMini v3 Board
	image::telemini-v3.jpg[width="5.5in"]

	TeleMini v3 is 0.5 inches by 1.67 inches.  It was
	designed to fit inside an 18mm air-frame tube, but using it in
	a tube that small in diameter may require some creativity in
	mounting and wiring to succeed!  Since there is no
	accelerometer, TeleMini can be mounted in any convenient
	orientation.  The default ¼ wave UHF wire antenna attached to
	the center of one end of the board is about 7 inches long. Screw
	terminals for the power switch are located in the
	middle of the board. Screw terminals for the e-matches for
	apogee and main ejection charges depart from the other end of
	the board, meaning an ideal “simple” avionics bay for TeleMini
	should have at least 9 inches of interior length.

      	=== TeleMini v3 Screw Terminals

		TeleMini v3 has four screw terminals on the end of the
		board opposite the telemetry antenna. Two are for the apogee
		and two are for main igniter circuits. Another two
		screw terminals are located in the middle of the board
		for the power switch.  Using the
		picture above and starting from the top for the pyro terminals
		and from the left for the power switch terminals, the
		connections are as follows:

		.TeleMini v3 Screw Terminals
		[options="header",grid="all",cols="2,3,10"]
		|====
		|Terminal #|Terminal Name|Description

		|1
		|Apogee -
		|Apogee pyro channel connection to pyro circuit

		|2
		|Apogee +
		|Apogee pyro channel common connection to battery +

		|3
		|Main -
		|Main pyro channel connection to pyro circuit

		|4
		|Main +
		|Main pyro channel common connection to battery +

		|Left
		|Switch Output
		|Switch connection to flight computer

		|Right
		|Switch Input
		|Switch connection to positive battery terminal
		|====

      	=== Using a Separate Pyro Battery with TeleMini v3

		As described above, using an external pyro battery involves
		connecting the negative battery terminal to the flight
		computer ground, connecting the positive battery terminal to
		one of the igniter leads and connecting the other igniter
		lead to the per-channel pyro circuit connection. Because
		there is no solid ground connection to use on TeleMini, this
		is not recommended.

		The only available ground connection on TeleMini v3 are
		the two mounting holes next to the telemetry
		antenna. Somehow connect a small piece of wire to one of
		those holes and hook it to the negative pyro battery terminal.

		Connecting the positive battery terminal to the pyro
		charges must be done separate from TeleMini v3, by soldering
		them together or using some other connector.

		The other lead from each pyro charge is then inserted into
		the appropriate per-pyro channel screw terminal (terminal 3 for the
		Main charge, terminal 1 for the Apogee charge).

      	=== Using an Active Switch with TeleMini v3

		As explained above, an external active switch requires three
		connections, one to the positive battery terminal, one to
		the flight computer positive input and one to ground. Again,
		because TeleMini doesn't have any good ground connection,
		this is not recommended.

		The positive battery terminal is available on the Right
		power switch wire, the positive flight computer input is on
		the left power switch wire. Hook a lead to either of the
		mounting holes for a ground connection.

	=== TeleMini v1

		TeleMini v1 is the earlier version of this product. It
		has a lower-power radio, less storage, no beeper and
		soldered-in wires instead of screw terminals for the
		power switch.

		.TeleMini v1 Board
		image::telemini-v1-top.jpg[width="5.5in"]

