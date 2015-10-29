== TeleMini v1.0

	image::telemini-v1-top.jpg[width="5.5in"]

	TeleMini v1.0 is ½ inches by 1½ inches.  It was
	designed to fit inside an 18mm air-frame tube, but using it in
	a tube that small in diameter may require some creativity in
	mounting and wiring to succeed!  Since there is no
	accelerometer, TeleMini can be mounted in any convenient
	orientation.  The default ¼ wave UHF wire antenna attached to
	the center of one end of the board is about 7 inches long. Two
	wires for the power switch are connected to holes in the
	middle of the board. Screw terminals for the e-matches for
	apogee and main ejection charges depart from the other end of
	the board, meaning an ideal “simple” avionics bay for TeleMini
	should have at least 9 inches of interior length.

      	=== TeleMini v1.0 Screw Terminals

		TeleMini v1.0 has four screw terminals on the end of the
		board opposite the telemetry antenna. Two are for the apogee
		and two are for main igniter circuits. There are also wires
		soldered to the board for the power switch.  Using the
		picture above and starting from the top for the terminals
		and from the left for the power switch wires, the
		connections are as follows:

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

      	=== Using a Separate Pyro Battery with TeleMini v1.0

		As described above, using an external pyro battery involves
		connecting the negative battery terminal to the flight
		computer ground, connecting the positive battery terminal to
		one of the igniter leads and connecting the other igniter
		lead to the per-channel pyro circuit connection. Because
		there is no solid ground connection to use on TeleMini, this
		is not recommended.

		The only available ground connection on TeleMini v1.0 are
		the two mounting holes next to the telemetry
		antenna. Somehow connect a small piece of wire to one of
		those holes and hook it to the negative pyro battery terminal.

		Connecting the positive battery terminal to the pyro
		charges must be done separate from TeleMini v1.0, by soldering
		them together or using some other connector.

		The other lead from each pyro charge is then inserted into
		the appropriate per-pyro channel screw terminal (terminal 3 for the
		Main charge, terminal 1 for the Apogee charge).

      	=== Using an Active Switch with TeleMini v1.0

		As explained above, an external active switch requires three
		connections, one to the positive battery terminal, one to
		the flight computer positive input and one to ground. Again,
		because TeleMini doesn't have any good ground connection,
		this is not recommended.

		The positive battery terminal is available on the Right
		power switch wire, the positive flight computer input is on
		the left power switch wire. Hook a lead to either of the
		mounting holes for a ground connection.
