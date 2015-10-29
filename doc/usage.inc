== Using Altus Metrum Hardware

	Here are general instructions for hooking up an Altus Metrum
	flight computer. Instructions specific to each model will be
	found in the section devoted to that model below.

	=== Wiring and Electrical Interference

		To prevent electrical interference from affecting the
		operation of the flight computer, it's important to always
		twist pairs of wires connected to the board. Twist the switch
		leads, the pyro leads and the battery leads. This reduces
		interference through a mechanism called common mode rejection.

	=== Hooking Up Lithium Polymer Batteries

		All Altus Metrum flight computers have a two pin JST PH
		series connector to connect up a single-cell Lithium Polymer
		cell (3.7V nominal). You can purchase matching batteries
		from the Altus Metrum store, or other vendors, or you can
		make your own. Pin 1 of the connector is positive, pin 2 is
		negative. Spark Fun sells a cable with the connector
		attached, which they call a
		link:https://www.sparkfun.com/products/9914[JST Jumper 2 Wire Assembly]

		Many RC vendors also sell lithium polymer batteries with
		this same connector. All that we have found use the opposite
		polarity, and if you use them that way, you will damage or
		destroy the flight computer.

	=== Hooking Up Pyro Charges

		Altus Metrum flight computers always have two screws for
		each pyro charge. This means you shouldn't need to put two
		wires into a screw terminal or connect leads from pyro
		charges together externally.

		On the flight computer, one lead from each charge is hooked
		to the positive battery terminal through the power switch.
		The other lead is connected through the pyro circuit, which
		is connected to the negative battery terminal when the pyro
		circuit is fired.

	=== Hooking Up a Power Switch

		Altus Metrum flight computers need an external power switch
		to turn them on. This disconnects both the computer and the
		pyro charges from the battery, preventing the charges from
		firing when in the Off position. The switch is in-line with
		the positive battery terminal.

	=== Using an External Active Switch Circuit

		You can use an active switch circuit, such as the
		Featherweight Magnetic Switch, with any Altus Metrum
		flight computer. These require three connections, one to
		the battery, one to the positive power input on the flight
		computer and one to ground. Find instructions on how to
		hook these up for each flight computer below. The follow
		the instructions that come with your active switch to
		connect it up.

	=== Using a Separate Pyro Battery

		As mentioned above in the section on hooking up pyro
		charges, one lead for each of the pyro charges is connected
		through the power switch directly to the positive battery
		terminal. The other lead is connected to the pyro circuit,
		which connects it to the negative battery terminal when the
		pyro circuit is fired. The pyro circuit on all of the flight
		computers is designed to handle up to 16V.

		To use a separate pyro battery, connect the negative pyro
		battery terminal to the flight computer ground terminal,
		the positive battery terminal to the igniter and the other
		igniter lead to the negative pyro terminal on the flight
		computer. When the pyro channel fires, it will complete the
		circuit between the negative pyro terminal and the ground
		terminal, firing the igniter. Specific instructions on how
		to hook this up will be found in each section below.

	=== Using a Different Kind of Battery

		EasyMini and TeleMini v2 are designed to use either a
		lithium polymer battery or any other battery producing
		between 4 and 12 volts, such as a rectangular 9V
		battery. TeleMega, EasyMega and TeleMetrum are not designed for this,
		and must only be powered by a lithium polymer battery. Find
		instructions on how to use other batteries in the EasyMini
		and TeleMini sections below.
