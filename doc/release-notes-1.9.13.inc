= Release Notes for Version 1.9.13
include::release-head.adoc[]
:doctype: article

	Version 1.9.13

	== AltOS

	* Add option to beep max height in feet after landing

	* Fix APRS reports to be sent at the correct time and spacing.

	* Fix possible barometric sensor communication failure when
	  the CPU is busy talking to the radio at the same time. This
	  would cause loss of telemetry and failure to track the state
	  of the rocket during flight. This was aggrevated by the APRS
	  reports getting sent more often than they should.

	* Change EasyMotor v3 code to base logging on motor pressure
          rather than the accelerometer. This allows use of EasyMotor
          v3 in a static test stand.

	== AltosUI

	* Add support for configuring the units used to report height
          after landing on the beeper.
