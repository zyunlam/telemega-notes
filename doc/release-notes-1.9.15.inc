= Release Notes for Version 1.9.15
include::release-head.adoc[]
:doctype: article

	Version 1.9.15

	== AltOS

	* Add TeleMega v6.0 support

	* Fix sign of IMU values for TeleMega v5 boards in the
          'across' axis. This affects IMU acceleration and gyro reports
	  for that axis, but has no effect on in-flight operation of
	  the tilt computation.

