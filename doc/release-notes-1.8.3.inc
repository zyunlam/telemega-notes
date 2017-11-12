= Release Notes for Version 1.8.3
:toc!:
:doctype: article

	Version 1.8.3 includes two important flight computer fixes,
	changed KML export data for Tripoli Record reporting and some
	updates to graph presentation and data downloading.

	== AltOS

	AltOS Bug Fixes

	* Ground testing EasyMega and TeleMega additional pyro
          channels could result in a sticky 'fired' status which would
          prevent these channels from firing on future flights.

	* Corrupted flight log records could prevent future flights
          from capturing log data.

	== AltosUI and TeleGPS Applications

	AltosUI and TeleGPS Changes

	* KML export now reports both barometric and GPS altitude data
          to make it more useful for Tripoli record reporting.

	* Graph lines have improved appearance to make them easier to
	  distinguish.

	* The download dialog now offers to graph new flights, checks
          for existing files to avoid overwriting data and reports if
          there are checksum errors in the downloaded data.
