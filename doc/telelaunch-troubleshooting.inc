[appendix]
== Troubleshooting

	=== Radio Signal Strength

		Each unit in the TeleLaunch system has a debugging feature
		that can be used to view the actual received radio signal 
		strength of each packet.  To use this feature, connect to the
		desired unit (TeleLCO is probably the most useful place to
		start) with a laptop and terminal program as explained in the
		configuration section of the manual.  Then, you can enable
		debug tracing using 'D 1' command.

		Debug mode is fairly chatty, but each time the TeleLCO unit
		queries the currently selected TeleFire unit for igniter
		status, the return packet will print out the RSSI value.
		RSSI is "received signal strength indicated" and is expressed
		in decibel units relative to a milliwatt, or "dBm".  

		Observing the RSSI is a great way to compare antennas, antenna
		mounting arrangements, and so forth with more granularity than
		provided by the red/amber/green operational LED indicators.

		The system is good down to below -100dBm, and it takes about 
		6dB to double the range.  So a reading of -80dBm means that you
		could extend the distance between the units by a factor of
		10 before losing the link.  Note, however, that real world
		range is affected by terrain, vegetation, etc.  And you really
		don't ever want to be operating close to the minimum signal
		threshold!  Keep the RF signal indicators green with good
		antenna choices and installations for maximum satisfaction.
