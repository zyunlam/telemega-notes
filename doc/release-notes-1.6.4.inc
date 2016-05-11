= Release Notes for Version 1.6.4
:toc!:
:doctype: article

	Version 1.6.4 fixes a bluetooth communication problem with
	TeleBT v1.0 devices, along with some altosui and altosdroid
	minor nits. Ship firmware for some newer devices.

	== AltOS

	AltOS fixes:

	* Fix hardware flow control on TeleBT v1.0. Hardware RTS/CTS
	  doesn't seem to work, switch from using the
	  hardware to driving these pins with software.

	== AltosUI, TeleGPS and AltosDroid Applications

	AltosUI and TeleGPS New Features:

	* Automatically switch from meters or feet to kilometers or
          miles for distance units.

	AltosUI and TeleGPS Fixes:

	* Abort map preloading when the preload map dialog is closed.

	== Documentation

	* Mention TeleMega v2.0 in hardware specs table.

	* Document TeleGPS RF output in telegps manual.
