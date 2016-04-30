= Release Notes for Version 1.6.3
:toc!:
:doctype: article

	Version 1.6.3 adds idle mode to AltosDroid and has bug fixes
	for our host software on desktops, laptops an android devices
	along with BlueTooth support for Windows.

	== AltOS

	There aren't any significant bug fixes or new features in the
	flight firmware in version 1.6.3.

	== AltosUI and TeleGPS Applications

	AltosUI and TeleGPS New Features:

	* Add BlueTooth support for Windows operating system. This
          supports connections to TeleBT over BlueTooth rather than
          just USB.

	AltosUI and TeleGPS Fixes:

	* Change Java detection and install on Windows. Detection is
          now done by looking for the 'javaw.exe' program, and
          installation by opening a browser on the java.com web site.

	* Delay polling while the Fire Igniters is visible to allow
          for TeleMega to report back complete status over the radio.

	* Disallow changing RF calibration numbers in the
          configuration UI. There's no good reason to change this from
          the field, and recovering is really hard if you haven't
          written down the right number.

	* Fix USB device discovery on Mac OS X El Capitan. This makes
          the connected Altus Metrum USB devices appear again.

	== AltosDroid

	AltosDroid new features:

	* Monitor Idle mode. Check state of flight computer while in
          idle mode over the radio link

	* Fire Igniters. Remotely fire ignires for recovery system
          ground tests.

	* Remote reboot. Cause the flight computer to reboot over the
          radio link. This provides a method for switching the flight
          computer from idle to flight mode without needing to reach
          the power switch.

	* Configurable frequency menu. Change the set of available
          frequencies and provide more descriptive names.

	AltosDroid bug fixes:

	* Don't set target location if GPS hasn't locked yet.

	* Fix saving target states so they can be reloaded when the
          application restarts. When the application is shut down and
          restarted, all previous target state information will be
          restored (including GPS position if available).

	* Fix crash on some Android devices for offline maps when
          changing the map scale or location.

	* Don't require USB OTG support. This kept the latest
	  AltosDroid from being offered on devices without USB device
	  support, although it can work without that just fine using
	  BlueTooth.

	* Don't require bluetooth to be enabled. This allows the
          application to operate with USB devices or just show old
          data without turning on the bluetooth radio.

	* Recover old tracker positions when restarting
          application. This finally allows you to safely stop and
          restart the application without losing the last known
          location of any tracker.

	== Documentation

	* Document TeleMega and EasyMega additional pyro channel
          continuity audio alert pattern.
