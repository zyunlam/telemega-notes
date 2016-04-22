= Release Notes for Version 1.6.3
:toc!:
:doctype: article

	Version 1.6.3 contains bug fixes for our host software on
	desktops, laptops an android devices along with BlueTooth
	support for Windows.

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

	* Don't set target location if GPS hasn't locked yet.

	* Fix saving target states so they can be reloaded when the
          application restarts. When the application is shut down and
          restarted, all previous target state information will be
          restored (including GPS position if available).

	* Fix crash on some Android devices for offline maps when
          changing the map scale or location.

	== Documentation

	* Document TeleMega and EasyMega additional pyro channel
          continuity audio alert pattern.
