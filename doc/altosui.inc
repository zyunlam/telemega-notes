== AltosUI

	.AltosUI Main Window
	image::altosui.png[width="4.6in"]

	The AltosUI program provides a graphical user interface for
	interacting with the Altus Metrum product family. AltosUI can
	monitor telemetry data, configure devices and many other
	tasks. The primary interface window provides a selection of
	buttons, one for each major activity in the system.  This
	chapter is split into sections, each of which documents one of
	the tasks provided from the top-level toolbar.

    	=== Monitor Flight
	////
	      <subtitle>Receive, Record and Display Telemetry Data</subtitle>
	////

		Selecting this item brings up a dialog box listing all
		of the connected TeleDongle devices. When you choose
		one of these, AltosUI will create a window to display
		telemetry data as received by the selected TeleDongle
		device.

		.Device Selection Dialog
		image::device-selection.png[width="3.1in"]

		All telemetry data received are automatically recorded
		in suitable log files. The name of the files includes
		the current date and rocket serial and flight numbers.

		The radio frequency being monitored by the TeleDongle
		device is displayed at the top of the window. You can
		configure the frequency by clicking on the frequency
		box and selecting the desired frequency. AltosUI
		remembers the last frequency selected for each
		TeleDongle and selects that automatically the next
		time you use that device.

		Below the TeleDongle frequency selector, the window
		contains a few significant pieces of information about
		the altimeter providing the telemetry data stream:

		 * The configured call-sign

		 * The device serial number

		 * The flight number. Each altimeter remembers how
		   many times it has flown.

		 * The rocket flight state. Each flight passes through
		   several states including Pad, Boost, Fast, Coast,
		   Drogue, Main and Landed.

		 * The Received Signal Strength Indicator value. This
		   lets you know how strong a signal TeleDongle is
		   receiving. At the default data rate, 38400 bps, in
		   bench testing, the radio inside TeleDongle v0.2
		   operates down to about -106dBm, while the v3 radio
		   works down to about -111dBm.  Weaker signals, or an
		   environment with radio noise may cause the data to
		   not be received. The packet link uses error
		   detection and correction techniques which prevent
		   incorrect data from being reported.

		 * The age of the displayed data, in seconds since the
		   last successfully received telemetry packet.  In
		   normal operation this will stay in the low single
		   digits.  If the number starts counting up, then you
		   are no longer receiving data over the radio link
		   from the flight computer.

		Finally, the largest portion of the window contains a
		set of tabs, each of which contain some information
		about the rocket.  They're arranged in 'flight order'
		so that as the flight progresses, the selected tab
		automatically switches to display data relevant to the
		current state of the flight. You can select other tabs
		at any time. The final 'table' tab displays all of the
		raw telemetry values in one place in a
		spreadsheet-like format.

		==== Launch Pad

			.Monitor Flight Launch Pad View
			image::launch-pad.png[width="5.5in"]

			The 'Launch Pad' tab shows information used to decide when the
			rocket is ready for flight. The first elements include red/green
			indicators, if any of these is red, you'll want to evaluate
			whether the rocket is ready to launch:

			Battery Voltage::
			This indicates whether the Li-Po battery powering the
			flight computer has sufficient charge to last for
			the duration of the flight. A value of more than
			3.8V is required for a 'GO' status.

			Apogee Igniter Voltage::
			This indicates whether the apogee
			igniter has continuity. If the igniter has a low
			resistance, then the voltage measured here will be close
			to the Li-Po battery voltage. A value greater than 3.2V is
			required for a 'GO' status.

			Main Igniter Voltage::
			This indicates whether the main
			igniter has continuity. If the igniter has a low
			resistance, then the voltage measured here will be close
			to the Li-Po battery voltage. A value greater than 3.2V is
			required for a 'GO' status.

			On-board Data Logging::
			This indicates whether there is space remaining
			on-board to store flight data for the upcoming
			flight. If you've downloaded data, but failed to erase
			flights, there may not be any space left. Most of our
			flight computers can store multiple flights, depending
			on the configured maximum flight log size. TeleMini
			v1.0 stores only a single flight, so it will need to
			be downloaded and erased after each flight to capture
			data. This only affects on-board flight logging; the
			altimeter will still transmit telemetry and fire
			ejection charges at the proper times even if the
			flight data storage is full.

			GPS Locked::
			For a TeleMetrum or TeleMega device, this indicates
			whether the GPS receiver is currently able to compute
			position information. GPS requires at least 4
			satellites to compute an accurate position.

			GPS Ready::

			For a TeleMetrum or TeleMega device, this indicates
			whether GPS has reported at least 10 consecutive
			positions without losing lock. This ensures that the
			GPS receiver has reliable reception from the
			satellites.

			The Launchpad tab also shows the computed launch pad
			position and altitude, averaging many reported
			positions to improve the accuracy of the fix.

		==== Ascent

			.Monitor Flight Ascent View
			image::ascent.png[width="5.5in"]

			This tab is shown during Boost, Fast and Coast
			phases. The information displayed here helps monitor the
			rocket as it heads towards apogee.

			The height, speed, acceleration and tilt are shown along
			with the maximum values for each of them. This allows you to
			quickly answer the most commonly asked questions you'll hear
			during flight.

			The current latitude and longitude reported by the GPS are
			also shown. Note that under high acceleration, these values
			may not get updated as the GPS receiver loses position
			fix. Once the rocket starts coasting, the receiver should
			start reporting position again.

			Finally, the current igniter voltages are reported as in the
			Launch Pad tab. This can help diagnose deployment failures
			caused by wiring which comes loose under high acceleration.

		==== Descent

			.Monitor Flight Descent View
			image::descent.png[width="5.5in"]

			Once the rocket has reached apogee and (we hope)
			activated the apogee charge, attention switches to
			tracking the rocket on the way back to the ground, and
			for dual-deploy flights, waiting for the main charge
			to fire.

			To monitor whether the apogee charge operated
			correctly, the current descent rate is reported along
			with the current height. Good descent rates vary based
			on the choice of recovery components, but generally
			range from 15-30m/s on drogue and should be below
			10m/s when under the main parachute in a dual-deploy
			flight.

			With GPS-equipped flight computers, you can locate the
			rocket in the sky using the elevation and bearing
			information to figure out where to look. Elevation is
			in degrees above the horizon. Bearing is reported in
			degrees relative to true north. Range can help figure
			out how big the rocket will appear. Ground Distance
			shows how far it is to a point directly under the
			rocket and can help figure out where the rocket is
			likely to land. Note that all of these values are
			relative to the pad location. If the elevation is near
			90°, the rocket is over the pad, not over you.

			Finally, the igniter voltages are reported in this tab
			as well, both to monitor the main charge as well as to
			see what the status of the apogee charge is.  Note
			that some commercial e-matches are designed to retain
			continuity even after being fired, and will continue
			to show as green or return from red to green after
			firing.

		==== Landed

			.Monitor Flight Landed View
			image::landed.png[width="5.5in"]

			Once the rocket is on the ground, attention switches
			to recovery. While the radio signal is often lost once
			the rocket is on the ground, the last reported GPS
			position is generally within a short distance of the
			actual landing location.

			The last reported GPS position is reported both by
			latitude and longitude as well as a bearing and
			distance from the launch pad. The distance should give
			you a good idea of whether to walk or hitch a ride.
			Take the reported latitude and longitude and enter
			them into your hand-held GPS unit and have that
			compute a track to the landing location.

			Our flight computers will continue to transmit RDF
			tones after landing, allowing you to locate the rocket
			by following the radio signal if necessary. You may
			need to get away from the clutter of the flight line,
			or even get up on a hill (or your neighbor's RV roof)
			to receive the RDF signal.

			The maximum height, speed and acceleration reported
			during the flight are displayed for your admiring
			observers.  The accuracy of these immediate values
			depends on the quality of your radio link and how many
			packets were received.  Recovering the on-board data
			after flight may yield more precise results.

			To get more detailed information about the flight, you
			can click on the 'Graph Flight' button which will
			bring up a graph window for the current flight.

		==== Table

			.Monitor Flight Table View
			image::table.png[width="5.5in"]

			The table view shows all of the data available from the
			flight computer. Probably the most useful data on
			this tab is the detailed GPS information, which includes
			horizontal dilution of precision information, and
			information about the signal being received from the satellites.

		==== Site Map

			.Monitor Flight Site Map View
			image::site-map.png[width="5.5in"]

			When the TeleMetrum has a GPS fix, the Site Map tab
			will map the rocket's position to make it easier for
			you to locate the rocket, both while it is in the air,
			and when it has landed. The rocket's state is
			indicated by color: white for pad, red for boost, pink
			for fast, yellow for coast, light blue for drogue,
			dark blue for main, and black for landed.

			The map's default scale is approximately 3m (10ft) per
			pixel. The map can be dragged using the left mouse
			button. The map will attempt to keep the rocket
			roughly centered while data is being received.

			You can adjust the style of map and the zoom level
			with buttons on the right side of the map window. You
			can draw a line on the map by moving the mouse over
			the map with a button other than the left one pressed,
			or by pressing the left button while also holding down
			the shift key. The length of the line in real-world
			units will be shown at the start of the line.

			Images are fetched automatically via the Google Maps
			Static API, and cached on disk for reuse. If map
			images cannot be downloaded, the rocket's path will be
			traced on a dark gray background instead.

			You can pre-load images for your favorite launch sites
			before you leave home; check out the 'Preload Maps'
			section below.

		==== Igniter

			.Monitor Flight Additional Igniter View
			image::ignitor.png[width="5.5in"]

			TeleMega includes four additional programmable pyro
			channels. The Ignitor tab shows whether each of them has
			continuity. If an ignitor has a low resistance, then the
			voltage measured here will be close to the pyro battery
			voltage. A value greater than 3.2V is required for a 'GO'
			status.

	=== Save Flight Data

		The altimeter records flight data to its internal
		flash memory.  TeleMetrum data is recorded at a much
		higher rate than the telemetry system can handle, and
		is not subject to radio drop-outs. As such, it
		provides a more complete and precise record of the
		flight. The 'Save Flight Data' button allows you to
		read the flash memory and write it to disk.

		Clicking on the 'Save Flight Data' button brings up a
		list of connected flight computers and TeleDongle
		devices. If you select a flight computer, the flight
		data will be downloaded from that device directly. If
		you select a TeleDongle device, flight data will be
		downloaded from a flight computer over radio link via
		the specified TeleDongle. See the chapter on
		Controlling An Altimeter Over The Radio Link for more
		information.

		After the device has been selected, a dialog showing
		the flight data saved in the device will be shown
		allowing you to select which flights to download and
		which to delete. With version 0.9 or newer firmware,
		you must erase flights in order for the space they
		consume to be reused by another flight. This prevents
		accidentally losing flight data if you neglect to
		download data before flying again. Note that if there
		is no more space available in the device, then no data
		will be recorded during the next flight.

		The file name for each flight log is computed
		automatically from the recorded flight date, altimeter
		serial number and flight number information.

	=== Replay Flight

		Select this button and you are prompted to select a flight
		record file, either a .telem file recording telemetry data or a
		.eeprom file containing flight data saved from the altimeter
		flash memory.

		Once a flight record is selected, the flight monitor interface
		is displayed and the flight is re-enacted in real time. Check
		the Monitor Flight chapter above to learn how this window operates.

	=== Graph Data

		Select this button and you are prompted to select a flight
		record file, either a .telem file recording telemetry data or a
		.eeprom file containing flight data saved from
		flash memory.

		Note that telemetry files will generally produce poor graphs
		due to the lower sampling rate and missed telemetry packets.
		Use saved flight data in .eeprom files for graphing where possible.

		Once a flight record is selected, a window with multiple tabs is
		opened.

		==== Flight Graph

			.Flight Data Graph
			image::graph.png[width="5.5in"]

			By default, the graph contains acceleration (blue),
			velocity (green) and altitude (red).

			The graph can be zoomed into a particular area by
			clicking and dragging down and to the right. Once
			zoomed, the graph can be reset by clicking and
			dragging up and to the left. Holding down control and
			clicking and dragging allows the graph to be panned.
			The right mouse button causes a pop-up menu to be
			displayed, giving you the option save or print the
			plot.

		==== Configure Graph

			.Flight Graph Configuration
			image::graph-configure.png[width="5.5in"]

			This selects which graph elements to show, and, at the
			very bottom, lets you switch between metric and
			imperial units

		==== Flight Statistics

			.Flight Statistics
			image::graph-stats.png[width="5.5in"]

			Shows overall data computed from the flight.

		==== Map

			.Flight Map
			image::graph-map.png[width="5.5in"]

			Shows a satellite image of the flight area overlaid
			with the path of the flight. The red concentric
			circles mark the launch pad, the black concentric
			circles mark the landing location.

	=== Export Data

		This tool takes the raw data files and makes them
		available for external analysis. When you select this
		button, you are prompted to select a flight data file,
		which can be either a .eeprom or .telem.  The .eeprom
		files contain higher resolution and more continuous
		data, while .telem files contain receiver signal
		strength information.  Next, a second dialog appears
		which is used to select where to write the resulting
		file. It has a selector to choose between CSV and KML
		file formats.

		==== Comma Separated Value Format

			This is a text file containing the data in a form
			suitable for import into a spreadsheet or other
			external data analysis tool. The first few lines of
			the file contain the version and configuration
			information from the altimeter, then there is a single
			header line which labels all of the fields. All of
			these lines start with a '#' character which many
			tools can be configured to skip over.

			The remaining lines of the file contain the data, with
			each field separated by a comma and at least one
			space. All of the sensor values are converted to
			standard units, with the barometric data reported in
			both pressure, altitude and height above pad units.

		==== Keyhole Markup Language (for Google Earth)

			This is the format used by Google Earth to provide an
			overlay within that application. With this, you can
			use Google Earth to see the whole flight path in 3D.

	=== Configure Altimeter

		.Altimeter Configuration
		image::configure-altimeter.png[width="3.6in"]

		Select this button and then select either an altimeter or
		TeleDongle Device from the list provided. Selecting a TeleDongle
		device will use the radio link to configure a remote altimeter.

		The first few lines of the dialog provide information about the
		connected device, including the product name,
		software version and hardware serial number. Below that are the
		individual configuration entries.

		At the bottom of the dialog, there are four buttons:

		Save::
		This writes any changes to the configuration parameter
		block in flash memory. If you don't press this button,
		any changes you make will be lost.

		Reset::
		This resets the dialog to the most recently saved
		values, erasing any changes you have made.

		Reboot::

		This reboots the device. Use this to switch from idle
		to pad mode by rebooting once the rocket is oriented
		for flight, or to confirm changes you think you saved
		are really saved.

		Close::

		This closes the dialog. Any unsaved changes will be
		lost.

	        The rest of the dialog contains the parameters to be configured.

		==== Main Deploy Altitude

			This sets the altitude (above the recorded pad
			altitude) at which the 'main' igniter will fire. The
			drop-down menu shows some common values, but you can
			edit the text directly and choose whatever you
			like. If the apogee charge fires below this altitude,
			then the main charge will fire two seconds after the
			apogee charge fires.

		==== Apogee Delay

			When flying redundant electronics, it's often
			important to ensure that multiple apogee charges don't
			fire at precisely the same time, as that can over
			pressurize the apogee deployment bay and cause a
			structural failure of the air-frame. The Apogee Delay
			parameter tells the flight computer to fire the apogee
			charge a certain number of seconds after apogee has
			been detected.

		==== Apogee Lockout

			Apogee lockout is the number of seconds after boost
			where the flight computer will not fire the apogee
			charge, even if the rocket appears to be at
			apogee. This is often called 'Mach Delay', as it is
			intended to prevent a flight computer from
			unintentionally firing apogee charges due to the
			pressure spike that occurrs across a mach
			transition. Altus Metrum flight computers include a
			Kalman filter which is not fooled by this sharp
			pressure increase, and so this setting should be left
			at the default value of zero to disable it.

		==== Frequency

			This configures which of the frequencies to use for
			both telemetry and packet command mode. Note that if
			you set this value via packet command mode, the
			TeleDongle frequency will also be automatically
			reconfigured to match so that communication will
			continue afterwards.

		==== RF Calibration

			The radios in every Altus Metrum device are calibrated
			at the factory to ensure that they transmit and
			receive on the specified frequency.  If you need to
			you can adjust the calibration by changing this value.
			Do not do this without understanding what the value
			means, read the appendix on calibration and/or the
			source code for more information.  To change a
			TeleDongle's calibration, you must reprogram the unit
			completely.

      		==== Telemetry/RDF/APRS Enable

			Enables the radio for transmission during
			flight. When disabled, the radio will not
			transmit anything during flight at all.

      		==== Telemetry baud rate

			This sets the modulation bit rate for data
			transmission for both telemetry and packet
			link mode. Lower bit rates will increase range
			while reducing the amount of data that can be
			sent and increasing battery consumption. All
			telemetry is done using a rate 1/2 constraint
			4 convolution code, so the actual data
			transmission rate is 1/2 of the modulation bit
			rate specified here.

      		==== APRS Interval

			How often to transmit GPS information via APRS
			(in seconds). When set to zero, APRS
			transmission is disabled. This option is
			available on TeleMetrum v2 and TeleMega
			boards. TeleMetrum v1 boards cannot transmit
			APRS packets. Note that a single APRS packet
			takes nearly a full second to transmit, so
			enabling this option will prevent sending any
			other telemetry during that time.

      		==== APRS SSID

			Which SSID to report in APRS packets. By
			default, this is set to the last digit of the
			serial number, but can be configured to any
			value from 0 to 9.

		==== Callsign

			This sets the call sign included in each
			telemetry packet. Set this as needed to
			conform to your local radio regulations.

      		==== Maximum Flight Log Size

			This sets the space (in kilobytes) allocated
			for each flight log. The available space will
			be divided into chunks of this size. A smaller
			value will allow more flights to be stored, a
			larger value will record data from longer
			flights.

      		==== Ignitor Firing Mode

			This configuration parameter allows the two standard ignitor
			channels (Apogee and Main) to be used in different
			configurations.

			Dual Deploy::
			This is the usual mode of operation; the
			'apogee' channel is fired at apogee and the
			'main' channel at the height above ground
			specified by the 'Main Deploy Altitude' during
			descent.

			Redundant Apogee::
			This fires both channels at apogee, the
			'apogee' channel first followed after a two
			second delay by the 'main' channel.

			Redundant Main::
			This fires both channels at the height above
			ground specified by the Main Deploy Altitude
			setting during descent. The 'apogee' channel
			is fired first, followed after a two second
			delay by the 'main' channel.

      		==== Pad Orientation

			Because they include accelerometers,
			TeleMetrum, TeleMega and EasyMega are
			sensitive to the orientation of the board. By
			default, they expect the antenna end to point
			forward. This parameter allows that default to
			be changed, permitting the board to be mounted
			with the antenna pointing aft instead.

			Antenna Up::
			In this mode, the antenna end of the flight
			computer must point forward, in line with the
			expected flight path.

			Antenna Down::
			In this mode, the antenna end of the flight
			computer must point aft, in line with the
			expected flight path.

      		==== Beeper Frequency

			The beeper on all Altus Metrum flight
			computers works best at 4000Hz, however if you
			have more than one flight computer in a single
			airframe, having all of them sound at the same
			frequency can be confusing. This parameter
			lets you adjust the base beeper frequency
			value.

      		==== Configure Pyro Channels

			.Additional Pyro Channel Configuration
			image::configure-pyro.png[width="5.5in"]

			This opens a separate window to configure the
			additional pyro channels available on TeleMega
			and EasyMega.  One column is presented for
			each channel. Each row represents a single
			parameter, if enabled the parameter must meet
			the specified test for the pyro channel to be
			fired.

			Select conditions and set the related value;
			the pyro channel will be activated when *all*
			of the conditions are met. Each pyro channel
			has a separate set of configuration values, so
			you can use different values for the same
			condition with different channels.

			At the bottom of the window, the 'Pyro Firing
			Time' configuration sets the length of time
			(in seconds) which each of these pyro channels
			will fire for.

			Once you have selected the appropriate
			configuration for all of the necessary pyro
			channels, you can save the pyro configuration
			along with the rest of the flight computer
			configuration by pressing the 'Save' button in
			the main Configure Flight Computer window.

			include::pyro-channels.raw[]

	=== Configure AltosUI

		image:configure-altosui.png[width="2.4in"]

		This button presents a dialog so that you can
		configure the AltosUI global settings.

            	==== Voice Settings

			AltosUI provides voice announcements during
			flight so that you can keep your eyes on the
			sky and still get information about the
			current flight status. However, sometimes you
			don't want to hear them.

			Enable::
			Turns all voice announcements on and off

			Test Voice::
			Plays a short message allowing you to verify
			that the audio system is working and the volume settings
			are reasonable

      		==== Log Directory

			AltosUI logs all telemetry data and saves all
			TeleMetrum flash data to this directory. This
			directory is also used as the staring point
			when selecting data files for display or
			export.

			Click on the directory name to bring up a
			directory choosing dialog, select a new
			directory and click 'Select Directory' to
			change where AltosUI reads and writes data
			files.

      		==== Callsign

			This value is transmitted in each command
			packet sent from TeleDongle and received from
			an altimeter.  It is not used in telemetry
			mode, as the callsign configured in the
			altimeter board is included in all telemetry
			packets.  Configure this with the AltosUI
			operators call sign as needed to comply with
			your local radio regulations.

			Note that to successfully command a flight
			computer over the radio (to configure the
			altimeter, monitor idle, or fire pyro
			charges), the callsign configured here must
			exactly match the callsign configured in the
			flight computer.  This matching is case
			sensitive.

		==== Imperial Units

			This switches between metric units (meters)
			and imperial units (feet and miles). This
			affects the display of values use during
			flight monitoring, configuration, data
			graphing and all of the voice
			announcements. It does not change the units
			used when exporting to CSV files, those are
			always produced in metric units.

		==== Font Size

			Selects the set of fonts used in the flight
			monitor window. Choose between the small,
			medium and large sets.

      		==== Serial Debug

			This causes all communication with a connected
			device to be dumped to the console from which
			AltosUI was started. If you've started it from
			an icon or menu entry, the output will simply
			be discarded. This mode can be useful to debug
			various serial communication issues.

		==== Manage Frequencies

			This brings up a dialog where you can
			configure the set of frequencies shown in the
			various frequency menus. You can add as many
			as you like, or even reconfigure the default
			set. Changing this list does not affect the
			frequency settings of any devices, it only
			changes the set of frequencies shown in the
			menus.

	=== Configure Groundstation

		image:configure-groundstation.png[width="3.1in"]

			Select this button and then select a
			TeleDongle or TeleBT Device from the list
			provided.

			The first few lines of the dialog provide
			information about the connected device,
			including the product name, software version
			and hardware serial number. Below that are the
			individual configuration entries.

			Note that TeleDongle and TeleBT don't save any
			configuration data, the settings here are
			recorded on the local machine in the Java
			preferences database. Moving the device to
			another machine, or using a different user
			account on the same machine will cause
			settings made here to have no effect.

			At the bottom of the dialog, there are three
			buttons:

			Save::
			This writes any changes to the local Java
			preferences file. If you don't press this
			button, any changes you make will be lost.

			Reset::
			This resets the dialog to the most recently
			saved values, erasing any changes you have
			made.

			Close::
			This closes the dialog. Any unsaved changes
			will be lost.

			The rest of the dialog contains the parameters
			to be configured.

		==== Frequency

			This configures the frequency to use for both
			telemetry and packet command mode. Set this
			before starting any operation involving packet
			command mode so that it will use the right
			frequency. Telemetry monitoring mode also
			provides a menu to change the frequency, and
			that menu also sets the same Java preference
			value used here.

		==== RF Calibration

			The radios in every Altus Metrum device are
			calibrated at the factory to ensure that they
			transmit and receive on the specified
			frequency.  To change a TeleDongle or TeleBT's
			calibration, you must reprogram the unit
			completely, so this entry simply shows the
			current value and doesn't allow any changes.

	      	==== Telemetry Rate

			This lets you match the telemetry and packet
			link rate from the transmitter. If they don't
			match, the device won't receive any data.

	=== Flash Image

		This reprograms Altus Metrum devices with new
		firmware. TeleMetrum v1.x, TeleDongle v0.2, TeleMini
		and TeleBT are all reprogrammed by using another
		similar unit as a programming dongle (pair
		programming). TeleMega, EasyMega, TeleMetrum v2,
		EasyMini and TeleDongle v3 are all programmed directly
		over their USB ports (self programming).  Please read
		the directions for flashing devices in the Updating
		Device Firmware chapter below.

	=== Fire Igniter

		.Fire Igniter Window
		image::fire-igniter.png[width="1.2in"]

		This activates the igniter circuits in the flight
		computer to help test recovery systems
		deployment. Because this command can operate over the
		Packet Command Link, you can prepare the rocket as for
		flight and then test the recovery system without
		needing to snake wires inside the air-frame.

		Selecting the 'Fire Igniter' button brings up the
		usual device selection dialog. Pick the desired
		device. This brings up another window which shows the
		current continuity test status for all of the pyro
		channels.

		Next, select the desired igniter to fire. This will
		enable the 'Arm' button.

		Select the 'Arm' button. This enables the 'Fire'
		button. The word 'Arm' is replaced by a countdown
		timer indicating that you have 10 seconds to press the
		'Fire' button or the system will deactivate, at which
		point you start over again at selecting the desired
		igniter.

	=== Scan Channels

		.Scan Channels Window
		image::scan-channels.png[width="3.2in"]

		This listens for telemetry packets on all of the
		configured frequencies, displaying information about
		each device it receives a packet from. You can select
		which of the baud rates and telemetry formats should
		be tried; by default, it only listens at 38400 baud
		with the standard telemetry format used in v1.0 and
		later firmware.

	=== Load Maps

		.Load Maps Window
		image::load-maps.png[width="5.2in"]

		Before heading out to a new launch site, you can use
		this to load satellite images in case you don't have
		internet connectivity at the site.

		There's a drop-down menu of launch sites we know
		about; if your favorites aren't there, please let us
		know the lat/lon and name of the site. The contents of
		this list are actually downloaded from our server at
		run-time, so as new sites are sent in, they'll get
		automatically added to this list.  If the launch site
		isn't in the list, you can manually enter the lat/lon
		values

		There are four different kinds of maps you can view;
		you can select which to download by selecting as many
		as you like from the available types:

		Hybrid::
		A combination of satellite imagery and road data. This
		is the default view.

		Satellite::
		Just the satellite imagery without any annotation.

		Roadmap::
		Roads, political boundaries and a few geographic
		features.

		Terrain::
		Contour intervals and shading that show hills and
		valleys.

		You can specify the range of zoom levels to download;
		smaller numbers show more area with less
		resolution. The default level, 0, shows about
		3m/pixel. One zoom level change doubles or halves that
		number. Larger zoom levels show more detail, smaller
		zoom levels less.

		The Map Radius value sets how large an area around the
		center point to download. Select a value large enough
		to cover any plausible flight from that site. Be aware
		that loading a large area with a high maximum zoom
		level can attempt to download a lot of data. Loading
		hybrid maps with a 10km radius at a minimum zoom of -2
		and a maximum zoom of 2 consumes about 120MB of
		space. Terrain and road maps consume about 1/10 as
		much space as satellite or hybrid maps.

		Clicking the 'Load Map' button will fetch images from
		Google Maps; note that Google limits how many images
		you can fetch at once, so if you load more than one
		launch site, you may get some gray areas in the map
		which indicate that Google is tired of sending data to
		you. Try again later.

	=== Monitor Idle

		.Monitor Idle Window
		image::monitor-idle.png[width="5.2in"]

		This brings up a dialog similar to the Monitor Flight
		UI, except it works with the altimeter in “idle” mode
		by sending query commands to discover the current
		state rather than listening for telemetry
		packets. Because this uses command mode, it needs to
		have the TeleDongle and flight computer callsigns
		match exactly. If you can receive telemetry, but
		cannot manage to run Monitor Idle, then it's very
		likely that your callsigns are different in some way.

		You can change the frequency and callsign used to
		communicate with the flight computer; they must both
		match the configuration in the flight computer
		exactly.
