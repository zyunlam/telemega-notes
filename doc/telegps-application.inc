== TeleGPS Application

	The TeleGPS application provides a graphical user interface for
	interacting with the Altus Metrum product family. TeleGPS can
	monitor telemetry data, configure devices and many other
	tasks. The primary interface window is for displaying data
	received over the telemetry link. There are additional
	tasks available from the main window menu bar.

	=== Telemetry Monitoring

		This is the window brought up when you start the
		application. If you have a TeleDongle device connected
		to the computer, it will automatically be selected for
		telemetry monitoring

		All telemetry data received are automatically recorded
		in suitable log files. The name of the files includes
		the current date and TeleGPS serial and flight
		numbers.

		The radio frequency being monitored by the TeleDongle
		device is displayed at the top of the window. You can
		configure the frequency by clicking on the frequency
		box and selecting the desired frequency. The TeleGPS
		application remembers the last frequency selected for
		each TeleDongle and selects that automatically the
		next time you use that device.

		Below the TeleDongle frequency selector, the window
		contains a few significant pieces of information about
		the altimeter providing the telemetry data stream:

		* The configured call-sign

		* The device serial number

		* The flight number. TeleGPS remembers how many times
                  it has flown.

		* The Received Signal Strength Indicator value. This
		  lets you know how strong a signal TeleDongle is
		  receiving. The radio inside TeleDongle operates down
		  to about -100dBm; weaker signals may not be
		  receivable. The packet link uses error detection and
		  correction techniques which prevent incorrect data
		  from being reported.

		* The age of the displayed data, in seconds since the
		  last successfully received telemetry packet.  In
		  normal operation this will stay in the low single
		  digits.  If the number starts counting up, then you
		  are no longer receiving data over the radio link
		  from the flight computer.

		Finally, the largest portion of the window contains a set of
		tabs, each of which contain some information about the TeleGPS
		board. The final 'table' tab displays many of the raw telemetry
		values in one place in a spreadsheet-like format.
      
		==== Map

			The Map tab shows the TeleGPS track over time
			on top of map data making it easy to locate
			the device.
        
			.TeleGPS Map View
			image::telegps-map.png[width="5.5in"]

			The map's default scale is approximately 3m
			(10ft) per pixel. The map can be dragged using
			the left mouse button. The map will attempt to
			keep the rocket roughly centered while data is
			being received.
        
			You can adjust the style of map and the zoom
			level with buttons on the right side of the
			map window. You can draw a line on the map by
			moving the mouse over the map with a button
			other than the left one pressed, or by
			pressing the left button while also holding
			down the shift key. The length of the line in
			real-world units will be shown at the start of
			the line.
	
			Images are fetched automatically via the
			Google Maps Static API, and cached on disk for
			reuse. If map images cannot be downloaded, the
			rocket's path will be traced on a dark gray
			background instead.
        
			You can pre-load images for your favorite
			launch sites before you leave home; check out
			the 'Preload Maps' section below.
	
		==== Location

			The Location tab shows the raw GPS data
			received from TeleGPS.
	
			.TeleGPS Location View
			image::telegps-location.png[width="5.5in"]

		==== Status

			The Status tab shows data relative to the
			location of TeleGPS when the application first
			received telemetry from it.
	
			.TeleGPS Status View
			image::telegps-status.png[width="5.5in"]

		==== Table

			The Table tab shows detailed information about
			the GPS receiver
	
			.TeleGPS Information Table
			image::telegps-table.png[width="5.5in"]

	=== TeleGPS Menus

		TeleGPS has three or four menus at the top of
		the window:

		File::

		New Window, Graph Data, Export Data, Load Maps,
		Preferences, Close and Exit
	      
		Monitor::

		Connect Device, Disconnect and Scan Channels

		Device::

		Download Data, Configure Device and Flash Device
	      
		Frequency::

		This shows the current monitoring frequency with a
		drop-down menu listing other configured
		frequencies. You can change the set of frequencies
		shown here from the Preferences dialog. This menu is
		only shown when the TeleGPS application is connected
		to a TeleDongle or TeleBT device.
	      
      
		==== New Window

			This creates another telemetry monitoring window, in case
			you have multiple TeleDongle devices connected to the
			computer.
	
	=== Graph Data

		The Graph tab shows a plot of the the GPS data
		collected. The X axis is time in seconds; there are a
		variety of Y axes available for different kinds of
		data. This window also allows you to see some
		statistics computed from the data, and an overall map
		of the entire data record.
	  
		==== Data Graph

			.TeleGPS Graph
			image::telegps-graph-graph.png[width="5.5in"]

		==== Graph Configuration

			.TeleGPS Graph Configuration
			image::telegps-graph-configure.png[width="5.5in"]

			This selects which graph elements to show, and, at the
			bottom, lets you switch between metric and imperial
			units

		==== Statistics

			.TeleGPS Statistics
			image::telegps-graph-stats.png[width="5.5in"]

			Shows overall data computed from the flight.
		
		==== Map

			.TeleGPS Map
			image::telegps-graph-map.png[width="6in"]

			Shows a map of the area overlaid with the GPS track. As with
			the telemetry monitoring window, you can select the style
			of map and zoom level using buttons along the side;
			you can scroll the map by dragging within the map pressing
			the left button and you can draw a line to measure
			distances using either the left button with the shift key,
			or any other button.

	=== Export Data

		This tool takes the raw data files and makes them
		available for external analysis. When you select this
		button, you are prompted to select a data file, which
		can be either a .eeprom or .telem.  The .eeprom files
		contain higher resolution and more continuous data,
		while .telem files contain receiver signal strength
		information.  Next, a second dialog appears which is
		used to select where to write the resulting file. It
		has a selector to choose between CSV and KML file
		formats.
	
		==== Comma Separated Value Format

			This is a text file containing the data in a
			form suitable for import into a spreadsheet or
			other external data analysis tool. The first
			few lines of the file contain the version and
			configuration information from TeleGPS, then
			there is a single header line which labels all
			of the fields. All of these lines start with a
			'#' character which many tools can be
			configured to skip over.

			The remaining lines of the file contain the
			data, with each field separated by a comma and
			at least one space. All of the sensor values
			are converted to standard units, with the
			barometric data reported in both pressure,
			altitude and height above pad units.

		==== Keyhole Markup Language (for Google Earth)

			This is the format used by Google Earth to provide an overlay
			within that application. With this, you can use Google Earth to
			see the whole path in 3D.

	=== Load Maps

		.Load Maps Window
		image::load-maps.png[width="5.2in"]

		Before using TeleGPS, you can use Load Maps to load
		map data in case you don't have access to the internet
		while receiving telemetry.

		There's a drop-down menu of rocket launch sites we
		know about; if your favorites aren't there, please let
		us know the lat/lon and name of the site. The contents
		of this list are actually downloaded from our server
		at run-time, so as new sites are sent in, they'll get
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

	=== Preferences

		.TeleGPS Preferences Window
		image::telegps-preferences.png[width="2.4in"]

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

	=== Close

		This closes the current window, leaving any other windows
		open and the application running.
	
	=== Exit

		This closes all TeleGPS windows and terminates the
		application.
	
	=== Connect Device

		Selecting this item brings up a dialog box listing all
		of the connected TeleDongle devices. When you choose
		one of these, AltosUI will display telemetry data as
		received by the selected TeleDongle device.
	
		.Device Selection Dialog
		image::device-selection.png[width="3.1in"]

	=== Disconnect

		Disconnects the currently connected TeleDongle or
		TeleBT

	=== Scan Channels

		.Radio Scanning Dialog
		image::telegps-scan.png[width="3.1in"]

		Scans the configured set of frequencies looking for
		telemetry signals. A list of all of the discovered
		signals is show; selecting one of those and clicking
		on 'Monitor' will select that frequency in the
		associated TeleGPS application window.

	=== Download Data

		TeleGPS records data to its internal flash memory.
		On-board data is recorded at the same rate as
		telemetry but is not subject to radio drop-outs. As
		such, it generally provides a more complete and
		precise record.  The 'Download Data' menu entry allows
		you to read the flash memory and write it to disk.

		Select the 'Download Data' menu entry to bring up a
		list of connected TeleGPS devices. After the device
		has been selected, a dialog showing the data stored in
		the device will be shown allowing you to select which
		entries to download and which to delete. You must
		erase flights in order for the space they consume to
		be reused by another track. This prevents accidentally
		losing data if you neglect to download data before
		starting TeleGPS again. Note that if there is no more
		space available in the device, then no data will be
		recorded.

		The file name for each data log is computed
		automatically from the recorded date, altimeter serial
		number and flight number information.
	
	=== Configure Device

		.TeleGPS Configuration Dialog
		image::telegps-configure.png[width="3.6in"]

		Select this button and then select any connected TeleGPS
		device from the list provided.

		The first few lines of the dialog provide information
		about the connected device, including the product
		name, software version and hardware serial
		number. Below that are the individual configuration
		entries.

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

          The rest of the dialog contains the parameters to be configured.
	
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

		==== Logging Trigger Motion

			If TeleGPS moves less than this distance over
			a long period of time, it will not log that
			location, saving storage space.

		==== Position Reporting Interval

			This sets how often TeleGPS reports position
			information via telemetry and to the on-board
			log. Reducing this value will save power and
			logging memory consumption.

	=== Flash Device

		This reprograms TeleGPS devices with new
		firmware. Please read the directions for flashing
		devices in the Updating Device Firmware chapter below.
