<?xml version="1.0" encoding="utf-8" ?>
<!DOCTYPE book PUBLIC "-//OASIS//DTD DocBook XML V4.5//EN"
"/usr/share/xml/docbook/schema/dtd/4.5/docbookx.dtd">

<article>
  <para>
    Version 1.6.1 includes support for our updated TeleBT v3.0
    product and bug fixes in in the flight software for all our boards
    and ground station interfaces.
  </para>
  <para>
    AltOS New Features
    <itemizedlist>
      <listitem>
	<para>
	  Add support for TeleBT v3.0 boards.
	</para>
      </listitem>
      <listitem>
	<para>
	  Add support for uncompressed APRS data, providing support
	  for older APRS receivers. Uncompressed APRS data is less
	  precise, takes more bandwidth and doesn't have integrated
	  altitude data.
	</para>
      </listitem>
    </itemizedlist>
  </para>
  <para>
    AltOS Fixes
    <itemizedlist>
      <listitem>
	<para>
	  Make TeleDongle and TeleBT more tolerant of data rate
	  variations from transmitting devices.
	</para>
      </listitem>
    </itemizedlist>
  </para>
  <para>
    AltosUI and TeleGPS New Features
    <itemizedlist>
      <listitem>
	<para>
	  Add map to Monitor Idle display. It's nice to be able to
	  verify that maps are working, instead of needing to use
	  Monitor Flight.
	</para>
      </listitem>
    </itemizedlist>
  </para>
  <para>
    AltosUI Fixes
    <itemizedlist>
      <listitem>
	<para>
	  Fix frequency configuration to round values instead of
	  truncate them, avoiding a common 1kHz error in the setting.
	</para>
      </listitem>
      <listitem>
	<para>
	  Turn the Windows stub into a more useful program that can
	  launch the application with parameters so that file manager
	  icons work more reliably.
	</para>
      </listitem>
      <listitem>
	<para>
	  Force KML export to use a C locale so that numbers are
	  formatted with '.' instead of ',' for a decimal separator in
	  non-US locales. 
	</para>
      </listitem>
      <listitem>
	<para>
	  Preload map tiles based on distance rather than number of
	  tiles; this means you get the same resolution covering the
	  entire area, rather than having high resolution near the
	  center and low resolution further away.
	</para>
      </listitem>
      <listitem>
	<para>
	  Allow configuration of frequency and callsign in Monitor
	  Idle mode.
	</para>
      </listitem>
      <listitem>
	<para>
	  Fix layout weirdness when resizing windows on
	  Windows. Windows shouldn't have giant blank spaces around
	  the useful content anymore.
	</para>
      </listitem>
      <listitem>
	<para>
	  Fix layout weirdness when resizing windows on
	  Windows. Windows shouldn't have giant blank spaces around
	  the useful content anymore.
	</para>
      </listitem>
      <listitem>
	<para>
	  Use a longer filter for descent speed values. This should
	  provide something more useful on the display, although it
	  will take longer to respond to changes now.
	</para>
      </listitem>
      <listitem>
	<para>
	  Make Replay Flight run in realtime again. It had been set to
	  run at 10x speed by mistake.
	</para>
      </listitem>
    </itemizedlist>
  </para>
  <para>
    AltosDroid New Features
    <itemizedlist>
      <listitem>
	<para>
	  Add offline map support using mapping code from AltosUI.
	</para>
      </listitem>
      <listitem>
	<para>
	  Support TeleDongle (and TeleBT via USB) on devices
	  supporting USB On-The-Go.
	</para>
      </listitem>
      <listitem>
	<para>
	  Display additional TeleMega pyro channel status in Pad tab.
	</para>
      </listitem>
      <listitem>
	<para>
	  Switch between metric and imperial units.
	</para>
      </listitem>
      <listitem>
	<para>
	  Monitor TeleBT battery voltage.
	</para>
      </listitem>
      <listitem>
	<para>
	  Track multiple devices at the same time, selecting between
	  them with a menu or using the map.
	</para>
      </listitem>
      <listitem>
	<para>
	  Add hybrid, satellite and terrain map types.
	</para>
      </listitem>
    </itemizedlist>
  </para>
  <para>
    AltosDroid Fixes
    <itemizedlist>
      <listitem>
	<para>
	  Use standard Android display conventions so that a menu
	  button is available in the application title bar.
	</para>
      </listitem>
      <listitem>
	<para>
	  Adjust layout to work on large and small screens; shrinking
	  the go/no-go lights in smaller environments to try and make
	  everything visible.
	</para>
      </listitem>
      <listitem>
	<para>
	  Make voice announcements depend on current tab.
	</para>
      </listitem>
      <listitem>
	<para>
	  Compute adjustment to current travel direction while in
	  motion towards rocket.
	</para>
      </listitem>
    </itemizedlist>
  </para>
</article>
