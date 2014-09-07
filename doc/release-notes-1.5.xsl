<?xml version="1.0" encoding="utf-8" ?>
<!DOCTYPE book PUBLIC "-//OASIS//DTD DocBook XML V4.5//EN"
"/usr/share/xml/docbook/schema/dtd/4.5/docbookx.dtd">

<article>
  <para>
    Version 1.5 is a major release. It includes support for our new
    EasyMega product, new features and bug fixes in in the flight
    software for all our boards and the AltosUI ground station
  </para>
  <para>
    AltOS New Features
    <itemizedlist>
      <listitem>
	<para>
	  Add support for EasyMega boards.
	</para>
      </listitem>
      <listitem>
	<para>
	  Make the APRS SSID be configurable. This lets you track
	  different rockets on the same receiver without getting
	  things mixed up.
	</para>
      </listitem>
      <listitem>
	<para>
	  Report extra pyro channel continuity state on EasyMega and
	  TeleMega via the beeper. This lets you easily verify flight
	  readiness on these boards after powering up the electronics
	  on the rail.
	</para>
      </listitem>
      <listitem>
	<para>
	  Add lower telemetry data rates (2400 and 9600 bps) to
	  increase telemetry radio range. This reduces the amount of
	  data received as well as increasing battery consumption in
	  the transmitter.
	</para>
      </listitem>
      <listitem>
	<para>
	  Change TeleGPS to have only a single log, and append new
	  data to it rather than using seperate per-flight logs. This
	  avoids accidentally filling up log storage by turning
	  TeleGPS on/off several times.
	</para>
      </listitem>
    </itemizedlist>
  </para>
  <para>
    AltOS Fixes
    <itemizedlist>
      <listitem>
	<para>
	  Increase the maximum range for altitude values from +/-32767m
	  to +/-2147483647m, allowing the flight computers to function
	  correctly above the 32km level.
	</para>
      </listitem>
      <listitem>
	<para>
	  Continuously test pyro firing conditions during delay stage,
	  inhibiting the pyro channel if the test fails. This prevents
	  firing pyro charges where the conditions were good before
	  the delay, but become bad before the delay expires.
	</para>
      </listitem>
      <listitem>
	<para>
	  Allow negative numbers in pyro configuration values. This
	  lets you specify things like descending speed or
	  deceleration.
	</para>
      </listitem>
    </itemizedlist>
  </para>
  <para>
    AltosUI and TeleGPS New Features
    <itemizedlist>
      <listitem>
	<para>
	  Support telemetry baud rate selection. Adds menus to
	  the flight monitoring and configuration for baud rate
	  selection.
	</para>
      </listitem>
      <listitem>
	<para>
	  Support APRS SSID configuration.
	</para>
      </listitem>
      <listitem>
	<para>
	  Integrate with file managers. This provides icons for all of
	  our file types and associates our application with the files
	  so that using a file manager to open a AltOS data file
	  results in launching our application.
	</para>
      </listitem>
    </itemizedlist>
  </para>
  <para>
    AltosUI Fixes
    <itemizedlist>
      <listitem>
	<para>
	  Make the 'Graph' button on the landed tab work again.
	</para>
      </listitem>
      <listitem>
	<para>
	  Make tests for Java on Windows a bit smarter, and also
	  provide the user with the option to skip installing Java for
	  cases where we just can't figure out what version is installed.
	</para>
      </listitem>
    </itemizedlist>
  </para>
</article>
