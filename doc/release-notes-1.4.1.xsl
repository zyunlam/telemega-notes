<?xml version="1.0" encoding="utf-8" ?>
<!DOCTYPE book PUBLIC "-//OASIS//DTD DocBook XML V4.5//EN"
"/usr/share/xml/docbook/schema/dtd/4.5/docbookx.dtd">

<article>
  <para>
    Version 1.4.1 is a minor release. It fixes install issues on
    Windows and provides the missing TeleMetrum V2.0 firmware. There
    aren't any changes to the firmware or host applications at
    all. All Windows users will want to upgrade to get the signed
    driver, but Mac and Linux users who do not need the TeleMetrum
    V2.0 firmware image will not need to upgrade.
  </para>
  <para>
    Windows Install Fixes
    <itemizedlist>
      <listitem>
	<para>
	  Provide signed Windows driver files. This should avoid any need to
	  disable driver signature checking on Windows 7 or 8.
	</para>
      </listitem>
      <listitem>
	<para>
	  Fix Java version detection and download. Previously, the
	  installer would only look for Java 6 or 7 and insist on
	  downloading its own Java bits if there was something else
	  installed. Furthermore, the 64-bit Java link provided didn't
	  work for anyone other than Keith, making it impossible to
	  install AltOS on any machine with Java SE 8 installed.
	</para>
      </listitem>
    </itemizedlist>
  </para>
  <para>
    Other Fixes
    <itemizedlist>
      <listitem>
	<para>
	  Include 1.4 firmware for TeleMetrum V2.0. None of the
	  installers shipped this file. Now it's included in the AltOS
	  packages for Linux, Mac and Windows.
	</para>
      </listitem>
    </itemizedlist>
  </para>
</article>
