<?xml version="1.0" encoding="utf-8" ?>
<!DOCTYPE book PUBLIC "-//OASIS//DTD DocBook XML V4.5//EN"
"/usr/share/xml/docbook/schema/dtd/4.5/docbookx.dtd">

<article>
  <para>
    Version 1.0.2 is a bugfix release, addressing a minor issue
    found in version 1.0.1
  </para>
  <para>
    AltOS Firmware Changes
    <itemizedlist>
      <listitem>
	On TeleMetrum, wait to enable the radio link for remote command operations
	until the device enters either idle or invalid mode. Ticket #26.
      </listitem>
      <listitem>
	On TeleMini, delay during reboot for one second to give the
	TeleDongle time to leave radio link mode. Otherwise, the
	TeleDongle would send another radio link packet out while the
	TeleMini was rebooting, sending TeleMini right back to idle
	mode. Ticket #27.
      </listitem>
    </itemizedlist>
  </para>
</article>
