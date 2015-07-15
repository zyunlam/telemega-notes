<?xml version="1.0" encoding="utf-8" ?>
<!DOCTYPE article PUBLIC "-//OASIS//DTD DocBook XML V4.5//EN"
"/usr/share/xml/docbook/schema/dtd/4.5/docbookx.dtd">

<article>
  <articleinfo>
    <title>AltOS Telemetry</title>
    <subtitle>Packet Definitions</subtitle>
    <author>
      <firstname>Keith</firstname>
      <surname>Packard</surname>
    </author>
    <copyright>
      <year>2011</year>
      <holder>Keith Packard</holder>
    </copyright>
    <legalnotice>
      <para>
	This document is released under the terms of the
	<ulink url="http://creativecommons.org/licenses/by-sa/3.0/">
	  Creative Commons ShareAlike 3.0
	</ulink>
	license.
      </para>
    </legalnotice>
    <revhistory>
      <revision>
	<revnumber>0.1</revnumber>
	<date>01 July 2011</date>
	<revremark>Initial content</revremark>
      </revision>
    </revhistory>
  </articleinfo>
  <section>
    <title>Packet Format Design</title>
    <para>
      AltOS telemetry data is split into multiple different packets,
      all the same size, but each includs an identifier so that the
      ground station can distinguish among different types. A single
      flight board will transmit multiple packet types, each type on a
      different schedule. The ground software need look for only a
      single packet size, and then decode the information within the
      packet and merge data from multiple packets to construct the
      full flight computer state.
    </para>
    <para>
      Each AltOS packet is 32 bytes long. This size was chosen based
      on the known telemetry data requirements. The power of two size
      allows them to be stored easily in flash memory without having
      them split across blocks or leaving gaps at the end.
    </para>
    <para>
      All packet types start with a five byte header which encodes the
      device serial number, device clock value and the packet
      type. The remaining 27 bytes encode type-specific data.
    </para>
  </section>
  <section>
    <title>Packet Formats</title>
    <para>
      This section first defines the packet header common to all packets
      and then the per-packet data layout.
    </para>
    <section>
      <title>Packet Header</title>
      <table frame='all'>
	<title>Telemetry Packet Header</title>
	<tgroup cols='4' align='center' colsep='1' rowsep='1'>
	  <colspec align='center' colwidth='*' colname='Offset'/>
	  <colspec align='center' colwidth='3*' colname='Data Type'/>
	  <colspec align='left' colwidth='3*' colname='Name'/>
	  <colspec align='left' colwidth='9*' colname='Description'/>
	  <thead>
	    <row>
	      <entry align='center'>Offset</entry>
	      <entry align='center'>Data Type</entry>
	      <entry align='center'>Name</entry>
	      <entry align='center'>Description</entry>
	    </row>
	  </thead>
	  <tbody>
	    <row>
	      <entry>0</entry>
	      <entry>uint16_t</entry>
	      <entry>serial</entry>
	      <entry>Device serial Number</entry>
	    </row>
	    <row>
	      <entry>2</entry>
	      <entry>uint16_t</entry>
	      <entry>tick</entry>
	      <entry>Device time in 100ths of a second</entry>
	    </row>
	    <row>
	      <entry>4</entry>
	      <entry>uint8_t</entry>
	      <entry>type</entry>
	      <entry>Packet type</entry>
	    </row>
	    <row>
	      <entry>5</entry>
	    </row>
	  </tbody>
	</tgroup>
      </table>
      <para>
      Each packet starts with these five bytes which serve to identify
      which device has transmitted the packet, when it was transmitted
      and what the rest of the packet contains.
      </para>
    </section>
    <section>
      <title>TeleMetrum v1.x, TeleMini and TeleNano Sensor Data</title>
      <informaltable frame='none' label='' tocentry='0'>
	<tgroup cols='2' align='center' colsep='1' rowsep='1'>
	  <colspec align='center' colwidth='*' colname='Offset'/>
	  <colspec align='left' colwidth='3*' colname='Description'/>
	  <thead>
	    <row>
	      <entry>Type</entry>
	      <entry>Description</entry>
	    </row>
	  </thead>
	  <tbody>
	    <row>
	      <entry>0x01</entry>
	      <entry>TeleMetrum v1.x Sensor Data</entry>
	    </row>
	    <row>
	      <entry>0x02</entry>
	      <entry>TeleMini Sensor Data</entry>
	    </row>
	    <row>
	      <entry>0x03</entry>
	      <entry>TeleNano Sensor Data</entry>
	    </row>
	  </tbody>
	</tgroup>
      </informaltable>
      <para>
	TeleMetrum v1.x, TeleMini and TeleNano share this same packet
	format for sensor data. Each uses a distinct packet type so
	that the receiver knows which data values are valid and which
	are undefined.
      </para>
      <para>
	Sensor Data packets are transmitted once per second on the
	ground, 10 times per second during ascent and once per second
	during descent and landing
      </para>
      <table frame='all'>
	<title>Sensor Packet Contents</title>
	<tgroup cols='4' align='center' colsep='1' rowsep='1'>
	  <colspec align='center' colwidth='*' colname='Offset'/>
	  <colspec align='center' colwidth='3*' colname='Data Type'/>
	  <colspec align='left' colwidth='3*' colname='Name'/>
	  <colspec align='left' colwidth='9*' colname='Description'/>
	  <thead>
	    <row>
	      <entry align='center'>Offset</entry>
	      <entry align='center'>Data Type</entry>
	      <entry align='center'>Name</entry>
	      <entry align='center'>Description</entry>
	    </row>
	  </thead>
	  <tbody>
	    <row>
	      <entry>5</entry><entry>uint8_t</entry><entry>state</entry><entry>Flight state</entry>
	    </row>
	    <row>
	      <entry>6</entry><entry>int16_t</entry><entry>accel</entry><entry>accelerometer (TM only)</entry>
	    </row>
	    <row>
	      <entry>8</entry><entry>int16_t</entry><entry>pres</entry><entry>pressure sensor</entry>
	    </row>
	    <row>
	      <entry>10</entry><entry>int16_t</entry><entry>temp</entry><entry>temperature sensor</entry>
	    </row>
	    <row>
	      <entry>12</entry><entry>int16_t</entry><entry>v_batt</entry><entry>battery voltage</entry>
	    </row>
	    <row>
	      <entry>14</entry><entry>int16_t</entry><entry>sense_d</entry><entry>drogue continuity sense (TM/Tm)</entry>
	    </row>
	    <row>
	      <entry>16</entry><entry>int16_t</entry><entry>sense_m</entry><entry>main continuity sense (TM/Tm)</entry>
	    </row>
	    <row>
	      <entry>18</entry><entry>int16_t</entry><entry>acceleration</entry><entry>m/s² * 16</entry>
	    </row>
	    <row>
	      <entry>20</entry><entry>int16_t</entry><entry>speed</entry><entry>m/s * 16</entry>
	    </row>
	    <row>
	      <entry>22</entry><entry>int16_t</entry><entry>height</entry><entry>m</entry>
	    </row>
	    <row>
	      <entry>24</entry><entry>int16_t</entry><entry>ground_pres</entry><entry>Average barometer reading on ground</entry>
	    </row>
	    <row>
	      <entry>26</entry><entry>int16_t</entry><entry>ground_accel</entry><entry>TM</entry>
	    </row>
	    <row>
	      <entry>28</entry><entry>int16_t</entry><entry>accel_plus_g</entry><entry>TM</entry>
	    </row>
	    <row>
	      <entry>30</entry><entry>int16_t</entry><entry>accel_minus_g</entry><entry>TM</entry>
	    </row>
	    <row>
	      <entry>32</entry>
	    </row>
	  </tbody>
	</tgroup>
      </table>
    </section>
    <section>
      <title>TeleMega Sensor  Data</title>
      <informaltable frame='none' label='' tocentry='0'>
	<tgroup cols='2' align='center' colsep='1' rowsep='1'>
	  <colspec align='center' colwidth='*' colname='Offset'/>
	  <colspec align='left' colwidth='3*' colname='Description'/>
	  <thead>
	    <row>
	      <entry>Type</entry>
	      <entry>Description</entry>
	    </row>
	  </thead>
	  <tbody>
	    <row>
	      <entry>0x08</entry>
	      <entry>TeleMega IMU Sensor Data</entry>
	    </row>
	    <row>
	      <entry>0x09</entry>
	      <entry>TeleMega Kalman and Voltage Data</entry>
	    </row>
	  </tbody>
	</tgroup>
      </informaltable>
      <para>
	TeleMega has a lot of sensors, and so it splits the sensor
	data into two packets. The raw IMU data are sent more often;
	the voltage values don't change very fast, and the Kalman
	values can be reconstructed from the IMU data.
      </para>
      <para>
	IMU Sensor Data packets are transmitted once per second on the
	ground, 10 times per second during ascent and once per second
	during descent and landing
      </para>
      <para>
	Kalman and Voltage Data packets are transmitted once per second on the
	ground, 5 times per second during ascent and once per second
	during descent and landing
      </para>
      <para>
	The high-g accelerometer is reported separately from the data
	for the 9-axis IMU (accel/gyro/mag). The 9-axis IMU is mounted
	so that the X axis is "across" the board (along the short
	axis0, the Y axis is "along" the board (along the long axis,
	with the high-g accelerometer) and the Z axis is "through" the
	board (perpendicular to the board). Rotation measurements are
	around the respective axis, so Y rotation measures the spin
	rate of the rocket while X and Z rotation measure the tilt
	rate.
      </para>
      <para>
	The overall tilt angle of the rocket is computed by first
	measuring the orientation of the rocket on the pad using the 3
	axis accelerometer, and then integrating the overall tilt rate
	from the 3 axis gyroscope to compute the total orientation
	change of the airframe since liftoff.
      </para>
      <table frame='all'>
	<title>TeleMega IMU Sensor Packet Contents</title>
	<tgroup cols='4' align='center' colsep='1' rowsep='1'>
	  <colspec align='center' colwidth='*' colname='Offset'/>
	  <colspec align='center' colwidth='3*' colname='Data Type'/>
	  <colspec align='left' colwidth='3*' colname='Name'/>
	  <colspec align='left' colwidth='9*' colname='Description'/>
	  <thead>
	    <row>
	      <entry align='center'>Offset</entry>
	      <entry align='center'>Data Type</entry>
	      <entry align='center'>Name</entry>
	      <entry align='center'>Description</entry>
	    </row>
	  </thead>
	  <tbody>
	    <row>
	      <entry>5</entry><entry>uint8_t</entry><entry>orient</entry><entry>Angle from vertical in degrees</entry>
	    </row>
	    <row>
	      <entry>6</entry><entry>int16_t</entry><entry>accel</entry><entry>High G accelerometer</entry>
	    </row>
	    <row>
	      <entry>8</entry><entry>int32_t</entry><entry>pres</entry><entry>pressure (Pa * 10)</entry>
	    </row>
	    <row>
	      <entry>12</entry><entry>int16_t</entry><entry>temp</entry><entry>temperature (°C * 100)</entry>
	    </row>
	    <row>
	      <entry>14</entry><entry>int16_t</entry><entry>accel_x</entry><entry>X axis acceleration (across)</entry>
	    </row>
	    <row>
	      <entry>16</entry><entry>int16_t</entry><entry>accel_y</entry><entry>Y axis acceleration (along)</entry>
	    </row>
	    <row>
	      <entry>18</entry><entry>int16_t</entry><entry>accel_z</entry><entry>Z axis acceleration (through)</entry>
	    </row>
	    <row>
	      <entry>20</entry><entry>int16_t</entry><entry>gyro_x</entry><entry>X axis rotation (across)</entry>
	    </row>
	    <row>
	      <entry>22</entry><entry>int16_t</entry><entry>gyro_y</entry><entry>Y axis rotation (along)</entry>
	    </row>
	    <row>
	      <entry>24</entry><entry>int16_t</entry><entry>gyro_z</entry><entry>Z axis rotation (through)</entry>
	    </row>
	    <row>
	      <entry>26</entry><entry>int16_t</entry><entry>mag_x</entry><entry>X field strength (across)</entry>
	    </row>
	    <row>
	      <entry>28</entry><entry>int16_t</entry><entry>mag_y</entry><entry>Y field strength (along)</entry>
	    </row>
	    <row>
	      <entry>30</entry><entry>int16_t</entry><entry>mag_z</entry><entry>Z field strength (through)</entry>
	    </row>
	    <row>
	      <entry>32</entry>
	    </row>
	  </tbody>
	</tgroup>
      </table>
      <table frame='all'>
	<title>TeleMega Kalman and Voltage Data Packet Contents</title>
	<tgroup cols='4' align='center' colsep='1' rowsep='1'>
	  <colspec align='center' colwidth='*' colname='Offset'/>
	  <colspec align='center' colwidth='3*' colname='Data Type'/>
	  <colspec align='left' colwidth='3*' colname='Name'/>
	  <colspec align='left' colwidth='9*' colname='Description'/>
	  <thead>
	    <row>
	      <entry align='center'>Offset</entry>
	      <entry align='center'>Data Type</entry>
	      <entry align='center'>Name</entry>
	      <entry align='center'>Description</entry>
	    </row>
	  </thead>
	  <tbody>
	    <row>
	      <entry>5</entry><entry>uint8_t</entry><entry>state</entry><entry>Flight state</entry>
	    </row>
	    <row>
	      <entry>6</entry><entry>int16_t</entry><entry>v_batt</entry><entry>battery voltage</entry>
	    </row>
	    <row>
	      <entry>8</entry><entry>int16_t</entry><entry>v_pyro</entry><entry>pyro battery voltage</entry>
	    </row>
	    <row>
	      <entry>10</entry><entry>int8_t[6]</entry><entry>sense</entry><entry>pyro continuity sense</entry>
	    </row>
	    <row>
	      <entry>16</entry><entry>int32_t</entry><entry>ground_pres</entry><entry>Average barometer reading on ground</entry>
	    </row>
	    <row>
	      <entry>20</entry><entry>int16_t</entry><entry>ground_accel</entry><entry>Average accelerometer reading on ground</entry>
	    </row>
	    <row>
	      <entry>22</entry><entry>int16_t</entry><entry>accel_plus_g</entry><entry>Accel calibration at +1g</entry>
	    </row>
	    <row>
	      <entry>24</entry><entry>int16_t</entry><entry>accel_minus_g</entry><entry>Accel calibration at -1g</entry>
	    </row>
	    <row>
	      <entry>26</entry><entry>int16_t</entry><entry>acceleration</entry><entry>m/s² * 16</entry>
	    </row>
	    <row>
	      <entry>28</entry><entry>int16_t</entry><entry>speed</entry><entry>m/s * 16</entry>
	    </row>
	    <row>
	      <entry>30</entry><entry>int16_t</entry><entry>height</entry><entry>m</entry>
	    </row>
	    <row>
	      <entry>32</entry>
	    </row>
	  </tbody>
	</tgroup>
      </table>
    </section>
    <section>
      <title>TeleMetrum v2 Sensor  Data</title>
      <informaltable frame='none' label='' tocentry='0'>
	<tgroup cols='2' align='center' colsep='1' rowsep='1'>
	  <colspec align='center' colwidth='*' colname='Offset'/>
	  <colspec align='left' colwidth='3*' colname='Description'/>
	  <thead>
	    <row>
	      <entry>Type</entry>
	      <entry>Description</entry>
	    </row>
	  </thead>
	  <tbody>
	    <row>
	      <entry>0x0A</entry>
	      <entry>TeleMetrum v2 Sensor Data</entry>
	    </row>
	    <row>
	      <entry>0x0B</entry>
	      <entry>TeleMetrum v2 Calibration Data</entry>
	    </row>
	  </tbody>
	</tgroup>
      </informaltable>
      <para>
	TeleMetrum v2 has higher resolution barometric data than
	TeleMetrum v1, and so the constant calibration data is
	split out into a separate packet.
      </para>
      <para>
	TeleMetrum v2 Sensor Data packets are transmitted once per second on the
	ground, 10 times per second during ascent and once per second
	during descent and landing
      </para>
      <para>
	TeleMetrum v2 Calibration Data packets are always transmitted once per second.
      </para>
      <table frame='all'>
	<title>TeleMetrum v2 Sensor Packet Contents</title>
	<tgroup cols='4' align='center' colsep='1' rowsep='1'>
	  <colspec align='center' colwidth='*' colname='Offset'/>
	  <colspec align='center' colwidth='3*' colname='Data Type'/>
	  <colspec align='left' colwidth='3*' colname='Name'/>
	  <colspec align='left' colwidth='9*' colname='Description'/>
	  <thead>
	    <row>
	      <entry align='center'>Offset</entry>
	      <entry align='center'>Data Type</entry>
	      <entry align='center'>Name</entry>
	      <entry align='center'>Description</entry>
	    </row>
	  </thead>
	  <tbody>
	    <row>
	      <entry>5</entry><entry>uint8_t</entry><entry>state</entry><entry>Flight state</entry>
	    </row>
	    <row>
	      <entry>6</entry><entry>int16_t</entry><entry>accel</entry><entry>accelerometer</entry>
	    </row>
	    <row>
	      <entry>8</entry><entry>int32_t</entry><entry>pres</entry><entry>pressure sensor (Pa * 10)</entry>
	    </row>
	    <row>
	      <entry>12</entry><entry>int16_t</entry><entry>temp</entry><entry>temperature sensor (°C * 100)</entry>
	    </row>

	    <row>
	      <entry>14</entry><entry>int16_t</entry><entry>acceleration</entry><entry>m/s² * 16</entry>
	    </row>
	    <row>
	      <entry>16</entry><entry>int16_t</entry><entry>speed</entry><entry>m/s * 16</entry>
	    </row>
	    <row>
	      <entry>18</entry><entry>int16_t</entry><entry>height</entry><entry>m</entry>
	    </row>

	    <row>
	      <entry>20</entry><entry>int16_t</entry><entry>v_batt</entry><entry>battery voltage</entry>
	    </row>
	    <row>
	      <entry>22</entry><entry>int16_t</entry><entry>sense_d</entry><entry>drogue continuity sense</entry>
	    </row>
	    <row>
	      <entry>24</entry><entry>int16_t</entry><entry>sense_m</entry><entry>main continuity sense</entry>
	    </row>
	    <row>
	      <entry>26</entry><entry>pad[6]</entry><entry>pad bytes</entry><entry></entry>
	    </row>
	    <row>
	      <entry>32</entry>
	    </row>
	  </tbody>
	</tgroup>
      </table>
      <table frame='all'>
	<title>TeleMetrum v2 Calibration Data Packet Contents</title>
	<tgroup cols='4' align='center' colsep='1' rowsep='1'>
	  <colspec align='center' colwidth='*' colname='Offset'/>
	  <colspec align='center' colwidth='3*' colname='Data Type'/>
	  <colspec align='left' colwidth='3*' colname='Name'/>
	  <colspec align='left' colwidth='9*' colname='Description'/>
	  <thead>
	    <row>
	      <entry align='center'>Offset</entry>
	      <entry align='center'>Data Type</entry>
	      <entry align='center'>Name</entry>
	      <entry align='center'>Description</entry>
	    </row>
	  </thead>
	  <tbody>
	    <row>
	      <entry>5</entry><entry>pad[3]</entry><entry>pad bytes</entry><entry></entry>
	    </row>
	    <row>
	      <entry>8</entry><entry>int32_t</entry><entry>ground_pres</entry><entry>Average barometer reading on ground</entry>
	    </row>
	    <row>
	      <entry>12</entry><entry>int16_t</entry><entry>ground_accel</entry><entry>Average accelerometer reading on ground</entry>
	    </row>
	    <row>
	      <entry>14</entry><entry>int16_t</entry><entry>accel_plus_g</entry><entry>Accel calibration at +1g</entry>
	    </row>
	    <row>
	      <entry>16</entry><entry>int16_t</entry><entry>accel_minus_g</entry><entry>Accel calibration at -1g</entry>
	    </row>
	    <row>
	      <entry>18</entry><entry>pad[14]</entry><entry>pad bytes</entry><entry></entry>
	    </row>
	    <row>
	      <entry>32</entry>
	    </row>
	  </tbody>
	</tgroup>
      </table>
    </section>
    <section>
      <title>Configuration Data</title>
      <informaltable frame='none' label='' tocentry='0'>
	<tgroup cols='2' align='center' colsep='1' rowsep='1'>
	  <colspec align='center' colwidth='*' colname='Offset'/>
	  <colspec align='left' colwidth='3*' colname='Description'/>
	  <thead>
	    <row>
	      <entry>Type</entry>
	      <entry>Description</entry>
	    </row>
	  </thead>
	  <tbody>
	    <row>
	      <entry>0x04</entry>
	      <entry>Configuration Data</entry>
	    </row>
	  </tbody>
	</tgroup>
      </informaltable>
      <para>
	This provides a description of the software installed on the
	flight computer as well as any user-specified configuration data.
      </para>
      <para>
	Configuration data packets are transmitted once per second
	during all phases of the flight
      </para>
      <table frame='all'>
	<title>Sensor Packet Contents</title>
	<tgroup cols='4' align='center' colsep='1' rowsep='1'>
	  <colspec align='center' colwidth='*' colname='Offset'/>
	  <colspec align='center' colwidth='3*' colname='Data Type'/>
	  <colspec align='left' colwidth='3*' colname='Name'/>
	  <colspec align='left' colwidth='9*' colname='Description'/>
	  <thead>
	    <row>
	      <entry align='center'>Offset</entry>
	      <entry align='center'>Data Type</entry>
	      <entry align='center'>Name</entry>
	      <entry align='center'>Description</entry>
	    </row>
	  </thead>
	  <tbody>
	    <row>
	      <entry>5</entry><entry>uint8_t</entry><entry>type</entry><entry>Device type</entry>
	    </row>
	    <row>
	      <entry>6</entry><entry>uint16_t</entry><entry>flight</entry><entry>Flight number</entry>
	    </row>
	    <row>
	      <entry>8</entry><entry>uint8_t</entry><entry>config_major</entry><entry>Config major version</entry>
	    </row>
	    <row>
	      <entry>9</entry><entry>uint8_t</entry><entry>config_minor</entry><entry>Config minor version</entry>
	    </row>
	    <row>
	      <entry>10</entry><entry>uint16_t</entry><entry>apogee_delay</entry>
	      <entry>Apogee deploy delay in seconds</entry>
	    </row>
	    <row>
	      <entry>12</entry><entry>uint16_t</entry><entry>main_deploy</entry><entry>Main deploy alt in meters</entry>
	    </row>
	    <row>
	      <entry>14</entry><entry>uint16_t</entry><entry>flight_log_max</entry>
	      <entry>Maximum flight log size (kB)</entry>
	    </row>
	    <row>
	      <entry>16</entry><entry>char</entry><entry>callsign[8]</entry><entry>Radio operator identifier</entry>
	    </row>
	    <row>
	      <entry>24</entry><entry>char</entry><entry>version[8]</entry><entry>Software version identifier</entry>
	    </row>
	    <row>
	      <entry>32</entry>
	    </row>
	  </tbody>
	</tgroup>
      </table>
    </section>
    <section>
      <title>GPS Location</title>
      <informaltable frame='none' label='' tocentry='0'>
	<tgroup cols='2' align='center' colsep='1' rowsep='1'>
	  <colspec align='center' colwidth='*' colname='Offset'/>
	  <colspec align='left' colwidth='3*' colname='Description'/>
	  <thead>
	    <row>
	      <entry>Type</entry>
	      <entry>Description</entry>
	    </row>
	  </thead>
	  <tbody>
	    <row>
	      <entry>0x05</entry>
	      <entry>GPS Location</entry>
	    </row>
	  </tbody>
	</tgroup>
      </informaltable>
      <para>
	This packet provides all of the information available from the
	GPS receiver—position, time, speed and precision
	estimates. 
      </para>
      <para>
	GPS Location packets are transmitted once per second during
	all phases of the flight
      </para>
      <table frame='all'>
	<title>GPS Location Packet Contents</title>
	<tgroup cols='4' align='center' colsep='1' rowsep='1'>
	  <colspec align='center' colwidth='*' colname='Offset'/>
	  <colspec align='center' colwidth='3*' colname='Data Type'/>
	  <colspec align='left' colwidth='3*' colname='Name'/>
	  <colspec align='left' colwidth='9*' colname='Description'/>
	  <thead>
	    <row>
	      <entry align='center'>Offset</entry>
	      <entry align='center'>Data Type</entry>
	      <entry align='center'>Name</entry>
	      <entry align='center'>Description</entry>
	    </row>
	  </thead>
	  <tbody>
	    <row>
	      <entry>5</entry><entry>uint8_t</entry><entry>flags</entry>
	      <entry>See GPS Flags table below</entry>
	    </row>
	    <row>
	      <entry>6</entry><entry>int16_t</entry><entry>altitude</entry><entry>m</entry>
	    </row>
	    <row>
	      <entry>8</entry><entry>int32_t</entry><entry>latitude</entry><entry>degrees * 10<superscript>7</superscript></entry>
	    </row>
	    <row>
	      <entry>12</entry><entry>int32_t</entry><entry>longitude</entry><entry>degrees * 10<superscript>7</superscript></entry>
	    </row>
	    <row>
	      <entry>16</entry><entry>uint8_t</entry><entry>year</entry>
	    </row>
	    <row>
	      <entry>17</entry><entry>uint8_t</entry><entry>month</entry>
	    </row>
	    <row>
	      <entry>18</entry><entry>uint8_t</entry><entry>day</entry>
	    </row>
	    <row>
	      <entry>19</entry><entry>uint8_t</entry><entry>hour</entry>
	    </row>
	    <row>
	      <entry>20</entry><entry>uint8_t</entry><entry>minute</entry>
	    </row>
	    <row>
	      <entry>21</entry><entry>uint8_t</entry><entry>second</entry>
	    </row>
	    <row>
	      <entry>22</entry><entry>uint8_t</entry><entry>pdop</entry><entry>* 5</entry>
	    </row>
	    <row>
	      <entry>23</entry><entry>uint8_t</entry><entry>hdop</entry><entry>* 5</entry>
	    </row>
	    <row>
	      <entry>24</entry><entry>uint8_t</entry><entry>vdop</entry><entry>* 5</entry>
	    </row>
	    <row>
	      <entry>25</entry><entry>uint8_t</entry><entry>mode</entry>
	      <entry>See GPS Mode table below</entry>
	    </row>
	    <row>
	      <entry>26</entry><entry>uint16_t</entry><entry>ground_speed</entry><entry>cm/s</entry>
	    </row>
	    <row>
	      <entry>28</entry><entry>int16_t</entry><entry>climb_rate</entry><entry>cm/s</entry>
	    </row>
	    <row>
	      <entry>30</entry><entry>uint8_t</entry><entry>course</entry><entry>/ 2</entry>
	    </row>
	    <row>
	      <entry>31</entry><entry>uint8_t</entry><entry>unused[1]</entry>
	    </row>
	    <row>
	      <entry>32</entry>
	    </row>
	  </tbody>
	</tgroup>
      </table>
      <para>
	Packed into a one byte field are status flags and the count of
	satellites used to compute the position fix. Note that this
	number may be lower than the number of satellites being
	tracked; the receiver will not use information from satellites
	with weak signals or which are close enough to the horizon to
	have significantly degraded position accuracy.
      </para>
      <table frame='all'>
	<title>GPS Flags</title>
	<tgroup cols='3' colsep='1' rowsep='1'>
	  <colspec align='center' colwidth='*' colname='bits'/>
	  <colspec align='left' colwidth='2*' colname='name'/>
	  <colspec align='left' colwidth='7*' colname='description'/>
	  <thead>
	    <row>
	      <entry align='center'>Bits</entry>
	      <entry align='center'>Name</entry>
	      <entry align='center'>Description</entry>
	    </row>
	  </thead>
	  <tbody>
	    <row>
	      <entry>0-3</entry>
	      <entry>nsats</entry>
	      <entry>Number of satellites in solution</entry>
	    </row>
	    <row>
	      <entry>4</entry>
	      <entry>valid</entry>
	      <entry>GPS solution is valid</entry>
	    </row>
	    <row>
	      <entry>5</entry>
	      <entry>running</entry>
	      <entry>GPS receiver is operational</entry>
	    </row>
	    <row>
	      <entry>6</entry>
	      <entry>date_valid</entry>
	      <entry>Reported date is valid</entry>
	    </row>
	    <row>
	      <entry>7</entry>
	      <entry>course_valid</entry>
	      <entry>ground speed, course and climb rates are valid</entry>
	    </row>
	  </tbody>
	</tgroup>
      </table>
      <para>
	Here are all of the valid GPS operational modes. Altus Metrum
	products will only ever report 'N' (not valid), 'A'
	(Autonomous) modes or 'E' (Estimated). The remaining modes
	are either testing modes or require additional data.
      </para>
      <table frame='all'>
	<title>GPS Mode</title>
	<tgroup cols='3' colsep='1' rowsep='1'>
	  <colspec align='center' colwidth='*' colname='value'/>
	  <colspec align='center' colwidth='3*' colname='name'/>
	  <colspec align='left' colwidth='7*' colname='description'/>
	  <thead>
	    <row>
	      <entry align='center'>Mode</entry>
	      <entry align='center'>Name</entry>
	      <entry align='center'>Decsription</entry>
	    </row>
	  </thead>
	  <tbody>
	    <row>
	      <entry>N</entry>
	      <entry>Not Valid</entry>
	      <entry>All data are invalid</entry>
	    </row>
	    <row>
	      <entry>A</entry>
	      <entry>Autonomous mode</entry>
	      <entry>Data are derived from satellite data</entry>
	    </row>
	    <row>
	      <entry>D</entry>
	      <entry>Differential Mode</entry>
	      <entry>
		  Data are augmented with differential data from a
		  known ground station. The SkyTraq unit in TeleMetrum
		  does not support this mode
		</entry>
	    </row>
	    <row>
	      <entry>E</entry>
	      <entry>Estimated</entry>
	      <entry>
		  Data are estimated using dead reckoning from the
		  last known data
		</entry>
	    </row>
	    <row>
	      <entry>M</entry>
	      <entry>Manual</entry>
	      <entry>Data were entered manually</entry>
	    </row>
	    <row>
	      <entry>S</entry>
	      <entry>Simulated</entry>
	      <entry>GPS receiver testing mode</entry>
	    </row>
	  </tbody>
	</tgroup>
      </table>
    </section>
    <section>
      <title>GPS Satellite Data</title>
      <informaltable frame='none' label='' tocentry='0'>
	<tgroup cols='2' align='center' colsep='1' rowsep='1'>
	  <colspec align='center' colwidth='*' colname='Offset'/>
	  <colspec align='left' colwidth='3*' colname='Description'/>
	  <thead>
	    <row>
	      <entry>Type</entry>
	      <entry>Description</entry>
	    </row>
	  </thead>
	  <tbody>
	    <row>
	      <entry>0x06</entry>
	      <entry>GPS Satellite Data</entry>
	    </row>
	  </tbody>
	</tgroup>
      </informaltable>
      <para>
	This packet provides space vehicle identifiers and signal
	quality information in the form of a C/N1 number for up to 12
	satellites. The order of the svids is not specified.
      </para>
      <para>
	GPS Satellite data are transmitted once per second during all
	phases of the flight.
      </para>
      <table frame='all'>
	<title>GPS Satellite Data Contents</title>
	<tgroup cols='4' align='center' colsep='1' rowsep='1'>
	  <colspec align='right' colwidth='*' colname='Offset'/>
	  <colspec align='center' colwidth='3*' colname='Data Type'/>
	  <colspec align='left' colwidth='3*' colname='Name'/>
	  <colspec align='left' colwidth='9*' colname='Description'/>
	  <thead>
	    <row>
	      <entry align='center'>Offset</entry>
	      <entry align='center'>Data Type</entry>
	      <entry align='center'>Name</entry>
	      <entry align='center'>Description</entry>
	    </row>
	  </thead>
	  <tbody>
	    <row>
	      <entry>5</entry><entry>uint8_t</entry><entry>channels</entry>
	      <entry>Number of reported satellite information</entry>
	    </row>
	    <row>
	      <entry>6</entry><entry>sat_info_t</entry><entry>sats[12]</entry>
	      <entry>See Per-Satellite data table below</entry>
	    </row>
	    <row>
	      <entry>30</entry><entry>uint8_t</entry><entry>unused[2]</entry>
	    </row>
	    <row>
	      <entry>32</entry>
	    </row>
	  </tbody>
	</tgroup>
      </table>
      <table frame='all'>
	<title>GPS Per-Satellite data (sat_info_t)</title>
	<tgroup cols='4' align='center' colsep='1' rowsep='1'>
	  <colspec align='right' colwidth='*' colname='Offset'/>
	  <colspec align='center' colwidth='3*' colname='Data Type'/>
	  <colspec align='left' colwidth='3*' colname='Name'/>
	  <colspec align='left' colwidth='9*' colname='Description'/>
	  <thead>
	    <row>
	      <entry align='center'>Offset</entry>
	      <entry align='center'>Data Type</entry>
	      <entry align='center'>Name</entry>
	      <entry align='center'>Description</entry>
	    </row>
	  </thead>
	  <tbody>
	    <row>
	      <entry>0</entry><entry>uint8_t</entry><entry>svid</entry>
	      <entry>Space Vehicle Identifier</entry>
	    </row>
	    <row>
	      <entry>1</entry><entry>uint8_t</entry><entry>c_n_1</entry>
	      <entry>C/N1 signal quality indicator</entry>
	    </row>
	    <row>
	      <entry>2</entry>
	    </row>
	  </tbody>
	</tgroup>
      </table>
    </section>
    <section>
      <title>Companion Data Data</title>
      <informaltable frame='none' label='' tocentry='0'>
	<tgroup cols='2' align='center' colsep='1' rowsep='1'>
	  <colspec align='center' colwidth='*' colname='Offset'/>
	  <colspec align='left' colwidth='3*' colname='Description'/>
	  <thead>
	    <row>
	      <entry>Type</entry>
	      <entry>Description</entry>
	    </row>
	  </thead>
	  <tbody>
	    <row>
	      <entry>0x07</entry>
	      <entry>Companion Data Data</entry>
	    </row>
	  </tbody>
	</tgroup>
      </informaltable>
      <para>
	When a companion board is attached to TeleMega or TeleMetrum,
	it can provide telemetry data to be included in the
	downlink. The companion board can provide up to 12 16-bit data
	values.
      </para>
      <para>
	The companion board itself specifies the transmission rate. On
	the ground and during descent, that rate is limited to one
	packet per second. During ascent, that rate is limited to 10
	packets per second.
      </para>
      <table frame='all'>
	<title>Companion Data Contents</title>
	<tgroup cols='4' align='center' colsep='1' rowsep='1'>
	  <colspec align='right' colwidth='*' colname='Offset'/>
	  <colspec align='center' colwidth='3*' colname='Data Type'/>
	  <colspec align='left' colwidth='3*' colname='Name'/>
	  <colspec align='left' colwidth='9*' colname='Description'/>
	  <thead>
	    <row>
	      <entry align='center'>Offset</entry>
	      <entry align='center'>Data Type</entry>
	      <entry align='center'>Name</entry>
	      <entry align='center'>Description</entry>
	    </row>
	  </thead>
	  <tbody>
	    <row>
	      <entry>5</entry><entry>uint8_t</entry><entry>board_id</entry>
	      <entry>Type of companion board attached</entry>
	    </row>
	    <row>
	      <entry>6</entry><entry>uint8_t</entry><entry>update_period</entry>
	      <entry>How often telemetry is sent, in 1/100ths of a second</entry>
	    </row>
	    <row>
	      <entry>7</entry><entry>uint8_t</entry><entry>channels</entry>
	      <entry>Number of data channels supplied</entry>
	    </row>
	    <row>
	      <entry>8</entry><entry>uint16_t[12]</entry><entry>companion_data</entry>
	      <entry>Up to 12 channels of 16-bit companion data</entry>
	    </row>
	    <row>
	      <entry>32</entry>
	    </row>
	  </tbody>
	</tgroup>
      </table>
    </section>
  </section>
  <section>
    <title>Data Transmission</title>
    <para>
      Altus Metrum devices use Texas Instruments sub-GHz digital radio
      products. Ground stations use parts with HW FEC while some
      flight computers perform FEC in software. TeleGPS is
      transmit-only.
    </para>
    <table>
      <title>Altus Metrum Radio Parts</title>
      <tgroup cols='3'>
	<colspec align="center" colwidth="*" colname="Part Number"/>
	<colspec align="center" colwidth="*" colname="Description"/>
	<colspec align="left" colwidth="*" colname="Used in"/>
	<thead>
	  <row>
	    <entry align="center">Part Number</entry>
	    <entry align="center">Description</entry>
	    <entry align="center">Used in</entry>
	  </row>
	</thead>
	<tbody>
	  <row>
	    <entry>CC1111</entry><entry>10mW transceiver with integrated SoC</entry>
	    <entry>TeleDongle v0.2, TeleBT v1.0, TeleMetrum v1.x, TeleMini</entry>
	  </row>
	  <row>
	    <entry>CC1120</entry><entry>35mW transceiver with SW FEC</entry>
	    <entry>TeleMetrum v2, TeleMega</entry>
	  </row>
	  <row>
	    <entry>CC1200</entry><entry>35mW transceiver with HW FEC</entry>
	    <entry>TeleDongle v3.0, TeleBT v3.0</entry>
	  </row>
	  <row>
	    <entry>CC115L</entry><entry>14mW transmitter with SW FEC</entry>
	    <entry>TeleGPS</entry>
	  </row>
	</tbody>
      </tgroup>
    </table>
    <section>
      <title>Modulation Scheme</title>
      <para>
	Texas Instruments provides a tool for computing modulation
	parameters given a desired modulation format and basic bit
	rate.

	While we might like to use something with better low-signal
	performance like BPSK, the radios we use don't support that,
	but do support Gaussian frequency shift keying (GFSK). Regular
	frequency shift keying (FSK) encodes the signal by switching
	the carrier between two frequencies. The Gaussian version is
	essentially the same, but the shift between frequencies gently
	follows a gaussian curve, rather than switching
	immediately. This tames the bandwidth of the signal without
	affecting the ability to transmit data.

	For AltOS, there are three available bit rates, 38.4kBaud,
	9.6kBaud and 2.4kBaud resulting in the following signal
	parmeters:

      </para>
      <table>
	<title>Modulation Scheme</title>
	<tgroup cols='3'>
	  <colspec align="center" colwidth="*" colname="rate"/>
	  <colspec align="center" colwidth="*" colname="deviation"/>
	  <colspec align="center" colwidth="*" colname="bandwidth"/>
	  <thead>
	    <row>
	      <entry align='center'>Rate</entry>
	      <entry align='center'>Deviation</entry>
	      <entry align='center'>Receiver Bandwidth</entry>
	    </row>
	  </thead>
	  <tbody>
	    <row>
	      <entry>38.4kBaud</entry>
	      <entry>20.5kHz</entry>
	      <entry>100kHz</entry>
	    </row>
	    <row>
	      <entry>9.6kBaud</entry>
	      <entry>5.125kHz</entry>
	      <entry>25kHz</entry>
	    </row>
	    <row>
	      <entry>2.4kBaud</entry>
	      <entry>1.5kHz</entry>
	      <entry>5kHz</entry>
	    </row>
	  </tbody>
	</tgroup>
      </table>
    </section>
    <section>
      <title>Error Correction</title>
      <para>
	The cc1111 and cc1200 provide forward error correction in
	hardware; on the cc1120 and cc115l that's done in
	software. AltOS uses this to improve reception of weak
	signals. As it's a rate 1/2 encoding, each bit of data takes
	two bits when transmitted, so the effective data rate is half
	of the raw transmitted bit rate.
      </para>
      <table>
	<title>Error Correction</title>
	<tgroup cols='3'>
	  <colspec align="center" colwidth="*" colname="parameter"/>
	  <colspec align="center" colwidth="*" colname="value"/>
	  <colspec align="center" colwidth="*" colname="description"/>
	  <thead>
	    <row>
	      <entry align='center'>Parameter</entry>
	      <entry align='center'>Value</entry>
	      <entry align='center'>Description</entry>
	    </row>
	  </thead>
	  <tbody>
	    <row>
	      <entry>Error Correction</entry>
	      <entry>Convolutional coding</entry>
	      <entry>1/2 rate, constraint length m=4</entry>
	    </row>
	    <row>
	      <entry>Interleaving</entry>
	      <entry>4 x 4</entry>
	      <entry>Reduce effect of noise burst</entry>
	    </row>
	    <row>
	      <entry>Data Whitening</entry>
	      <entry>XOR with 9-bit PNR</entry>
	      <entry>Rotate right with bit 8 = bit 0 xor bit 5, initial
	      value 111111111</entry>
	    </row>
	  </tbody>
	</tgroup>
      </table>
    </section>
  </section>
  <section>
    <title>TeleDongle packet format</title>
    <para>
      TeleDongle does not do any interpretation of the packet data,
      instead it is configured to receive packets of a specified
      length (32 bytes in this case). For each received packet,
      TeleDongle produces a single line of text. This line starts with
      the string "TELEM " and is followed by a list of hexadecimal
      encoded bytes.
    </para>
    <programlisting>TELEM 224f01080b05765e00701f1a1bbeb8d7b60b070605140c000600000000000000003fa988</programlisting>
    <para>
      The hexadecimal encoded string of bytes contains a length byte,
      the packet data, two bytes added by the cc1111 radio receiver
      hardware and finally a checksum so that the host software can
      validate that the line was transmitted without any errors.
    </para>
    <table>
      <title>Packet Format</title>
      <tgroup cols='4'>
	<colspec align="center" colwidth="2*" colname="offset"/>
	<colspec align="center" colwidth="*" colname="name"/>
	<colspec align="center" colwidth="*" colname="value"/>
	<colspec align="center" colwidth="5*" colname="description"/>
	<thead>
	  <row>
	    <entry align='center'>Offset</entry>
	    <entry align='center'>Name</entry>
	    <entry align='center'>Example</entry>
	    <entry align='center'>Description</entry>
	  </row>
	</thead>
	<tbody>
	  <row>
	    <entry>0</entry>
	    <entry>length</entry>
	    <entry>22</entry>
	    <entry>Total length of data bytes in the line. Note that
	    this includes the added RSSI and status bytes</entry>
	  </row>
	  <row>
	    <entry>1 ·· length-3</entry>
	    <entry>packet</entry>
	    <entry>4f ·· 00</entry>
	    <entry>Bytes of actual packet data</entry>
	  </row>
	  <row>
	    <entry>length-2</entry>
	    <entry>rssi</entry>
	    <entry>3f</entry>
	    <entry>Received signal strength. dBm = rssi / 2 - 74</entry>
	  </row>
	  <row>
	    <entry>length-1</entry>
	    <entry>lqi</entry>
	    <entry>a9</entry>
	    <entry>Link Quality Indicator and CRC status. Bit 7
	    is set when the CRC is correct</entry>
	  </row>
	  <row>
	    <entry>length</entry>
	    <entry>checksum</entry>
	    <entry>88</entry>
	    <entry>(0x5a + sum(bytes 1 ·· length-1)) % 256</entry>
	  </row>
	</tbody>
      </tgroup>
    </table>
  </section>
  <section>
    <title>History and Motivation</title>
    <para>
      The original AltoOS telemetry mechanism encoded everything
      available piece of information on the TeleMetrum hardware into a
      single unified packet. Initially, the packets contained very
      little data—some raw sensor readings along with the current GPS
      coordinates when a GPS receiver was connected. Over time, the
      amount of data grew to include sensor calibration data, GPS
      satellite information and a host of internal state information
      designed to help diagnose flight failures in case of a loss of
      the on-board flight data.
    </para>
    <para>
      Because every packet contained all of the data, packets were
      huge—95 bytes long. Much of the information was also specific to
      the TeleMetrum hardware. With the introduction of the TeleMini
      flight computer, most of the data contained in the telemetry
      packets was unavailable. Initially, a shorter, but still
      comprehensive packet was implemented. This required that the
      ground station be pre-configured as to which kind of packet to
      expect.
    </para>
    <para>
      The development of several companion boards also made the
      shortcomings evident—each companion board would want to include
      telemetry data in the radio link; with the original design, the
      packet would have to hold the new data as well, requiring
      additional TeleMetrum and ground station changes.
    </para>
  </section>
</article>
