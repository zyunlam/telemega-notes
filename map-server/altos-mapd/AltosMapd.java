/*
 * Copyright © 2018 Keith Packard <keithp@keithp.com>
 *
 * This program is free software; you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation, either version 2 of the License, or
 * (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful, but
 * WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
 * General Public License for more details.
 */

package altosmapd;

import java.net.*;
import java.io.*;

import org.altusmetrum.altoslib_13.*;

public class AltosMapd {

	public final static int port = 16717;

	public final static int maptype = AltosMap.maptype_hybrid;

	public final static int px_size = 512;

	public final static int scale = 1;

	public static void main(final String[] args) {

		AltosMapdServer server = new AltosMapdServer(port);

		AltosPreferences.init(new AltosMapdPreferences());

		AltosPreferences.mapdir = new File("/home/keithp/misc/rockets/flights/maps");

		for (;;) {
			Socket client = server.accept();
			if (client == null) {
				System.out.printf("accept failed\n");
				continue;
			}
			System.out.printf("got client\n");
			new AltosMapdClient(client);
		}
	}
}
