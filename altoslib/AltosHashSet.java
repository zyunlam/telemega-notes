/*
 * Copyright © 2016 Keith Packard <keithp@keithp.com>
 *
 * This program is free software; you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation; version 2 of the License.
 *
 * This program is distributed in the hope that it will be useful, but
 * WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
 * General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License along
 * with this program; if not, write to the Free Software Foundation, Inc.,
 * 59 Temple Place, Suite 330, Boston, MA 02111-1307 USA.
 */

package org.altusmetrum.altoslib_11;

import java.io.*;
import java.util.*;
import java.text.*;

public class AltosHashSet extends Hashtable<String,String> {
	private StringWriter	writer;

	static private int get(StringReader reader) throws IOException {
		return reader.read();
	}

	static private String get_token(StringReader reader) throws IOException {
		int	c = get(reader);

		if (c == -1)
			return null;

		ArrayList<Integer>	chars = new ArrayList<Integer>();

		for (;;) {
			chars.add(c);
			c = get(reader);
			if (c == -1 || c == ';')
				break;
			if (c == '\\')
				c = get(reader);
		}
		int[] ch = new int[chars.size()];
		for (int i = 0; i < ch.length; i++)
			ch[i] = chars.get(i);
		return new String(ch, 0, ch.length);
	}

	static private void put(StringWriter writer, int c) throws IOException {
		writer.write(c);
	}

	static private void put_token(StringWriter writer, String token) throws IOException {
		for (int i = 0; i < token.length(); i++) {
			int c = token.codePointAt(i);

			switch (c) {
			case ';':
			case '\\':
				put(writer, '\\');
			}
			put(writer, c);
		}
		put(writer, ';');
	}

	public String toString() {
		try {
			StringWriter writer = new StringWriter();

			for (String key : keySet()) {
				String value = get(key);
				put_token(writer, key);
				put_token(writer, value);
			}
			return writer.toString();
		} catch (IOException ie) {
			return null;
		}
	}

	public void putInt(String key, int value) {
		put(key, Integer.toString(value));
	}

	public int getInt(String key, int def) {
		String	value = get(key);

		if (value == null)
			return def;
		try {
			return AltosParse.parse_int(value);
		} catch (ParseException pe) {
			return def;
		}
	}

	public void putDouble(String key, double value) {
		put(key, AltosParse.format_double_net(value));
	}

	public double getDouble(String key, double def) {
		String	value = get(key);

		if (value == null)
			return def;
		try {
			return AltosParse.parse_double_net(value);
		} catch (ParseException pe) {
			return def;
		}
	}

	public String getString(String key, String def) {
		String	value = get(key);

		if (value == null)
			return def;
		return value;
	}

	public void putString(String key, String value) {
		put(key, value);
	}

	public AltosHashSet (String string) throws IOException {
		StringReader reader = new StringReader(string);
		String	key, value;

		for (;;) {
			key = get_token(reader);
			value = get_token(reader);
			if (key == null || value == null)
				break;
			put(key, value);
		}
	}

	public AltosHashSet() {
	}

	static public AltosHashSet[] array(String string) throws IOException {

		if (string == null)
			return null;

		StringReader 		reader = new StringReader(string);
		ArrayList<AltosHashSet>	array = new ArrayList<AltosHashSet>();
		String			element;

		while ((element = get_token(reader)) != null)
			array.add(new AltosHashSet(element));
		return array.toArray(new AltosHashSet[0]);
	}

	static public String toString(AltosHashSet[] sets) throws IOException {

		if (sets == null)
			return null;

		StringWriter		writer = new StringWriter();

		for (AltosHashSet h : sets) {
			String		element = h.toString();
			put_token(writer, element);
		}
		return writer.toString();
	}
}
