/*
 * Copyright © 2010 Mike Beattie <mike@ethernal.org>
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

package org.altusmetrum.altoslib_10;

import java.io.*;
import java.util.*;
import java.text.*;

public abstract class AltosPreferencesBackend {

	public abstract String  getString(String key, String def);
	public abstract void    putString(String key, String value);

	public abstract int     getInt(String key, int def);
	public abstract void    putInt(String key, int value);

	public abstract double  getDouble(String key, double def);
	public abstract void    putDouble(String key, double value);

	public abstract boolean getBoolean(String key, boolean def);
	public abstract void    putBoolean(String key, boolean value);

	public abstract byte[]  getBytes(String key, byte[] def);
	public abstract void    putBytes(String key, byte[] value);

	public Serializable getSerializable(String key, Serializable def) {
		byte[] bytes = null;

		bytes = getBytes(key, null);
		if (bytes == null)
			return def;

		ByteArrayInputStream bais = new ByteArrayInputStream(bytes);

		try {
			ObjectInputStream ois = new ObjectInputStream(bais);
			Serializable object = (Serializable) ois.readObject();
			return object;
		} catch (IOException ie) {
			debug("IO exception %s\n", ie.toString());
		} catch (ClassNotFoundException ce) {
			debug("ClassNotFoundException %s\n", ce.toString());
		}
		return def;
	}

	public void putSerializable(String key, Serializable object) {
		ByteArrayOutputStream baos = new ByteArrayOutputStream();

		try {
			ObjectOutputStream oos = new ObjectOutputStream(baos);

			oos.writeObject(object);
			byte[] bytes = baos.toByteArray();

			putBytes(key, bytes);
		} catch (IOException ie) {
			debug("set_state failed %s\n", ie.toString());
		}
	}

	public abstract boolean nodeExists(String key);
	public abstract AltosPreferencesBackend node(String key);

	public abstract String[] keys();
	public abstract void    remove(String key);

	public abstract void    flush();

	public abstract File homeDirectory();

	public abstract void debug(String format, Object ... arguments);
}
