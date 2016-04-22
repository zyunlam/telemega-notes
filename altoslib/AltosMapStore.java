/*
 * Copyright © 2014 Keith Packard <keithp@keithp.com>
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
import java.net.*;
import java.util.*;

public class AltosMapStore {
	String					url;
	public File				file;
	LinkedList<AltosMapStoreListener>	listeners = new LinkedList<AltosMapStoreListener>();

	int					status;

	public int status() {
		return status;
	}

	public synchronized void add_listener(AltosMapStoreListener listener) {
		if (!listeners.contains(listener))
			listeners.add(listener);
	}

	public synchronized void remove_listener(AltosMapStoreListener listener) {
		listeners.remove(listener);
	}

	private synchronized void notify_listeners(int status) {
		this.status = status;
		for (AltosMapStoreListener listener : listeners)
			listener.notify_store(this, status);
	}

	static Object	forbidden_lock = new Object();
	static long	forbidden_time;
	static boolean	forbidden_set;

	private int fetch_url() {
		URL u;

		try {
			u = new URL(url);
		} catch (java.net.MalformedURLException e) {
			return AltosMapTile.bad_request;
		}

		byte[] data;
		URLConnection uc = null;
		try {
			uc = u.openConnection();
			String type = uc.getContentType();
			int contentLength = uc.getContentLength();
			if (uc instanceof HttpURLConnection) {
				int response = ((HttpURLConnection) uc).getResponseCode();
				switch (response) {
				case HttpURLConnection.HTTP_FORBIDDEN:
				case HttpURLConnection.HTTP_PAYMENT_REQUIRED:
				case HttpURLConnection.HTTP_UNAUTHORIZED:
					synchronized (forbidden_lock) {
						forbidden_time = System.nanoTime();
						forbidden_set = true;
						return AltosMapTile.forbidden;
					}
				}
			}
			InputStream in = new BufferedInputStream(uc.getInputStream());
			int bytesRead = 0;
			int offset = 0;
			data = new byte[contentLength];
			while (offset < contentLength) {
				bytesRead = in.read(data, offset, data.length - offset);
				if (bytesRead == -1)
					break;
				offset += bytesRead;
			}
			in.close();

			if (offset != contentLength)
				return AltosMapTile.failed;

		} catch (IOException e) {
			return AltosMapTile.failed;
		}

		try {
			FileOutputStream out = new FileOutputStream(file);
			out.write(data);
			out.flush();
			out.close();
		} catch (FileNotFoundException e) {
			return AltosMapTile.bad_request;
		} catch (IOException e) {
			if (file.exists())
				file.delete();
			return AltosMapTile.bad_request;
		}
		return AltosMapTile.success;
	}

	static Object	fetch_lock = new Object();

	static final long	forbidden_interval = 60l * 1000l * 1000l * 1000l;
	static final long 	google_maps_ratelimit_ms = 1200;

	static Object	loader_lock = new Object();

	static LinkedList<AltosMapStore> waiting = new LinkedList<AltosMapStore>();
	static LinkedList<AltosMapStore> running = new LinkedList<AltosMapStore>();

	static final int concurrent_loaders = 128;

	static void start_loaders() {
		while (!waiting.isEmpty() && running.size() < concurrent_loaders) {
			AltosMapStore 	s = waiting.remove();
			running.add(s);
			Thread lt = s.make_loader_thread();
			lt.start();
		}
	}

	void finish_loader() {
		synchronized(loader_lock) {
			running.remove(this);
			start_loaders();
		}
	}

	void add_loader() {
		synchronized(loader_lock) {
			waiting.add(this);
			start_loaders();
		}
	}

	class loader implements Runnable {

		public void run() {
			try {
				if (file.exists()) {
					notify_listeners(AltosMapTile.success);
					return;
				}

				synchronized(forbidden_lock) {
					if (forbidden_set && (System.nanoTime() - forbidden_time) < forbidden_interval) {
						notify_listeners(AltosMapTile.forbidden);
						return;
					}
				}

				int new_status;

				if (!AltosVersion.has_google_maps_api_key()) {
					synchronized (fetch_lock) {
						long startTime = System.nanoTime();
						new_status = fetch_url();
						if (new_status == AltosMapTile.success) {
							long duration_ms = (System.nanoTime() - startTime) / 1000000;
							if (duration_ms < google_maps_ratelimit_ms) {
								try {
									Thread.sleep(google_maps_ratelimit_ms - duration_ms);
								} catch (InterruptedException e) {
									Thread.currentThread().interrupt();
								}
							}
						}
					}
				} else {
					new_status = fetch_url();
				}
				notify_listeners(new_status);
			} finally {
				finish_loader();
			}
		}
	}

	private Thread make_loader_thread() {
		return new Thread(new loader());
	}

	private void load() {
		add_loader();
	}

	private AltosMapStore (String url, File file) {
		this.url = url;
		this.file = file;

		if (file.exists())
			status = AltosMapTile.success;
		else {
			status = AltosMapTile.loading;
			load();
		}
	}

	public int hashCode() {
		return url.hashCode();
	}

	public boolean equals(Object o) {
		if (o == null)
			return false;

		if (!(o instanceof AltosMapStore))
			return false;

		AltosMapStore other = (AltosMapStore) o;
		return url.equals(other.url);
	}

	static HashMap<String,AltosMapStore> stores = new HashMap<String,AltosMapStore>();

	public static AltosMapStore get(String url, File file) {
		AltosMapStore	store;
		synchronized(stores) {
			if (stores.containsKey(url)) {
				store = stores.get(url);
			} else {
				store = new AltosMapStore(url, file);
				stores.put(url, store);
			}
		}
		return store;
	}
}
