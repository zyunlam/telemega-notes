/*
 * Copyright © 2011 Keith Packard <keithp@keithp.com>
 * Copyright © 2012 Mike Beattie <mike@ethernal.org>
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

package org.altusmetrum.AltosDroid;

import android.speech.tts.TextToSpeech;
import android.speech.tts.TextToSpeech.OnInitListener;

import org.altusmetrum.altoslib_5.*;

public class AltosVoice {

	private TextToSpeech tts         = null;
	private boolean      tts_enabled = false;

	private IdleThread   idle_thread = null;

	private AltosState   old_state   = null;

	public AltosVoice(AltosDroid a) {

		tts = new TextToSpeech(a, new OnInitListener() {
			public void onInit(int status) {
				if (status == TextToSpeech.SUCCESS) tts_enabled = true;
				if (tts_enabled) {
					idle_thread = new IdleThread();
				}
			}
		});

	}

	public synchronized void speak(String s) {
		if (!tts_enabled) return;
		tts.speak(s, TextToSpeech.QUEUE_ADD, null);
	}

	public void stop() {
		if (tts != null) tts.shutdown();
		if (idle_thread != null) {
			idle_thread.interrupt();
			idle_thread = null;
		}
	}

	public void tell(AltosState state, AltosGreatCircle from_receiver) {
		if (!tts_enabled) return;

		boolean	spoke = false;
		if (old_state == null || old_state.state != state.state) {
			if (state.state != AltosLib.ao_flight_stateless)
				speak(state.state_name());
			if ((old_state == null || old_state.state <= AltosLib.ao_flight_boost) &&
			    state.state > AltosLib.ao_flight_boost) {
				if (state.max_speed() != AltosLib.MISSING)
					speak(String.format("Max speed: %s.",
							    AltosConvert.speed.say_units(state.max_speed())));
				spoke = true;
			} else if ((old_state == null || old_state.state < AltosLib.ao_flight_drogue) &&
			           state.state >= AltosLib.ao_flight_drogue) {
				if (state.max_height() != AltosLib.MISSING)
					speak(String.format("Max height: %s.",
							    AltosConvert.height.say_units(state.max_height())));
				spoke = true;
			}
		}
		if (old_state == null || old_state.gps_ready != state.gps_ready) {
			if (state.gps_ready) {
				speak("GPS ready");
				spoke = true;
			} else if (old_state != null) {
				speak("GPS lost");
				spoke = true;
			}
		}
		old_state = state;
		if (idle_thread != null)
			idle_thread.notice(state, from_receiver, spoke);
	}


	class IdleThread extends Thread {
		boolean	           started;
		private AltosState state;
		private AltosGreatCircle from_receiver;
		int                reported_landing;
		int                report_interval;
		long               report_time;

		public synchronized void report(boolean last) {
			if (state == null)
				return;

			/* reset the landing count once we hear about a new flight */
			if (state.state < AltosLib.ao_flight_drogue)
				reported_landing = 0;

			/* Shut up once the rocket is on the ground */
			if (reported_landing > 2) {
				return;
			}

			/* If the rocket isn't on the pad, then report location */
			if ((AltosLib.ao_flight_drogue <= state.state &&
			      state.state < AltosLib.ao_flight_landed) ||
			     state.state == AltosLib.ao_flight_stateless)
			{
				AltosGreatCircle	position;

				if (from_receiver != null)
					position = from_receiver;
				else
					position = state.from_pad;

				if (position != null) {
					speak(String.format("Height %s, bearing %s %d, elevation %d, range %s.\n",
							    AltosConvert.height.say_units(state.height()),
							    position.bearing_words(
								    AltosGreatCircle.BEARING_VOICE),
							    (int) (position.bearing + 0.5),
							    (int) (position.elevation + 0.5),
							    AltosConvert.distance.say_units(position.range)));
				}
			} else if (state.state > AltosLib.ao_flight_pad) {
				if (state.height() != AltosLib.MISSING)
					speak(AltosConvert.height.say_units(state.height()));
			} else {
				reported_landing = 0;
			}

			/* If the rocket is coming down, check to see if it has landed;
			 * either we've got a landed report or we haven't heard from it in
			 * a long time
			 */
			if (state.state >= AltosLib.ao_flight_drogue &&
			    (last ||
			     System.currentTimeMillis() - state.received_time >= 15000 ||
			     state.state == AltosLib.ao_flight_landed))
			{
				if (Math.abs(state.speed()) < 20 && state.height() < 100)
					speak("rocket landed safely");
				else
					speak("rocket may have crashed");
				if (state.from_pad != null)
					speak(String.format("Bearing %d degrees, range %s.",
					                    (int) (state.from_pad.bearing + 0.5),
							    AltosConvert.distance.say_units(state.from_pad.distance)));
				++reported_landing;
			}
		}

		long now () {
			return System.currentTimeMillis();
		}

		void set_report_time() {
			report_time = now() + report_interval;
		}

		public void run () {
			try {
				for (;;) {
					set_report_time();
					for (;;) {
						synchronized (this) {
							long sleep_time = report_time - now();
							if (sleep_time <= 0)
								break;
							wait(sleep_time);
						}
					}
					report(false);
				}
			} catch (InterruptedException ie) {
			}
		}

		public synchronized void notice(AltosState new_state, AltosGreatCircle new_from_receiver, boolean spoken) {
			AltosState old_state = state;
			state = new_state;
			from_receiver = new_from_receiver;
			if (!started && state.state > AltosLib.ao_flight_pad) {
				started = true;
				start();
			}

			if (state.state < AltosLib.ao_flight_drogue)
				report_interval = 10000;
			else
				report_interval = 20000;
			if (old_state != null && old_state.state != state.state) {
				report_time = now();
				this.notify();
			} else if (spoken)
				set_report_time();
		}

		public IdleThread() {
			state = null;
			reported_landing = 0;
			report_interval = 10000;
		}
	}

}
