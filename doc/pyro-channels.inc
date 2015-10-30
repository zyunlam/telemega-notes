
Acceleration:: Select a value, and then choose
whether acceleration should be above or below
that value. Acceleration is positive upwards,
so accelerating towards the ground would
produce negative numbers. Acceleration during
descent is noisy and inaccurate, so be careful
when using it during these phases of the
flight.

Vertical speed:: Select a value, and then
choose whether vertical speed should be above
or below that value. Speed is positive
upwards, so moving towards the ground would
produce negative numbers. Speed during descent
is a bit noisy and so be careful when using it
during these phases of the flight.

Height:: Select a value, and then choose
whether the height above the launch pad should
be above or below that value.

Orientation:: TeleMega and EasyMega contain a
3-axis gyroscope and accelerometer which is
used to measure the current angle. Note that
this angle is not the change in angle from the
launch pad, but rather absolute relative to
gravity; the 3-axis accelerometer is used to
compute the angle of the rocket on the launch
pad and initialize the system.

  [NOTE]
  ====
  Because this value is computed by integrating
  rate gyros, it gets progressively less
  accurate as the flight goes on. It should have
  an accumulated error of less than 0.2°/second
  (after 10 seconds of flight, the error should
  be less than 2°).

  The usual use of the orientation configuration
  is to ensure that the rocket is traveling
  mostly upwards when deciding whether to ignite
  air starts or additional stages. For that,
  choose a reasonable maximum angle (like 20°)
  and set the motor igniter to require an angle
  of less than that value.
  ====

Flight Time:: Time since boost was detected. Select a value and choose
whether to activate the pyro channel before or after that amount of
time.

Ascending:: A simple test saying whether the rocket is going up or
not. This is exactly equivalent to testing whether the speed is > 0.

Descending:: A simple test saying whether the rocket is going down or
not. This is exactly equivalent to testing whether the speed is < 0.

After Motor:: The flight software counts each time the rocket starts
accelerating and then decelerating (presumably due to a motor or
motors burning). Use this value for multi-staged or multi-airstart
launches.

Delay:: This value doesn't perform any checks, instead it inserts a
delay between the time when the other parameters become true and when
the pyro channel is activated.

Flight State:: The flight software tracks the flight
through a sequence of states:

 * Boost. The motor has lit and the rocket is
   accelerating upwards.

 * Fast. The motor has burned out and the
   rocket is decelerating, but it is going
   faster than 200m/s.

 * Coast. The rocket is still moving upwards
   and decelerating, but the speed is less
   than 200m/s.

 * Drogue. The rocket has reached apogee and
   is heading back down, but is above the
   configured Main altitude.

 * Main. The rocket is still descending, and
   is below the Main altitude

 * Landed. The rocket is no longer moving.

You can select a state to limit when the pyro channel may activate;
note that the check is based on when the rocket transitions *into* the
state, and so checking for “greater than Boost” means that the rocket
is currently in boost or some later state.

When a motor burns out, the rocket enters either Fast or Coast state
(depending on how fast it is moving). If the computer detects upwards
acceleration again, it will move back to Boost state.
