# qsl

A little while ago (2015-10-13, apparently) I wondered

> Is there sufficient space and processing power on an Arduino Nano to run something that's recognisably an interactive Lisp environment?  And, if so, would the massive 2K of RAM be enough to do anything useful?  Or even enough to blink the LED on a timer?

Thus was born QSL - a Quite Small Lisp.

For "an interactive Lisp environment", I was aiming for something like:
```
qsl> (+ 3 4)
= 7
```
(though, to be honest, if I'd ended up with
```
qsl> (+ 3 4)
= 9
```
I'd've considered that something of a win).

Getting that far turned out to be surprisingly easy, so I kept adding features until I could do this:

```
(let ((state t))
  (defun blink ()
    (pin 13 state)
    (setq state (not state))))

(on-tick 500 blink)

(on-serial
 (lambda ()
   (cond
    ((>= (readchar) ? ) (setq running nil)))))

(setq running t)
(while running
  (wait-for-event)
  (do-events))
```
to get the LED blinking at 1Hz, have the interpreter return to the prompt on a keystroke, _and_ with the processor spending most of its time in a `sleep` state.

Now all I need to do is document it.

