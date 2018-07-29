qsl
===

A little while ago (2015-10-13, apparently) I wondered

>   Is there sufficient space and processing power on an Arduino Nano
>   to run something that's recognisably an interactive Lisp environment?
>   And, if so, would the massive 2K of RAM be enough to do anything useful?
>   Or even enough to blink the LED on a timer?

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

Getting that far turned out to be surprisingly easy, so I kept adding
features until I could do this:

```
(let ((state t))
  (defun blink ()
    (pin 13 state)
    (setq state (not state))))

(on-tick 500 blink)

(on-serial
 (lambda ()
   (cond
    ((> (readchar) ? ) (setq running nil)))))

(setq running t)
(while running
  (wait-for-event)
  (apply (next-event)))
```
to get the LED blinking at 1Hz, have the interpreter return to the
prompt on a keystroke, _and_ with the processor spending most of its
time in a `sleep` state.

----------------------------------------------------------------------------

I had, for no discernible reason, decided that it should take me no
more than a month to get something working.  In fact, it took about
three weeks to get from zero to `blink`, but ...

After running out of memory on even _mildly_ complex functions, I
wondered if there might be some possible improvements to be made.  A
month was a pretty tight schedule, and it meant that I was more or less
committed to interpreting directly from the raw CONS cells the user
had entered at the REPL prompt.  This caused a few problems.

Consider: if `eval` saw `(while (< n 10) (setq n (+ n 1)))`, then

-   since `while` isn't a true function, `eval` calls a special handler

-   `while` then needs to call `eval` for each clause in the body

-   this nested `eval` needs a special handler for the `setq`

-   `setq` calls _yet another_ `eval` to do the `+`

This `eval`-recursion happens at both the C and Lisp levels.  If we
were a bit smarter, though, we could flatten the original expression
to something like:

      XXX: n@, 10, <, jump-if-nil YYY, n@, 1, +, n!, jump XXX, YYY:

which can be evaluated with _no_ recursion.  Yes, we still need the
original ping-pong to do this compilation, but only at the C level.
**And**, once we've compiled an expression, we can abandon the original
conses and re-use the memory to do the evaluation.  Win/win.

So, does an Arduino Nano have enough ROM and RAM to implement a
bytecode compiler and interpreter?  This took a bit long than three
weeks to answer - about a year and a half, in fact - but the
conclusion seems to be "yes".

----------------------------------------------------------------------------

Yesterday (2018-07-28) I was in a shop that was selling an Arduino Mega
2560 quite cheaply, and decided it might be interesting to see if the
assumptions QSL makes are as portable as I'd hoped.

As it happens, the only change I needed to make was in the `pin` function
(in `hardware.cpp`): the Mega needs an explicit `pinMode(...,OUTPUT)`,
but the Nano is happy with `digitalWrite()` alone.
