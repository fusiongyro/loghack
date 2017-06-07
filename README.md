# Loghack

This is just a demo of a “hack” project I whipped together with
SWI-Prolog and the tty library.

I think it would be neat to try and marry the adventure style with the
hack style by having a direct-move mode and an NLP mode, and having it
hold long descriptions of objects and places as well as having a more
interactive style of fighting for those cases where you must. This
would be a good fit for Prolog, especially SWI, except for the fact
that the tty support is not very strong, basically using whatever
termcap supports but not real direct keyboard input (try handling the
‘up’ key) and I’m not sure how to achieve color effects either.

But, it’s short, and it should be easy to improve on, so that’s something.
