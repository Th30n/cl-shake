# Doom input handling, every TICRATE (1/35) seconds:

  * Poll SDL events and convert to internal event repr.
    - queue, max-sized, read/tail and write/head pointers
  * Handle events:
    - menu -> directly
    - game -> update state (keyboard, mouse, ...)
  * Build ticcmds (at least 1 per frame)
    - from input state, translate keybindings -> only 1 binding per key
  * Simulate ticcmd (TICRATE)


# Quake input handling, every FRAMERATE (30-72 fps):

  * Poll Sys keyboard events, convert and handle:
    - update keyboard state
    - menu -> directly
    - rest -> *console* bindings (handles multiple bindings)
              or console input
  * Execute *console* commands:
    - update game key state (e.g. attack, moveleft ...)
    - other
  * Build cmd (only 1):
    - from game key state
    - get mouse input
  * Simulate a cmd over delta time
    - cmd stores duration/dt


# cl-shake input handling:

  * Poll SDL events and handle:
    - menu -> directly
    - game -> update state (keyboard, mouse, ...)
  * Build ticcmd, every TICRATE seconds:
    - from input state, translate keybindings
  * Simulate ticcmd (TICRATE)

No console, no multiple bindings
