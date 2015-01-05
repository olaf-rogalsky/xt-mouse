# xt-mouse

Enable mouse support for emacs when run inside a xterm TTY frame. This
version of `xt-mouse.el` extends the official version by highliting
the selection during mouse dragging.

## Usage

Install the file at a place, where it is searched before official version. Then
put the following somewhere in your init script.

```lisp
(require 'xt-mouse)
(xterm-mouse-mode 1)
```
