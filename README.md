# xt-mouse

Enable mouse support for emacs when run inside a xterm TTY frame. This
version of `xt-mouse.el` extends the official version by highlighting
the selection during mouse dragging.

## Usage

Install the file at a suitible directory (e.g. $HOME/.emacs.d/lisp) and then
put the following somewhere into your init script.

```lisp
(add-to-list 'load-path "<INSTALLATION-DIRECTORY>")
(require 'xt-mouse)
(xterm-mouse-mode 1)
```

