;;; xt-mouse.el --- support the mouse when emacs run in an xterm

;; Copyright (C) 1994, 2000-2014 Free Software Foundation, Inc.

;; Author: Per Abrahamsen <abraham@dina.kvl.dk>
;; Keywords: mouse, terminals

;; This file is part of GNU Emacs.

;; GNU Emacs is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; GNU Emacs is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; Enable mouse support when running inside an xterm.

;; This is actually useful when you are running X11 locally, but is
;; working on remote machine over a modem line or through a gateway.

;; It works by translating xterm escape codes into generic emacs mouse
;; events so it should work with any package that uses the mouse.

;; You don't have to turn off xterm mode to use the normal xterm mouse
;; functionality, it is still available by holding down the SHIFT key
;; when you press the mouse button.

;;; Todo:

;; Support multi-click -- somehow.

;;; Code:

(defvar xterm-mouse-debug-buffer nil)

(defun xterm-mouse-translate (_event)
  "Read a click and release event from XTerm."
  (xterm-mouse-translate-1))

(defun xterm-mouse-translate-extended (_event)
  "Read a click and release event from XTerm.
Similar to `xterm-mouse-translate', but using the \"1006\"
extension, which supports coordinates >= 231 (see
http://invisible-island.net/xterm/ctlseqs/ctlseqs.html)."
  (xterm-mouse-translate-1 1006))

(defun xterm-mouse-translate-1 (&optional extension)
  (save-excursion
    (let* ((event (xterm-mouse-event extension))
	   (ev-command (nth 0 event))
	   (ev-data    (nth 1 event))
	   (ev-where   (nth 1 ev-data))
	   (vec (vector event))
	   (is-down (string-match "down-" (symbol-name ev-command)))
           (is-move (eq 'mouse-movement ev-command)))
      ;; Mouse events symbols must have an 'event-kind property with
      ;; the value 'mouse-click.
      (when ev-command (put ev-command 'event-kind 'mouse-click))

      (cond
       ((null event) nil)		;Unknown/bogus byte sequence!
       (is-down
	(setf (terminal-parameter nil 'xterm-mouse-last-down) event)
	vec)
       (is-move vec)
       (t
	(let* ((down (terminal-parameter nil 'xterm-mouse-last-down))
	       (down-data (nth 1 down))
	       (down-where (nth 1 down-data)))
          (setf (terminal-parameter nil 'xterm-mouse-last-down) nil)
	  (cond
	   ((null down)
	    ;; This is an "up-only" event.  Pretend there was an up-event
	    ;; right before and keep the up-event for later.
	    (push event unread-command-events)
	    (vector (cons (intern (replace-regexp-in-string
				   "\\`\\([ACMHSs]-\\)*" "\\&down-"
				   (symbol-name ev-command) t))
			  (cdr event))))
	   ((equal ev-where down-where) vec)
           (t
	    (let ((drag (if (symbolp ev-where)
			    0		;FIXME: Why?!?
			  (list (intern (replace-regexp-in-string
					 "\\`\\([ACMHSs]-\\)*" "\\&drag-"
					 (symbol-name ev-command) t))
				down-data ev-data))))
	      (if (null track-mouse)
		  (vector drag)
		(push drag unread-command-events)
		(vector (list 'mouse-movement ev-data))))))))))))

;; These two variables have been converted to terminal parameters.
;;
;;(defvar xterm-mouse-x 0
;;  "Position of last xterm mouse event relative to the frame.")
;;
;;(defvar xterm-mouse-y 0
;;  "Position of last xterm mouse event relative to the frame.")

(defvar xt-mouse-epoch nil)

;; Indicator for the xterm-mouse mode.

(defun xterm-mouse-position-function (pos)
  "Bound to `mouse-position-function' in XTerm mouse mode."
  (when (terminal-parameter nil 'xterm-mouse-x)
    (setcdr pos (cons (terminal-parameter nil 'xterm-mouse-x)
		      (terminal-parameter nil 'xterm-mouse-y))))
  pos)

(defun xterm-mouse-truncate-wrap (f)
  "Truncate with wrap-around."
  (condition-case nil
      ;; First try the built-in truncate, in case there's no overflow.
      (truncate f)
    ;; In case of overflow, do wraparound by hand.
    (range-error
     ;; In our case, we wrap around every 3 days or so, so if we assume
     ;; a maximum of 65536 wraparounds, we're safe for a couple years.
     ;; Using a power of 2 makes rounding errors less likely.
     (let* ((maxwrap (* 65536 2048))
            (dbig (truncate (/ f maxwrap)))
            (fdiff (- f (* 1.0 maxwrap dbig))))
       (+ (truncate fdiff) (* maxwrap dbig))))))

;; The following code relies on the evaluation order of function
;; paramerters, which must be evaluated from left to right. According
;; to the elips manual "Evaluation of Function Forms" this is true.
;; Ther is no error checking performed wether the utf-8 character is
;; encoded with minimal number of bytes.
(defun read-utf8-char (&optional prompt seconds)
  "Read an utf-8 encoded character from the current terminal.
This function reads and returns an utf-8 encoded character of
command input. If the user generates an event which is not a
character (i.e., a mouse click or function key event), read-char
signals an error.

The returned event may come directly from the user, or from a
keyboard macro. It is not decoded by the keyboard's input coding
system and always treated with an utf-8 input encoding.

The optional arguments prompt and seconds work like in
`read-event'. But note, that if the utf character is encoded with
several bytes, then `read-utf8-char' waits for each of those
bytes for the given time.

There is no provision for error detecting of illegal utf-8
sequences."
  (let ((c (read-char prompt nil seconds)))
    (cond
     ((< c 128)
      c)
     ((< c 224)
      (+ (lsh (logand c 31) 6)
         (logand (read-char prompt nil seconds) 63)))
     ((< c 240)
      (+ (lsh (logand c 15) 12)
         (lsh (logand (read-char prompt nil seconds) 63) 6)
         (logand (read-char prompt nil seconds) 63)))
     ((< c 248)
      (+ (lsh (logand c 7) 18)
         (lsh (logand (read-char prompt nil seconds) 63) 12)
         (lsh (logand (read-char prompt nil seconds) 63) 6)
         (logand (read-char prompt nil seconds) 63)))
     (t
      (error "An iIllegal utf-8 character code was received from the terminal")))))

;; In default mode each numeric parameter of XTerm's mouse reports is
;; a single char, possibly encoded as utf-8.  The actual numeric
;; parameter then is obtained by subtracting 32 from the character
;; code.  In extendend mode the parameters are returned as decimal
;; string delemited either by semicolons or for the last parameter by
;; one of the characters "m" or "M". If the last character is a "m",
;; then the mouse event was a button release, else it was a button
;; press or a mouse motion.
(defmacro xterm-mouse--read-number-from-terminal (c extension)
  `(if ,extension
       (let ((n 0))
         (while (progn
                  (setq ,c (read-utf8-char))
                  (<= ?0 ,c ?9))
           (setq n (+ (* 10 n) ,c ,(- ?0))))
         n)
     (- (setq ,c (read-utf8-char)) 32)))

;; XTerm reports mouse events as
;; <EVENT-CODE> <X> <Y> in default mode, and
;; <EVENT-CODE> ";" <X> ";" <Y> <"M" or "m"> in extended mode.
;; The macro read-number-from-terminal takes care of reading
;; the response parameters appropriatly. The event codes differ
;; slightly between default and extended mode.
;; Return a list (EVENT-TYPE-SYMBOL X Y).
(defun xterm-mouse--read-event-sequence (&optional extension)
  (let* (c ; remember last read char
         (code (xterm-mouse--read-number-from-terminal c extension))
         (x (1- (xterm-mouse--read-number-from-terminal c extension)))
         (y (1- (xterm-mouse--read-number-from-terminal c extension)))
         (wheel (/= (logand code 64) 0))
         (move (/= (logand code 32) 0))
         (ctrl (/= (logand code 16) 0))
         (meta (/= (logand code 8) 0))
         (shift (/= (logand code 4) 0))
         (down (and (not wheel)
                    (not move)
                    (if extension
                        (eq c ?M)
                      (/= (logand code 3) 3))))
         (btn (if (or extension down wheel)
                  (+ (logand code 3) (if wheel 4 1))
                ;; The default mouse protocol does not report the button
                ;; number in release events: use the button from the last
                ;; button-down event.
                (terminal-parameter nil 'xterm-mouse-last-button)
                ;; Spurious release event without previous button-down
                ;; event: assume, that the last button was button 1.
                1))
         (sym (if move 'mouse-movement
                (intern (concat (if ctrl "C-" "")
                                (if meta "M-" "")
                                (if shift "S-" "")
                                (if down "down-" "")
                                "mouse-"
                                (number-to-string btn))))))
    (if down (set-terminal-parameter nil 'xterm-mouse-last-button btn))
    (list sym x y)))

(defun xterm-mouse--set-click-count (event click-count)
  (setcdr (cdr event) (list click-count))
  (let ((name (symbol-name (car event))))
    (when (string-match "\\(.*?\\)\\(\\(?:down-\\)?mouse-.*\\)" name)
      (setcar event
              (intern (concat (match-string 1 name)
                              (if (= click-count 2)
                                  "double-" "triple-")
                              (match-string 2 name)))))))

(defun xterm-mouse-event (&optional extension)
  "Convert XTerm mouse event to Emacs mouse event.
EXTENSION, if non-nil, means to use an extension to the usual
terminal mouse protocol; we currently support the value 1006,
which is the \"1006\" extension implemented in Xterm >= 277."
  (let* ((click (cond ((or (null extension) (= extension 1006))
		       (xterm-mouse--read-event-sequence extension))
		      (t
		       (error "Unsupported XTerm mouse protocol")))))
    (when click
      (let* ((type (nth 0 click))
             (x    (nth 1 click))
             (y    (nth 2 click))
             ;; Emulate timestamp information.  This is accurate enough
             ;; for default value of mouse-1-click-follows-link (450msec).
             (timestamp (xterm-mouse-truncate-wrap
                         (* 1000
                            (- (float-time)
                               (or xt-mouse-epoch
                                   (setq xt-mouse-epoch (float-time)))))))
             (w (window-at x y))
             (ltrb (window-edges w))
             (left (nth 0 ltrb))
             (top (nth 1 ltrb))
             (posn (if w
                                (posn-at-x-y (- x left) (- y top) w t)
                              (append (list nil 'menu-bar)
                             (nthcdr 2 (posn-at-x-y x y)))))
             (event (list type posn)))
        (setcar (nthcdr 3 posn) timestamp)

        ;; Try to handle double/triple clicks.
        (let* ((last-click (terminal-parameter nil 'xterm-mouse-last-click))
               (last-type (nth 0 last-click))
               (last-name (symbol-name last-type))
               (last-time (nth 1 last-click))
               (click-count (nth 2 last-click))
               (this-time (float-time))
               (name (symbol-name type)))
          (cond
           ((not (string-match "down-" name))
            ;; For up events, make the up side match the down side.
            (setq this-time last-time)
            (when (and click-count (> click-count 1)
                       (string-match "down-" last-name)
                       (equal name (replace-match "" t t last-name)))
              (xterm-mouse--set-click-count event click-count)))
           ((not last-time) nil)
           ((and (> double-click-time (* 1000 (- this-time last-time)))
                 (equal last-name (replace-match "" t t name)))
            (setq click-count (1+ click-count))
            (xterm-mouse--set-click-count event click-count))
           (t (setq click-count 1)))
          (set-terminal-parameter nil 'xterm-mouse-last-click
                                  (list type this-time click-count)))

        (set-terminal-parameter nil 'xterm-mouse-x x)
        (set-terminal-parameter nil 'xterm-mouse-y y)
        (setq last-input-event event)))))

;;;###autoload
(define-minor-mode xterm-mouse-mode
  "Toggle XTerm mouse mode.
With a prefix argument ARG, enable XTerm mouse mode if ARG is
positive, and disable it otherwise.  If called from Lisp, enable
the mode if ARG is omitted or nil.

Turn it on to use Emacs mouse commands, and off to use xterm mouse commands.
This works in terminal emulators compatible with xterm.  It only
works for simple uses of the mouse.  Basically, only non-modified
single clicks are supported.  When turned on, the normal xterm
mouse functionality for such clicks is still available by holding
down the SHIFT key while pressing the mouse button."
  :global t :group 'mouse
  (funcall (if xterm-mouse-mode 'add-hook 'remove-hook)
           'terminal-init-xterm-hook
           'turn-on-xterm-mouse-tracking-on-terminal)
  (if xterm-mouse-mode
      ;; Turn it on
      (progn
	(setq mouse-position-function #'xterm-mouse-position-function)
        (mapc #'turn-on-xterm-mouse-tracking-on-terminal (terminal-list)))
    ;; Turn it off
    (mapc #'turn-off-xterm-mouse-tracking-on-terminal (terminal-list))
    (setq mouse-position-function nil)))

(defconst xterm-mouse-tracking-enable-sequence
  "\e[?1000h\e[?1002h\e[?1005h\e[?1006h"
  "Control sequence to enable xterm mouse tracking.
Enables basic tracking, then extended tracking on
terminals that support it.")

(defconst xterm-mouse-tracking-disable-sequence
  "\e[?1006l\e[?1005l\e[?1002l\e[?1000l"
  "Reset the modes set by `xterm-mouse-tracking-enable-sequence'.")

(defun turn-on-xterm-mouse-tracking-on-terminal (&optional terminal)
  "Enable xterm mouse tracking on TERMINAL."
  (when (and xterm-mouse-mode (eq t (terminal-live-p terminal))
	     ;; Avoid the initial terminal which is not a termcap device.
	     ;; FIXME: is there more elegant way to detect the initial
             ;; terminal?
	     (not (string= (terminal-name terminal) "initial_terminal")))
    (unless (terminal-parameter terminal 'xterm-mouse-mode)
      ;; Simulate selecting a terminal by selecting one of its frames
      ;; so that we can set the terminal-local `input-decode-map'.
      (with-selected-frame (car (frames-on-display-list terminal))
        (define-key input-decode-map "\e[M" 'xterm-mouse-translate)
        (define-key input-decode-map "\e[<" 'xterm-mouse-translate-extended))
      (condition-case err
          (send-string-to-terminal xterm-mouse-tracking-enable-sequence
                                   terminal)
        ;; FIXME: This should use a dedicated error signal.
        (error (if (equal (cadr err) "Terminal is currently suspended")
                   nil                  ;The sequence will be sent upon resume.
                 (signal (car err) (cdr err)))))
      (push xterm-mouse-tracking-enable-sequence
            (terminal-parameter nil 'tty-mode-set-strings))
      (push xterm-mouse-tracking-disable-sequence
            (terminal-parameter nil 'tty-mode-reset-strings))
      (set-terminal-parameter terminal 'xterm-mouse-mode t))))

(defun turn-off-xterm-mouse-tracking-on-terminal (terminal)
  "Disable xterm mouse tracking on TERMINAL."
  ;; Only send the disable command to those terminals to which we've already
  ;; sent the enable command.
  (when (and (terminal-parameter terminal 'xterm-mouse-mode)
             (eq t (terminal-live-p terminal)))
    ;; We could remove the key-binding and unset the `xterm-mouse-mode'
    ;; terminal parameter, but it seems less harmful to send this escape
    ;; command too many times (or to catch an unintended key sequence), than
    ;; to send it too few times (or to fail to let xterm-mouse events
    ;; pass by untranslated).
    (condition-case err
        (send-string-to-terminal xterm-mouse-tracking-disable-sequence
                                 terminal)
      ;; FIXME: This should use a dedicated error signal.
      (error (if (equal (cadr err) "Terminal is currently suspended")
                 nil
               (signal (car err) (cdr err)))))
    (setf (terminal-parameter nil 'tty-mode-set-strings)
          (remq xterm-mouse-tracking-enable-sequence
                (terminal-parameter nil 'tty-mode-set-strings)))
    (setf (terminal-parameter nil 'tty-mode-reset-strings)
          (remq xterm-mouse-tracking-disable-sequence
                (terminal-parameter nil 'tty-mode-reset-strings)))
    (set-terminal-parameter terminal 'xterm-mouse-mode nil)))

(provide 'xt-mouse)

;;; xt-mouse.el ends here
