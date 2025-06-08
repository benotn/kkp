;;; kkp.el --- Enable support for the Kitty Keyboard Protocol -*- lexical-binding: t -*-

;; Copyright (C) 2025  Benjamin Orthen

;; Author: Benjamin Orthen <contact@orthen.net>
;; Maintainer: Benjamin Orthen <contact@orthen.net>
;; Keywords: terminals
;; Version: 0.4
;; URL: https://github.com/benotn/kkp
;; Package-Requires: ((emacs "27.1") (compat "29.1.3.4"))

;; This file is not part of GNU Emacs.

;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;; kkp.el enables support for the Kitty Keyboard Protocol in Emacs.
;; This protocol is documented here:
;; https://sw.kovidgoyal.net/kitty/keyboard-protocol. It provides an
;; alternative, improved way to transmit keyboard input from a
;; terminal to Emacs running in that terminal.

;; For debugging key translation, see the file `kkp-debug.el`.
;;
;; To load the debugging helpers, you can run:
;;    (require 'kkp-debug)
;;
;; or M-x load-library RET kkp-debug RET


;;; Code:

;; kitty modifier encoding
;; shift     0b1         (1)
;; alt       0b10        (2)
;; ctrl      0b100       (4)
;; super     0b1000      (8)
;; hyper     0b10000     (16)
;; meta      0b100000    (32)
;; caps_lock 0b1000000   (64)
;; num_lock  0b10000000  (128)

;; Possible format of escape sequences sent to Emacs.
;; - CSI keycode u
;; - CSI keycode; modifier u
;; - CSI number ; modifier ~
;; - CSI {ABCDEFHPQRS}
;; - CSI 1; modifier {ABCDEFHPQRS}


(require 'cl-lib)
(require 'compat)
(require 'term/xterm)

(defgroup kkp nil
  "Kitty Keyboard Protocol (KKP) support."
  :group 'convenience
  :prefix "kkp-")

(defcustom kkp-terminal-query-timeout 0.1
  "Seconds to wait for an answer from the terminal. Nil means no timeout."
  :type 'float)

(defcustom kkp-active-enhancements
  '(disambiguate-escape-codes report-alternate-keys)
  "List of enhancements which should be enabled.
Possible values are the keys in `kkp--progressive-enhancement-flags'."
  :type '(repeat (choice (const disambiguate-escape-codes) (const report-alternate-keys))))

(defvar kkp--progressive-enhancement-flags
  '((disambiguate-escape-codes . (:bit 1))
    (report-alternate-keys . (:bit 4))))

(defconst kkp--modifiers
  '(choice (const shift) (const alt) (const control)
           (const super) (const hyper) (const meta)
           (const caps-lock) (const num-lock)))

;; These mirror the behavior of `mac-command-modifier' and friends.
;; They specify which virtual key the physical key maps to.
(defcustom kkp-shift-modifier 'shift
  "This variable describes the behavior of the shift key.

It is one of the symbols `shift', `alt', `control', `super',
`hyper', `meta', `caps-lock' or `num-lock'."
  :type kkp--modifiers)

(defcustom kkp-alt-modifier 'meta
  "This variable describes the behavior of the alt key.

It is one of the symbols `shift', `alt', `control', `super',
`hyper', `meta', `caps-lock' or `num-lock'."
  :type kkp--modifiers)

(defcustom kkp-control-modifier 'control
  "This variable describes the behavior of the ctrl key.

It is one of the symbols `shift', `alt', `control', `super',
`hyper', `meta', `caps-lock' or `num-lock'."
  :type kkp--modifiers)

(defcustom kkp-super-modifier 'super
  "This variable describes the behavior of the super key.

It is one of the symbols `shift', `alt', `control', `super',
`hyper', `meta', `caps-lock' or `num-lock'."
  :type kkp--modifiers)

(defcustom kkp-hyper-modifier 'hyper
  "This variable describes the behavior of the hyper key.

It is one of the symbols `shift', `alt', `control', `super',
`hyper', `meta', `caps-lock' or `num-lock'."
  :type kkp--modifiers)

(defcustom kkp-meta-modifier 'meta
  "This variable describes the behavior of the meta key.

It is one of the symbols `shift', `alt', `control', `super',
`hyper', `meta', `caps-lock' or `num-lock'."
  :type kkp--modifiers)

(defcustom kkp-caps-lock-modifier 'caps-lock
  "This variable describes the behavior of the caps key.

It is one of the symbols `shift', `alt', `control', `super',
`hyper', `meta', `caps-lock' or `num-lock'."
  :type kkp--modifiers)

(defcustom kkp-num-lock-modifier 'num-lock
  "This variable describes the behavior of the num key.

It is one of the symbols `shift', `alt', `control', `super',
`hyper', `meta', `caps-lock' or `num-lock'."
  :type kkp--modifiers)

;; kitty modifier encoding
(put 'kkp-shift-modifier :encoding 1)
(put 'kkp-alt-modifier :encoding 2)
(put 'kkp-control-modifier :encoding 4)
(put 'kkp-super-modifier :encoding 8)
(put 'kkp-hyper-modifier :encoding 16)
(put 'kkp-meta-modifier :encoding 32)
(put 'kkp-caps-lock-modifier :encoding 64)
(put 'kkp-num-lock-modifier :encoding 128)

(defvar kkp--printable-ascii-letters
  (cl-loop for c from ?a to ?z collect c))

;; when not found in emacs source code, taken from http://xahlee.info/linux/linux_show_keycode_keysym.html
(defvar kkp--non-printable-keys-with-u-terminator
  '((27 . "<escape>")
    (13 . "<return>")
    (?\s . "SPC")
    (9 . "<tab>")
    (127 . "<backspace>")
    (57358 . "<Caps_Lock>")
    (57359 . "<Scroll_Lock>")
    (57360 . "<kp-numlock>")
    (57361 . "<print>")
    (57362 . "<pause>")
    (57363 . "<menu>")
    (57376 . "<f13>")
    (57377 . "<f14>")
    (57378 . "<f15>")
    (57379 . "<f16>")
    (57380 . "<f17>")
    (57381 . "<f18>")
    (57382 . "<f19>")
    (57383 . "<f20>")
    (57384 . "<f21>")
    (57385 . "<f22>")
    (57386 . "<f23>")
    (57387 . "<f24>")
    (57388 . "<f25>")
    (57389 . "<f26>")
    (57390 . "<f27>")
    (57391 . "<f28>")
    (57392 . "<f29>")
    (57393 . "<f30>")
    (57394 . "<f31>")
    (57395 . "<f32>")
    (57396 . "<f33>")
    (57397 . "<f34>")
    (57398 . "<f35>")
    (57399 . "<kp-0>")
    (57400 . "<kp-1>")
    (57401 . "<kp-2>")
    (57402 . "<kp-3>")
    (57403 . "<kp-4>")
    (57404 . "<kp-5>")
    (57405 . "<kp-6>")
    (57406 . "<kp-7>")
    (57407 . "<kp-8>")
    (57408 . "<kp-9>")
    (57409 . "<kp-decimal>")
    (57410 . "<kp-divide>")
    (57411 . "<kp-multiply>")
    (57412 . "<kp-subtract>")
    (57413 . "<kp-add>")
    (57414 . "<kp-enter>")
    (57415 . "<kp-equal>")
    (57416 . "<kp-separator>")
    (57417 . "<kp-left>")
    (57418 . "<kp-right>")
    (57419 . "<kp-up>")
    (57420 . "<kp-down>")
    (57421 . "<kp-prior>") ;; KP_PAGE_UP
    (57422 . "<kp-next>") ;; KP_PAGE_DOWN
    (57423 . "<kp-home>")
    (57424 . "<kp-end>")
    (57425 . "<kp-insert>")
    (57426 . "<kp-delete>")
    (57428 . "<media-play>")
    (57429 . "<media-pause>")
    (57430 . "<media-play-pause>")
    (57431 . "<media-reverse>")
    (57432 . "<media-stop>")
    (57433 . "<media-fast-forward>")
    (57434 . "<media-rewind>")
    (57435 . "<media-next>") ;; MEDIA_TRACK_NEXT
    (57436 . "<media-previous>") ;; MEDIA_TRACK_PREVIOUS
    (57437 . "<media-record>")
    (57438 . "<volume-down>")
    (57439 . "<volume-up>")
    (57440 . "<volume-mute>")

    ;; it is rather unlikely Emacs gets this keysequence directly from the terminal
    ;; but just for the case...
    (57441 . "<SHIFT_L>")
    (57442 . "<Control_L>")
    (57443 . "<Alt_L>")
    (57444 . "<Super_L>")
    (57445 . "<Hyper_L>")
    (57446 . "<Meta_L>")
    (57447 . "<Shift_R>")
    (57448 . "<Control_R>")
    (57449 . "<Alt_R>")
    (57450 . "<Super_R>")
    (57451 . "<Hyper_R>")
    (57452 . "<Meta_R>")
    (57453 . "<ISO_Level3_Shift>")
    (57454 . "<ISO_Level5_Shift>")))

(defvar kkp--non-printable-keys-with-tilde-terminator
  '((2 . "<insert>")
    (3 . "<delete>")
    (5 . "<prior>")
    (6 . "<next>")
    (7 . "<home>")
    (8 . "<end>")
    (11 . "<f1>")
    (12 . "<f2>")
    (13 . "<f3>")
    (14 . "<f4>")
    (15 . "<f5>")
    (17 . "<f6>")
    (18 . "<f7>")
    (19 . "<f8>")
    (20 . "<f9>")
    (21 . "<f10>")
    (23 . "<f11>")
    (24 . "<f12>")
    (57427 . "<kp-begin>")))

(defvar kkp--non-printable-keys-with-letter-terminator
  '((?A . "<up>")
    (?B . "<down>")
    (?C . "<right>")
    (?D . "<left>")
    (?E . "<kp-begin>")
    (?F . "<end>")
    (?H . "<home>")
    (?P . "<f1>")
    (?Q . "<f2>")
    (?S . "<f4>")))

(defvar kkp--letter-terminators
  (mapcar #'car kkp--non-printable-keys-with-letter-terminator))

(defvar kkp--acceptable-terminators
  (cl-concatenate 'list '(?u ?~) kkp--letter-terminators))

(defvar kkp--key-prefixes
  (cl-concatenate 'list
                  (cl-loop for c from ?1 to ?9 collect c)
                  kkp--letter-terminators))

(defvar kkp--active-terminal-list
  nil "Internal variable to track terminals which have enabled KKP.")

(defvar kkp--setup-visited-terminal-list
  nil "Internal variable to track visited terminals after enabling `global-kkp-mode´.")

(defvar kkp--suspended-terminal-list
  nil "Internal variable to track suspended terminals which have enabled KKP in activate state.")

(defvar kkp-terminal-setup-complete-hook nil
  "Hook run after KKP finishes terminal setup in a given terminal.")

(defvar kkp-terminal-teardown-complete-hook nil
  "Hook run after KKP finishes terminal teardown in a given terminal.")

;; NOTE this is a copy from x-alternatives-map
(defvar kkp-alternatives-map
  (let ((map (make-sparse-keymap)))
    ;; Map certain keypad keys into ASCII characters that people usually expect.
    (define-key map [M-backspace] [?\M-\d])
    (define-key map [M-delete] [?\M-\d])
    (define-key map [M-tab] [?\M-\t])
    (define-key map [M-linefeed] [?\M-\n])
    (define-key map [M-clear] [?\M-\C-l])
    (define-key map [M-return] [?\M-\C-m])
    (define-key map [M-escape] [?\M-\e])
    (unless (featurep 'ns)
      (define-key map [iso-lefttab] [backtab])
      (define-key map [S-iso-lefttab] [backtab]))
    (and (or (eq system-type 'windows-nt)
	         (featurep 'ns))
	     (define-key map [S-tab] [backtab]))
    map)
  "Keymap of possible alternative meanings for some keys.")


(defun kkp--mod-bits (modifier)
  "Return the KKP encoding bits that should be interpreted as MODIFIER.

MODIFIER is one of the symbols `shift', `alt', `control',
`super', `hyper', `meta', `caps-lock' or `num-lock'."
  (apply #'logior
         (cl-map 'sequence
                 (lambda (sym)
                   (get sym :encoding))
                 (cl-remove-if-not
                  (lambda (sym) (eq (symbol-value sym) modifier))
                  '(kkp-shift-modifier
                    kkp-alt-modifier
                    kkp-control-modifier
                    kkp-super-modifier
                    kkp-hyper-modifier
                    kkp-meta-modifier
                    kkp-caps-lock-modifier
                    kkp-num-lock-modifier)))))

(defun kkp--csi-escape (&rest args)
  "Prepend the CSI bytes before the ARGS."
  (concat "\e[" (apply #'concat args)))

(defun kkp--selected-terminal ()
  "Get the terminal that is now selected."
  (frame-terminal (selected-frame)))

(defun kkp--bit-set-p (num bit)
  "Check if BIT is set in NUM."
  (not (eql (logand num bit) 0)))

(defun kkp--create-modifiers-string (modifier-num)
  "Create a string of Emacs key modifiers according to MODIFIER-NUM."

  ;; add modifiers as defined in key-valid-p
  ;; Modifiers have to be specified in this order:
  ;;    A-C-H-M-S-s which is
  ;;    Alt-Control-Hyper-Meta-Shift-super

  (let ((key-str ""))
    (when (kkp--bit-set-p modifier-num (kkp--mod-bits 'alt))
      (setq key-str (concat key-str "A-")))
    (when (kkp--bit-set-p modifier-num (kkp--mod-bits 'control))
      (setq key-str (concat key-str "C-")))
    (when (kkp--bit-set-p modifier-num (kkp--mod-bits 'hyper))
      (setq key-str (concat key-str "H-")))
    (when (kkp--bit-set-p modifier-num (kkp--mod-bits 'meta))
      (setq key-str (concat key-str "M-")))
    (when (kkp--bit-set-p modifier-num (kkp--mod-bits 'shift))
      (setq key-str (concat key-str "S-")))
    (when (kkp--bit-set-p modifier-num (kkp--mod-bits 'super))
      (setq key-str (concat key-str "s-")))
    key-str))


(defun kkp--get-keycode-representation (keycode mapping)
  "Try to lookup the Emacs key representation for KEYCODE in MAPPING.
This is either in the mapping or it is the string representation of the
key codepoint."
  (let ((rep (alist-get keycode mapping)))
    (if rep
        rep
      (string keycode))))

(defun kkp--handle-bracketed-paste (terminal-input)
  "Handle bracketed paste mode sequences.
TERMINAL-INPUT is a list of characters representing the terminal input
sequence. This function checks if TERMINAL-INPUT matches the bracketed
paste mode sequence ('200~') and calls `xterm-translate-bracketed-paste`
if it does."
  (when (equal (concat terminal-input) "200~")
    (xterm-translate-bracketed-paste nil)))


(defun kkp--handle-u-or-tilde-terminators (terminator terminal-input)
  "Handle input sequences ending with ?u or ?~ terminators.
TERMINATOR is the last character in the TERMINAL-INPUT sequence,
indicating the end of the sequence. TERMINAL-INPUT is a list of
characters representing the terminal input sequence. This function
contains the specific logic for processing sequences terminated by ?u or
?~."
  (let* ((terminator-alist (pcase terminator
                             (?u kkp--non-printable-keys-with-u-terminator)
                             (?~ kkp--non-printable-keys-with-tilde-terminator)))
         (input-string (mapconcat 'char-to-string (remq terminator terminal-input) ""))
         (input-parts (split-string input-string ";"))
         (keycode-parts (split-string (cl-first input-parts) ":")) ;; get keycodes from sequence
         (primary-keycode (cl-first keycode-parts))
         (secondary-keycode-str (cl-second keycode-parts))
         (secondary-keycode (and (stringp secondary-keycode-str)
                                 (not (string-empty-p secondary-keycode-str))
                                 secondary-keycode-str))
         (modifier-parts (split-string (or (cl-second input-parts) "") ":")) ;; list of modifiers and event types
         (modifier-string (cl-first modifier-parts))
         (modifier-num (if (not (string-empty-p modifier-string))
                           (1- (string-to-number modifier-string))
                         0))
         (has-shift (/= 0 (logand modifier-num (kkp--mod-bits 'shift))))
         (has-ctrl (/= 0 (logand modifier-num (kkp--mod-bits 'control))))
         (is-ascii-letter (member (string-to-number primary-keycode) kkp--printable-ascii-letters))
         ;; -------------------------------------------------------------
         ;; Shift-collapse rule:
         ;;   • Shift present
         ;;   • secondary code available
         ;;   • no Control *when key is an ASCII letter*
         ;; -------------------------------------------------------------
         (use-secondary (and secondary-keycode
                             has-shift
                             (or (not is-ascii-letter)
                                 (not has-ctrl))))
         (final-key-code (if use-secondary secondary-keycode primary-keycode)))

    ;; if we use the secondary keycode, remove shift from modifier number
    (when use-secondary
      (setq modifier-num (logand modifier-num (lognot 1))))

    ;; create keybinding by concatenating the modifier string with the key-name
    (let
        ((modifier-str (kkp--create-modifiers-string modifier-num))
         (key-name (kkp--get-keycode-representation (string-to-number final-key-code)
                                                    terminator-alist)))
      (kbd (concat modifier-str key-name)))))

(defun kkp--handle-letter-terminators (terminator terminal-input)
  "Handle input sequences ending with letter terminators.
TERMINATOR is the last character in the TERMINAL-INPUT sequence,
indicating the type of the terminator. TERMINAL-INPUT is a list of
characters representing the terminal input sequence."
  (let* ((input-string (mapconcat 'char-to-string (remq terminator terminal-input) ""))
         (input-parts (split-string input-string ";"))
         (modifier-parts (split-string (or (cl-second input-parts) "") ":")) ;; list of modifiers and event types
         (modifier-string (cl-first modifier-parts))
         (modifier-num (if (not (string-empty-p modifier-string))
                           (1- (string-to-number modifier-string))
                         0))
         (modifier-str (kkp--create-modifiers-string modifier-num))
         (key-name (alist-get terminator kkp--non-printable-keys-with-letter-terminator)))

    (kbd (concat modifier-str key-name))))

(defun kkp--translate-terminal-input (terminal-input)
  "Translate TERMINAL-INPUT according to KKP into an Emacs keybinding.
TERMINAL-INPUT is a list of characters representing the terminal input
sequence. This function dispatches the input sequence to the appropriate
handler based on its terminator:
- Bracketed paste mode sequences are
handled by `kkp--handle-bracketed-paste`.
- Sequences ending with ?u or ?~
are handled by `kkp--handle-u-or-tilde-terminators`.
- Sequences ending with a letter terminator are handled by
`kkp--handle-letter-terminators`.
The function returns the Emacs
keybinding associated with the terminal input sequence."

  ;; input has this form: keycode[:[shifted-key][:base-layout-key]];[modifiers[:event-type]][;text-as-codepoints]{u~}
  (let ((terminator (car (last terminal-input))))
    (or (kkp--handle-bracketed-paste terminal-input)
        (and (member terminator '(?u ?~)) (kkp--handle-u-or-tilde-terminators terminator terminal-input))
        (and (member terminator kkp--letter-terminators) (kkp--handle-letter-terminators terminator terminal-input)))))


(defun kkp--read-terminal-events (initial-byte)
  "Read terminal events until an acceptable terminator is found.
INITIAL-BYTE is the first byte of the sequence."
  (let ((events (list initial-byte)))
    (while (not (member (cl-first events) kkp--acceptable-terminators))
      (push (read-event) events))
    (nreverse events)))

(defun kkp--process-keys (first-byte)
  "Read input from terminal to parse key events to an Emacs keybinding.
FIRST-BYTE is the byte read before this function is called.
This function returns the Emacs keybinding associated with the sequence read."
  (let ((terminal-input (kkp--read-terminal-events first-byte)))
    (kkp--translate-terminal-input terminal-input)))


(defun kkp--get-enhancement-bit (enhancement)
  "Get the bitflag which enables the ENHANCEMENT."
  (plist-get (cdr enhancement) :bit))


(defun kkp--query-terminal-sync (query)
  "Send QUERY to TERMINAL (to current if nil) and return response (if any)."
  (discard-input)
  (send-string-to-terminal (kkp--csi-escape query))
  (let ((loop-cond t)
        (terminal-input nil))
    (while loop-cond
      (let ((evt (read-event nil nil kkp-terminal-query-timeout)))
        (if (null evt)
            (setq loop-cond nil)
          (push evt terminal-input))))
    (nreverse terminal-input)))


(defun kkp--query-terminal-async (query handlers terminal)
  "Send QUERY string to TERMINAL and register HANDLERS for a response.
HANDLERS is an alist with elements of the form (STRING . FUNCTION).
We run the first FUNCTION whose STRING matches the input events.
This function code is copied from `xterm--query'."
  (with-selected-frame (car (frames-on-display-list terminal))
    (let ((register
           (lambda (handlers)
             (dolist (handler handlers)
               (define-key input-decode-map (car handler)
                           (lambda (&optional _prompt)
                             ;; Unregister the handler, since we don't expect
                             ;; further answers.
                             (dolist (handler handlers)
                               (define-key input-decode-map (car handler) nil))
                             (funcall (cdr handler))
                             []))))))

      (funcall register handlers)
      (send-string-to-terminal (kkp--csi-escape query) terminal))))


(defun kkp--this-terminal-enabled-enhancements ()
  "Query the current terminal and return list of currently enabled enhancements."
  (let ((reply (kkp--query-terminal-sync "?u")))
    (when (not reply)
      (error "Terminal did not reply correctly to query"))

    (let ((enhancement-flag (- (nth 3 reply) ?0))
          (enabled-enhancements nil))

      (dolist (bind kkp--progressive-enhancement-flags)
        (when (> (logand enhancement-flag (kkp--get-enhancement-bit bind)) 0)
          (push (car bind) enabled-enhancements)))
      enabled-enhancements)))


(defun kkp--this-terminal-supports-kkp-p ()
  "Check if the current terminal supports the Kitty Keyboard Protocol.
This does not work well if checking for another terminal which
does not have focus, as input from this terminal cannot be reliably read."
  (let ((reply (kkp--query-terminal-sync "?u")))
    (and
     (member (length reply) '(5 6))
     (equal '(27 91 63) (cl-subseq reply 0 3))
     (eql 117 (car (last reply))))))

(defun kkp--this-terminal-has-active-kkp-p()
  "Check if the current terminal has KKP activated."
  (member (kkp--selected-terminal) kkp--active-terminal-list))

(defun kkp--calculate-flags-integer ()
  "Calculate the flag integer to send to the terminal to activate the enhancements."
  (cl-reduce (lambda (sum elt)
               (+
                sum
                (kkp--get-enhancement-bit (assoc elt kkp--progressive-enhancement-flags))))
             kkp-active-enhancements :initial-value 0))


;; NOTE this is a copy of the x-setup-function-keys function
(defun kkp-setup-function-keys (terminal)
  "Activate alternative keypad mappings in TERMINAL.
This function updates the `local-function-key-map` of the first frame on
TERMINAL’s display, reparenting it to `kkp-alternatives-map`. This remapping
causes certain keys, such as [M-backspace], to be interpreted like ASCII
characters (e.g., [?\M-\\d]). Once set, the parameter
`kkp-setup-function-keys` is stored on TERMINAL to avoid repeated setup."
  (let ((frame (car (frames-on-display-list terminal))))
    (unless (terminal-parameter terminal 'kkp-setup-function-keys)
      ;; Map certain keypad keys into ASCII characters that people usually expect.
      (with-selected-frame frame
        (set-keymap-parent kkp-alternatives-map (keymap-parent local-function-key-map))
        (set-keymap-parent local-function-key-map kkp-alternatives-map)))
    (set-terminal-parameter terminal 'kkp-setup-function-keys t)))

(defun kkp-teardown-function-keys (terminal)
  "Deactivate alternative keypad mappings in TERMINAL.
Restore the original `local-function-key-map` for the first frame on TERMINAL’s
display by removing `kkp-alternatives-map` as a parent. Once done, the parameter
`kkp-setup-function-keys` on TERMINAL is reset so that setup can be applied
again later if needed."
  (let ((frame (car (frames-on-display-list terminal))))
    (when (terminal-parameter terminal 'kkp-setup-function-keys)
      ;; Map certain keypad keys into ASCII characters that people usually expect.
      (with-selected-frame frame
        (set-keymap-parent local-function-key-map (keymap-parent kkp-alternatives-map)))
      (set-terminal-parameter terminal 'kkp-setup-function-keys nil))))


(defun kkp--terminal-teardown (terminal)
  "Run procedures to disable KKP in TERMINAL."
  (when
      (and
       (terminal-live-p terminal)
       (member terminal kkp--active-terminal-list))
    (kkp-teardown-function-keys terminal)
    (send-string-to-terminal (kkp--csi-escape "<u") terminal)

    (normal-erase-is-backspace-mode (terminal-parameter terminal 'kkp--previous-normal-erase-is-backspace-val))

    (with-selected-frame (car (frames-on-display-list terminal))
      (dolist (prefix kkp--key-prefixes)
        (compat-call define-key input-decode-map (kkp--csi-escape (string prefix)) nil t))
      (run-hooks 'kkp-terminal-teardown-complete-hook)))
  ;; We want to remove the terminal anyway from the active terminal list
  ;; Either we just tore it down, or it is not live anyway and should not be on the list.
  (setq kkp--active-terminal-list (delete terminal kkp--active-terminal-list)))


(defun kkp--terminal-setup ()
  "Run setup to enable KKP support in current terminal.
This does not work well if checking for another terminal which
does not have focus, as input from this terminal cannot be reliably read."

  (let ((terminal-input "")
        (terminal (kkp--selected-terminal))
        chr)
    (while (and (setq chr (read-event nil nil kkp-terminal-query-timeout)) (not (equal chr ?c)))
      (setq terminal-input (concat terminal-input (string chr))))

    ;; remove the setup-started parameter as soon as possible
    ;; to enable another try if somehow the string-match-p evaluates to nil
    (set-terminal-parameter terminal 'kkp--setup-started nil)

    ;; Condition: CSI?<flags>u CSI?...c must be in response
    ;; CSI? is already in response as it was registered as handler for the async request
    ;; thus it is not in terminal-input.
    (when (string-match-p (rx line-start
                              (+ digit) ;; <flags>
                              "u\e[?"
                              (+ anychar) ;; primary device attributes
                              eol) terminal-input)

      (unless (member terminal kkp--active-terminal-list)
        (let ((enhancement-flag (kkp--calculate-flags-integer)))
          (unless (eq enhancement-flag 0)

            (push terminal kkp--active-terminal-list)
            (send-string-to-terminal (kkp--csi-escape (format ">%su" enhancement-flag)) terminal)

            (kkp-setup-function-keys terminal)
            (set-terminal-parameter terminal 'kkp--previous-normal-erase-is-backspace-val (terminal-parameter terminal 'normal-erase-is-backspace))
            (normal-erase-is-backspace-mode 1)

            ;; we register functions for each prefix to not interfere with e.g., M-[ I
            (with-selected-frame (car (frames-on-display-list terminal))
              (dolist (prefix kkp--key-prefixes)
                (define-key input-decode-map (kkp--csi-escape (string prefix))
                            (lambda (_prompt) (kkp--process-keys prefix))))
              (run-hooks 'kkp-terminal-setup-complete-hook))))))))


(defun kkp--disable-in-active-terminals()
  "In all terminals with active KKP, pop the previously pushed enhancement flag."
  (dolist (terminal kkp--active-terminal-list)
    (kkp--terminal-teardown terminal)))


(defun kkp--suspend-in-terminal()
  "If the terminal has activate KKP, disable it before suspending."
  (let ((terminal (kkp--selected-terminal)))
    (when (member terminal kkp--active-terminal-list)
      (push terminal kkp--suspended-terminal-list)
      (kkp--terminal-teardown terminal))))

(defun kkp--resume-in-terminal()
  "Restore KKP in resumed terminals where it was active before suspension."
  (let ((terminal (kkp--selected-terminal)))
    (when (member terminal kkp--suspended-terminal-list)
      (setq kkp--suspended-terminal-list (delete terminal kkp--suspended-terminal-list))
      (kkp-enable-in-terminal terminal))))


(cl-defun kkp-enable-in-terminal (&optional (terminal (kkp--selected-terminal)))
  "Try to enable KKP support in Emacs running in the TERMINAL."
  (interactive)
  (when
      (and
       (terminal-live-p terminal)
       (not (display-graphic-p terminal)))
    (push terminal kkp--setup-visited-terminal-list)
    (unless
        (or
         (terminal-parameter terminal 'kkp--setup-started)
         (member terminal kkp--active-terminal-list))

      ;; NOTE: to avoid race conditions, we set the custom terminal
      ;; parameter here to not send the query multiple times to the
      ;; terminal
      (set-terminal-parameter terminal 'kkp--setup-started t)
      ;; https://sw.kovidgoyal.net/kitty/keyboard-protocol/#detection-of-support-for-this-protocol
      ;; query for the current progressive enhancements together with the primary device attributes
      (kkp--query-terminal-async "?u\e[c"
                                 '(("\e[?" . kkp--terminal-setup)) terminal))))

;;;###autoload
(defun kkp-disable-in-terminal ()
  "Disable in this terminal where command is executed, the activated enhancements."
  (interactive)
  (kkp--terminal-teardown (kkp--selected-terminal)))


(defun kkp-focus-change (&rest _)
  "Enable KKP when focus on terminal which has not yet enabled it once."
  (let* ((frame (selected-frame))
         (terminal (kkp--selected-terminal)))
    (when
        (and (not (member terminal kkp--setup-visited-terminal-list))
             (frame-focus-state frame))
      (kkp-enable-in-terminal))))

(defun kkp--display-symbol-keys-p (orig-fun &rest args)
  "Advice function for display-symbols-key-p ORIG-FUN with ARGS.
This ensures display-symbols-key-p returns non nil in a terminal with KKP enabled."
  (or
   (member (kkp--selected-terminal) kkp--active-terminal-list)
   (apply orig-fun args)))

;;;###autoload
(define-minor-mode global-kkp-mode
  "Toggle KKP support in all terminal frames."
  :global t
  :lighter nil
  :group 'kkp
  (cond
   (global-kkp-mode
    ;; if terminal has KKP enabled, it supports symbol names as keys
    (advice-add 'display-symbol-keys-p :around #'kkp--display-symbol-keys-p)
    ;; call setup for future terminals to be opened
    (add-hook 'tty-setup-hook #'kkp-enable-in-terminal)
    ;; call teardown for terminals to be closed
    (add-hook 'kill-emacs-hook #'kkp--disable-in-active-terminals)
    ;; we call this on each frame teardown, this has no effects if kkp is not enabled
    (add-to-list 'delete-terminal-functions #'kkp--terminal-teardown)
    (add-hook 'suspend-hook #'kkp--suspend-in-terminal)
    (add-hook 'suspend-resume-hook #'kkp--resume-in-terminal)

    ;; this is by far the most reliable method to enable kkp in all associated terminals
    ;; trying to switch to each terminal with `with-selected-frame' does not work very well
    ;; as input from `read-event' cannot be reliably read from the corresponding terminal
    (add-function :after after-focus-change-function #'kkp-focus-change)
    (setq kkp--setup-visited-terminal-list nil)

    ;; At startup, this global mode might be called before the 'tty-setup-hook'.
    ;; To avoid running 'kkp-enable-in-terminal' before, we only run it if
    ;; the 'tty-run-terminal-initialization' already ran.
    (when (terminal-parameter (kkp--selected-terminal) 'terminal-initted)
      (kkp-enable-in-terminal)))
   (t
    (advice-remove 'display-symbol-keys-p #'kkp--display-symbol-keys-p)
    (kkp--disable-in-active-terminals)
    (remove-hook 'tty-setup-hook #'kkp-enable-in-terminal)
    (remove-hook 'kill-emacs-hook #'kkp--disable-in-active-terminals)
    (remove-hook 'suspend-hook #'kkp--suspend-in-terminal)
    (remove-hook 'suspend-resume-hook #'kkp--resume-in-terminal)
    (remove-function after-focus-change-function #'kkp-focus-change)
    (setq delete-terminal-functions (delete #'kkp--terminal-teardown delete-terminal-functions)))))


;;;###autoload
(defun kkp-status ()
  "Message, if terminal supports KKP, if yes, currently enabled enhancements."
  (interactive)
  (if (kkp--this-terminal-supports-kkp-p)
      (message "KKP supported in this terminal.\n%s"
               (if (kkp--this-terminal-has-active-kkp-p)
                   (format "KKP active in this terminal. Enabled enhancements: %s" (mapconcat 'symbol-name (kkp--this-terminal-enabled-enhancements) " and "))
                 "KKP not active in this terminal."))
    (message "KKP not supported in this terminal.")))


(provide 'kkp)
;;; kkp.el ends here
