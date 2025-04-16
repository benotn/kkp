;;; kkp-debug.el --- Debugging helpers for Kitty Keyboard Protocol -*- lexical-binding: t; -*-

;; Copyright (C) 2025  Benjamin Orthen
;; Author: Benjamin Orthen <contact@orthen.net>
;; Maintainer: Benjamin Orthen <contact@orthen.net>
;; Version: 0.4
;; URL: https://github.com/benotn/kkp
;; Package-Requires: ((emacs "27.1") (compat "29.1.3.4"))
;; Keywords: terminals, debugging

;; This file is not part of GNU Emacs.

;;; Commentary:
;;
;; This file provides helper functions for debugging the key translation
;; of the kitty keyboard protocol. The main interactive command it provides is
;; `kkp-debug-describe-key-translation-chain'.
;;
;; The key translation process is broken into three stages:
;;
;;  1. Input decoding via `input-decode-map` (with a fallback to KKP helper functions if needed).
;;     This step converts raw terminal events into a canonical key sequence.
;;
;;  2. Normal binding lookup: the input-decoded sequence is checked against the
;;     active keymaps (local, minor, or global). If a normal binding exists,
;;     remapping via `local-function-key-map` is skipped.
;;
;;  3. If no normal binding is found, remapping via `local-function-key-map` is
;;     applied, followed by further translation via `key-translation-map`.
;;
;; The final key sequence and its associated command binding are then displayed,
;; with each stage output in aligned columns for clarity.
;;
;; To use these debugging helpers, you can either:
;;
;;   M-x load-library RET kkp-debug RET
;;
;; or add (require 'kkp-debug) to your Emacs init file.
;;
;;; Code:

(require 'cl-lib)
(require 'kkp)

(defvar kkp-debug--key-prompt "Input your key:")

(defun kkp-debug--key-sequence-has-shift (keyseq)
  "Check if any event in KEYSEQ includes the shift modifier."
  (seq-some (lambda (key) (member 'shift (event-modifiers key))) keyseq))

(defun kkp-debug--remove-shift-from-keyseq (keyseq)
  "Return a new key sequence from KEYSEQ with shift removed from all events."
  (vconcat (mapcar (lambda (key)
                     (let ((mods (remove 'shift (event-modifiers key)))
                           (basic (event-basic-type key))) ;; Get the base character
                       (event-convert-list (append mods (list basic))))) ;; Rebuild the event
                   keyseq)))

(defun kkp-debug--menu-item-binding (binding)
  "Return the \"real\" binding behind a menu-item BINDING.
If BINDING is a menu-item form, extract its REAL-BINDING.
If that binding is itself a menu-item, keep unwrapping.
Otherwise, return BINDING unchanged."
  (cond
   ((and (consp binding)
         (eq (car binding) 'menu-item))
    ;; A menu-item looks like (menu-item NAME REAL-BINDING . PROPS)
    ;; so the third element is the real binding.
    (kkp-debug--menu-item-binding (nth 2 binding)))
   (t
    binding)))

(defun kkp-debug--lookup-key-sequence (keymap keyseq)
  "Lookup KEYSEQ in KEYMAP using `map-keymap' logic, including menu-item submaps.
Return the \"raw\" binding (which may be a command, a keymap, or a menu-item),
or nil if KEYSEQ is not bound in KEYMAP.
Replicates prefix descent like `lookup-key' but does not actually call it."
  (let ((current keymap)
        (keys (append keyseq nil)))      ; convert vector/string to list of events
    (catch 'done
      (while t
        (when (null keys)
          ;; If we ran out of events, then `current` might be the final submap if
          ;; we consider a bare submap as a final binding.  Return it here.
          (throw 'done current))
        (let ((evt (pop keys))
              found-binding)
          ;; Search CURRENT for EVT.
          (catch 'found
            (map-keymap
             (lambda (map-key map-val)
               (when (equal evt map-key)
                 (setq found-binding map-val)
                 (throw 'found nil)))
             current))
          (unless found-binding
            ;; Nothing matched this event, fail.
            (throw 'done nil))
          ;; If found-binding is a menu-item, unwrap it to get the real binding.
          (setq found-binding (kkp-debug--menu-item-binding found-binding))

          (cond
           ;; If it's a keymap and we still have leftover events, descend.
           ((and (keymapp found-binding) keys)
            (setq current found-binding))

           ;; If it's a keymap but no leftover keys, treat the submap as final.
           ((and (keymapp found-binding) (null keys))
            (throw 'done found-binding))

           ;; If it's not a keymap but we do have leftover events, can't descend.
           ((and (not (keymapp found-binding)) keys)
            (throw 'done nil))

           ;; If it's not a keymap and no leftover keys, this is our final binding.
           (t
            (throw 'done found-binding))))))))


(defun kkp-debug--get-key-events-from-terminal ()
  "Read a full key sequence from the terminal.
This function uses `read-event' with a short timeout to gather all parts
of the sequence and returns them as a list of events."
  (let ((events (list (read-event kkp-debug--key-prompt)))
        (timeout 0.1)
        evt)
    ;; Gather additional events with a short timeout.
    (while (setq evt (read-event nil nil timeout))
      (push evt events))
    (nreverse events)))

(defun kkp-debug--translate-events-with-kkp-fallback (events)
  "Translate terminal input EVENTS using `input-decode-map`, with KKP fallback.
Steps:
1. Build a key sequence from EVENTS and look it up in `input-decode-map`.
2. If that lookup is not a number, it's a direct binding or nil, so return it.
3. If it returns a number (partial prefix match):
   a) Check if the first three events are 27 (ESC), 91 ([), and something
      in `kkp--key-prefixes`.
   b) If so, do a second `lookup-key` on exactly those three bytes.
      - If it returns a function/closure, log it but do NOT call it.
      - Then call `kkp--translate-terminal-input` on the rest of the events.
   c) Otherwise, return nil."
  (let* ((key-seq (vconcat events))
         (lookup-result (lookup-key input-decode-map key-seq)))
    (if (not (numberp lookup-result))
        ;; Case 1 & 2: Direct binding or nil => return as is.
        (progn
          (message "Mapping found in input-decode-map: %s" lookup-result)
          lookup-result)

      ;; Case 3: A number => partial prefix match.
      (if (and (>= (length events) 3)
               (eq (nth 0 events) 27)  ; ESC
               (eq (nth 1 events) 91)  ; [
               (memq (nth 2 events) kkp--key-prefixes))
          (let* ((prefix (seq-subseq events 0 3))
                 ;; Check short prefix in input-decode-map:
                 (short-prefix-fn (kkp-debug--lookup-key-sequence input-decode-map (vconcat prefix)))
                 (rest (nthcdr 2 events)))
            (when (and (functionp short-prefix-fn)
                       (let ((fn-text (format "%S" short-prefix-fn)))
                         (string-match-p "kkp--process-keys" fn-text)))
              (message "Short prefix %s mapped to function: %s (not calling it)."
                       (key-description (vconcat prefix)) short-prefix-fn)
              (message "Prefix and KKP closure detected, now calling kkp--translate-terminal-input on remainder: %s"
                       (key-description (vconcat rest)))
              (kkp--translate-terminal-input rest)))
        (message "lookup-key returned a prefix %s, but conditions not met." lookup-result)
        nil))))


;;;###autoload
(defun kkp-debug-describe-key-translation-chain ()
  "Display the key translation chain for a terminal key sequence.
The translation process is performed in the following stages:
  1. Input decoding via `input-decode-map` (with fallback to KKP if needed).
  2. Normal binding lookup: if the input-decoded sequence has a normal binding,
     then remapping via `local-function-key-map` is skipped.
  3. If no normal binding is found, remap via `local-function-key-map`.
  4. Finally, `key-translation-map` is applied unconditionally.
The resulting key sequence and its final command binding are displayed.
Output is arranged in aligned columns for clarity."
  (interactive)
  (with-help-window "*Key Translation Chain*"
    (let* ((kkp-is-active (kkp--this-terminal-has-active-kkp-p)))

      (if kkp-is-active
          (let* ((events (kkp-debug--get-key-events-from-terminal))
                 (raw-key (vconcat events))
                 (input-decoded-keys (kkp-debug--translate-events-with-kkp-fallback events))
                 ;; Check if the decoded key sequence already has a normal binding.
                 (normal-binding (key-binding input-decoded-keys))
                 ;; Stage 2: Apply local-function-key-map only if no normal binding exists.
                 (local-result (if normal-binding
                                   nil
                                 (let ((temp (lookup-key local-function-key-map input-decoded-keys)))
                                   (and (not (numberp temp)) temp))))
                 (local-output (or local-result input-decoded-keys))
                 ;; Stage 3: Always apply key-translation-map.
                 (translation-result (let ((temp (lookup-key key-translation-map local-output)))
                                       (and (not (numberp temp)) temp)))
                 (prelim-final-output (or translation-result local-output))
                 (prelim-final-binding (key-binding prelim-final-output))
                 (prelim-final-output-has-shift (kkp-debug--key-sequence-has-shift prelim-final-output))
                 (should-lowercase-binding (and (not prelim-final-binding) prelim-final-output-has-shift translate-upper-case-key-bindings))
                 (lowercase-output (kkp-debug--remove-shift-from-keyseq prelim-final-output))
                 (final-binding (if should-lowercase-binding
                                    (key-binding lowercase-output)
                                  prelim-final-binding)))
            (princ (format "%-45s %s\n" "KKP is active:" (if kkp-is-active "YES" "NO")))
            (princ (format "%-45s %s (key vector: %s)\n" "Raw key events:" (key-description raw-key) raw-key))
            (princ (format "%-45s %s\n" "After input-decode-map:" (key-description input-decoded-keys)))
            (princ (format "%-45s %s\n" "Normal binding (if any):" (or normal-binding "none")))
            (princ (format "%-45s %s => %s\n" "After local-function-key-map:"
                           (if normal-binding "not considered" (if local-result "found" "not found"))
                           (key-description local-output)))
            (princ (format "%-45s %s => %s\n" "After key-translation-map:"
                           (if translation-result "found" "not found")
                           (key-description prelim-final-output)))
            (when (and should-lowercase-binding final-binding)
              (princ (format "%-45s %s => %s\n" "Uppercase to lowercase binding:" (key-description prelim-final-output) (key-description lowercase-output))))
            (princ (format "%-45s %s\n" "Final command binding (in this help buffer):" (or final-binding "undefined"))))


        ;; KKP is not active
        (let ((translated-keys (read-key-sequence-vector kkp-debug--key-prompt))
              (raw-key (this-single-command-raw-keys)))
          (princ (format "%-45s %s\n" "KKP is active:" (if kkp-is-active "YES" "NO")))
          (princ (format "%-45s %s (key vector: %s)\n" "Raw key events:" (key-description raw-key) raw-key))
          (princ (format "%-45s %s\n" "After all key translation maps:" (key-description translated-keys)))
          (princ (format "%-45s %s\n" "Final command binding (in this help buffer):" (or (key-binding translated-keys) "undefined"))))))))

(provide 'kkp-debug)
;;; kkp-debug.el ends here
