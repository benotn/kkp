;;; kkp-tests.el --- Tests for kkp (Kitty Keyboard Protocol) -*- lexical-binding: t -*-

;; Copyright (C) 2025  Benjamin Orthen
;; This file is not part of GNU Emacs.

;;; Commentary:
;;
;; ERT tests for kkp.el that mimic:
;; - A strangely behaved terminal (malformed replies, garbage, partial CSI, wrong format)
;; - Slow SSH (no reply within timeout, partial/delayed reply)
;;
;; Run with: emacs -batch -l ert -l kkp.el -l kkp-tests.el -f ert-run-tests-batch-and-exit
;;
;;; Code:

(require 'ert)
(require 'cl-lib)
(require 'kkp)

;; ---------------------------------------------------------------------------
;; Terminal input in tests: use (string-to-list STR) so input is human-readable.
;; In strings, \e = ESC, \0 = NUL, \377 = byte 255 (octal).  KKP reply format:
;; CSI? then optional flag digits then "u" (e.g. "\e[?0u" or "\e[?01u").
;; ---------------------------------------------------------------------------

(defun kkp-test--events (string)
  "Return STRING as a list of character codes (events).  \\e is ESC."
  (string-to-list string))

(ert-deftest kkp-test/strange-terminal--nil-reply ()
  "Mimic terminal that never responds (e.g. broken or non-KKP)."
  (cl-letf (((symbol-function 'kkp--query-terminal-sync) (lambda (_query) nil)))
    (should-not (kkp--this-terminal-supports-kkp-p))))

(ert-deftest kkp-test/strange-terminal--empty-reply ()
  "Mimic terminal that sends nothing before timeout."
  (cl-letf (((symbol-function 'kkp--query-terminal-sync) (lambda (_query) (list))))
    (should-not (kkp--this-terminal-supports-kkp-p))))

(ert-deftest kkp-test/strange-terminal--wrong-length-short ()
  "Mimic terminal that sends too few bytes (e.g. truncated)."
  (cl-letf (((symbol-function 'kkp--query-terminal-sync)
             (lambda (_query) (kkp-test--events "\e[?u"))))  ; only CSI?u, no flags
    (should-not (kkp--this-terminal-supports-kkp-p))))

(ert-deftest kkp-test/strange-terminal--wrong-length-long ()
  "Mimic terminal that sends too many bytes (garbage or wrong protocol)."
  (cl-letf (((symbol-function 'kkp--query-terminal-sync)
             (lambda (_query) (kkp-test--events "\e[?0123u"))))  ; 8 bytes
    (should-not (kkp--this-terminal-supports-kkp-p))))

(ert-deftest kkp-test/strange-terminal--wrong-prefix ()
  "Mimic terminal that does not send CSI? (e.g. wrong escape sequence)."
  (cl-letf (((symbol-function 'kkp--query-terminal-sync)
             (lambda (_query) (kkp-test--events "\eZ?0u"))))  ; ESC Z ? not ESC [ ?
    (should-not (kkp--this-terminal-supports-kkp-p))))

(ert-deftest kkp-test/strange-terminal--wrong-terminator ()
  "Mimic terminal that does not end with 'u' (e.g. different protocol)."
  (cl-letf (((symbol-function 'kkp--query-terminal-sync)
             (lambda (_query) (kkp-test--events "\e[?0c"))))  ; ends with c not u
    (should-not (kkp--this-terminal-supports-kkp-p))))

(ert-deftest kkp-test/strange-terminal--garbage-bytes ()
  "Mimic terminal that sends random bytes before/after."
  (cl-letf (((symbol-function 'kkp--query-terminal-sync)
             (lambda (_query)
               (append (kkp-test--events "\0\001\002")  ; NUL SOH STX
                       (kkp-test--events "\e[?0u")
                       (list 255 254)))))  ; garbage tail
    (should-not (kkp--this-terminal-supports-kkp-p))))

(ert-deftest kkp-test/strange-terminal--valid-reply ()
  "Sanity: valid KKP reply is recognized."
  (cl-letf (((symbol-function 'kkp--query-terminal-sync)
             (lambda (_query) (kkp-test--events "\e[?01u"))))
    (should (kkp--this-terminal-supports-kkp-p)))
  (cl-letf (((symbol-function 'kkp--query-terminal-sync)
             (lambda (_query) (kkp-test--events "\e[?0u"))))
    (should (kkp--this-terminal-supports-kkp-p))))

;; ---------------------------------------------------------------------------
;; Slow SSH: no or delayed response within kkp-terminal-query-timeout
;; ---------------------------------------------------------------------------

(ert-deftest kkp-test/slow-ssh--no-reply-within-timeout ()
  "Mimic slow SSH: terminal does not respond before timeout (empty reply)."
  (cl-letf (((symbol-function 'kkp--query-terminal-sync) (lambda (_query) (list))))
    (should-not (kkp--this-terminal-supports-kkp-p))))

(ert-deftest kkp-test/slow-ssh--enabled-enhancements-errors-on-no-reply ()
  "Mimic slow SSH: query returns nil, enabled-enhancements should error."
  (cl-letf (((symbol-function 'kkp--query-terminal-sync) (lambda (_query) nil)))
    (should-error (kkp--this-terminal-enabled-enhancements)
                  :type 'error)))

(ert-deftest kkp-test/slow-ssh--partial-reply ()
  "Mimic slow SSH: terminal sends only part of reply before timeout (e.g. CSI? only)."
  (cl-letf (((symbol-function 'kkp--query-terminal-sync)
             (lambda (_query) (kkp-test--events "\e[?"))))  ; only CSI?, no flags nor u
    (should-not (kkp--this-terminal-supports-kkp-p))))

;; ---------------------------------------------------------------------------
;; Strange terminal: malformed or unexpected input to key translation
;; ---------------------------------------------------------------------------

(ert-deftest kkp-test/strange-terminal--translate-empty-input ()
  "Mimic terminal sending empty sequence to translator."
  (should-not (kkp--translate-terminal-input (list))))

(ert-deftest kkp-test/strange-terminal--translate-unknown-terminator ()
  "Mimic terminal sending sequence with non-KKP terminator."
  (should-not (kkp--translate-terminal-input (kkp-test--events "1;1X"))))  ; X not in u~ or letter

(ert-deftest kkp-test/strange-terminal--translate-u-minimal-valid ()
  "Minimal valid CSI-u sequence: key 'a', no modifier, terminator u."
  (let ((result (kkp--translate-terminal-input (kkp-test--events "au"))))
    (should result)
    ;; kbd can return a key sequence (vector) or string for simple keys
    (should (or (vectorp result) (stringp result)))))

(ert-deftest kkp-test/strange-terminal--translate-u-with-modifier ()
  "Valid CSI-u with modifier: a;2u (key a, shift)."
  (let ((result (kkp--translate-terminal-input (kkp-test--events "a;2u"))))
    (should result)
    (should (vectorp result))))

(ert-deftest kkp-test/strange-terminal--translate-malformed-modifier ()
  "Mimic terminal sending non-numeric modifier (should not crash)."
  (let ((result (kkp--translate-terminal-input (kkp-test--events "a;xu"))))
    (should result)
    (should (vectorp result))))

(ert-deftest kkp-test/strange-terminal--translate-letter-terminator ()
  "Valid letter terminator: up arrow CSI A."
  (let ((result (kkp--translate-terminal-input (kkp-test--events "A"))))
    (should result)
    (should (vectorp result))))

;; ---------------------------------------------------------------------------
;; Legacy-key encoding around call-process (C-g abort fix, issue #28)
;; ---------------------------------------------------------------------------

(defmacro kkp-test--capture-terminal-output (&rest body)
  "Run BODY with a fake KKP-active terminal; return the list of terminal writes.
Stubs terminal I/O so nothing real is touched, and pretends the selected
terminal is in `kkp--active-terminal-list' so `kkp-with-legacy-keys' engages."
  (declare (indent 0) (debug t))
  `(let ((out nil)
         (kkp--legacy-keys-terminals nil)
         (kkp--active-terminal-list (list 'fake-term)))
     (cl-letf (((symbol-function 'kkp--selected-terminal) (lambda () 'fake-term))
               ((symbol-function 'terminal-live-p) (lambda (_) t))
               ((symbol-function 'send-string-to-terminal)
                (lambda (s &optional _terminal) (push s out))))
       ,@body)
     (nreverse out)))

(ert-deftest kkp-test/legacy-keys--brackets-body-when-active ()
  "`kkp-with-legacy-keys' pushes flags 0 and pops around the body."
  (should (equal (kkp-test--capture-terminal-output
                   (kkp-with-legacy-keys (ignore)))
                 (list (kkp--csi-escape ">0u")
                       (kkp--csi-escape "<u")))))

(ert-deftest kkp-test/legacy-keys--pops-on-non-local-exit ()
  "The encoding is restored even when the body signals."
  (should (equal (kkp-test--capture-terminal-output
                   (ignore-errors (kkp-with-legacy-keys (error "boom"))))
                 (list (kkp--csi-escape ">0u")
                       (kkp--csi-escape "<u")))))

(ert-deftest kkp-test/legacy-keys--nested-toggles-once ()
  "Nested `kkp-with-legacy-keys' forms toggle the terminal only once."
  (should (equal (kkp-test--capture-terminal-output
                   (kkp-with-legacy-keys
                     (kkp-with-legacy-keys (ignore))))
                 (list (kkp--csi-escape ">0u")
                       (kkp--csi-escape "<u")))))

(ert-deftest kkp-test/legacy-keys--noop-when-inactive ()
  "No terminal writes happen when KKP is not active in the terminal."
  (let ((out nil)
        (kkp--legacy-keys-terminals nil)
        (kkp--active-terminal-list nil))  ; terminal not active
    (cl-letf (((symbol-function 'kkp--selected-terminal) (lambda () 'fake-term))
              ((symbol-function 'send-string-to-terminal)
               (lambda (s &optional _terminal) (push s out))))
      (kkp-with-legacy-keys (ignore)))
    (should-not out)))

(ert-deftest kkp-test/legacy-keys--keyed-per-terminal ()
  "Toggling is keyed per terminal, not by a global flag."
  (cl-letf (((symbol-function 'kkp--selected-terminal) (lambda () 'fake-term))
            ((symbol-function 'terminal-live-p) (lambda (_) t)))
    ;; Another terminal already in legacy mode must not suppress this one.
    (let ((out nil)
          (kkp--legacy-keys-terminals (list 'other-term))
          (kkp--active-terminal-list (list 'fake-term)))
      (cl-letf (((symbol-function 'send-string-to-terminal)
                 (lambda (s &optional _terminal) (push s out))))
        (kkp-with-legacy-keys (ignore)))
      (should (equal (nreverse out)
                     (list (kkp--csi-escape ">0u") (kkp--csi-escape "<u")))))
    ;; This terminal already in legacy mode suppresses re-toggling.
    (let ((out nil)
          (kkp--legacy-keys-terminals (list 'fake-term))
          (kkp--active-terminal-list (list 'fake-term)))
      (cl-letf (((symbol-function 'send-string-to-terminal)
                 (lambda (s &optional _terminal) (push s out))))
        (kkp-with-legacy-keys (ignore)))
      (should-not out))))

(ert-deftest kkp-test/legacy-keys--multiple-terminals ()
  "Across two live terminals, each is toggled and balanced independently.
Nesting for a *different* terminal inside the body toggles that terminal
\(not suppressed by a global flag); nesting for the *same* terminal does not
re-toggle.  Writes are captured per terminal to check both the byte and the
target terminal."
  (let ((selected 'term-a)
        (writes nil)                    ; reversed list of (TERMINAL . STRING)
        (kkp--legacy-keys-terminals nil)
        (kkp--active-terminal-list (list 'term-a 'term-b)))
    (cl-letf (((symbol-function 'kkp--selected-terminal) (lambda () selected))
              ((symbol-function 'terminal-live-p) (lambda (_) t))
              ((symbol-function 'send-string-to-terminal)
               (lambda (s &optional terminal) (push (cons terminal s) writes))))
      (kkp-with-legacy-keys             ; toggles term-a
        (kkp-with-legacy-keys (ignore)) ; same terminal -> no re-toggle
        (setq selected 'term-b)
        (kkp-with-legacy-keys (ignore)) ; different terminal -> toggles term-b
        (setq selected 'term-a)))
    (should (equal (nreverse writes)
                   (list (cons 'term-a (kkp--csi-escape ">0u"))
                         (cons 'term-b (kkp--csi-escape ">0u"))
                         (cons 'term-b (kkp--csi-escape "<u"))
                         (cons 'term-a (kkp--csi-escape "<u")))))))

(ert-deftest kkp-test/restore-legacy-keys--brackets-the-call ()
  "`kkp-restore-legacy-keys' (the public advice) brackets ORIG-FUN when active.
It does not consult any defcustom; gating happens at the advice-install site."
  (should (equal (kkp-test--capture-terminal-output
                   (kkp-restore-legacy-keys (lambda (&rest _) 0) "true"))
                 (list (kkp--csi-escape ">0u")
                       (kkp--csi-escape "<u")))))

(ert-deftest kkp-test/restore-legacy-keys--nested-advice-toggles-once ()
  "Stacking the advice (e.g. process-file delegating to call-process) toggles once.
The inner call must see the legacy switch already in effect and not re-toggle."
  (cl-labels ((inner (&rest _) 0)
              (outer (&rest _)
                (kkp-restore-legacy-keys #'inner "true")))
    (should (equal (kkp-test--capture-terminal-output
                     (kkp-restore-legacy-keys #'outer "true"))
                   (list (kkp--csi-escape ">0u")
                         (kkp--csi-escape "<u"))))))

(provide 'kkp-tests)
;;; kkp-tests.el ends here
