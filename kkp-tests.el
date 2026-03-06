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

(provide 'kkp-tests)
;;; kkp-tests.el ends here
