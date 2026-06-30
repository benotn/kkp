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
Stubs terminal I/O so nothing real is touched, and gives the selected
terminal a `kkp--state' with enhancements active so `kkp-with-legacy-keys'
engages."
  (declare (indent 0) (debug t))
  `(let ((out nil)
         (states (list (cons 'fake-term (kkp--make-state :enhancements 1)))))
     (cl-letf (((symbol-function 'kkp--selected-terminal) (lambda () 'fake-term))
               ((symbol-function 'terminal-live-p) (lambda (_) t))
               ((symbol-function 'kkp--terminal-state)
                (lambda (term) (cdr (assq term states))))
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
  "No terminal writes happen when the terminal has no active `kkp--state'."
  (let ((out nil))
    (cl-letf (((symbol-function 'kkp--selected-terminal) (lambda () 'fake-term))
              ((symbol-function 'kkp--terminal-state) (lambda (_) nil))
              ((symbol-function 'send-string-to-terminal)
               (lambda (s &optional _terminal) (push s out))))
      (kkp-with-legacy-keys (ignore)))
    (should-not out)))

(ert-deftest kkp-test/legacy-keys--keyed-per-terminal ()
  "Toggling is keyed per terminal via its `kkp--state', not a global flag."
  (cl-letf (((symbol-function 'kkp--selected-terminal) (lambda () 'fake-term))
            ((symbol-function 'terminal-live-p) (lambda (_) t)))
    ;; Another terminal being in legacy mode must not suppress this one.
    (let* ((out nil)
           (states (list (cons 'fake-term (kkp--make-state :enhancements 1))
                         (cons 'other-term (kkp--make-state :enhancements 1
                                                            :legacy-active t)))))
      (cl-letf (((symbol-function 'kkp--terminal-state)
                 (lambda (term) (cdr (assq term states))))
                ((symbol-function 'send-string-to-terminal)
                 (lambda (s &optional _terminal) (push s out))))
        (kkp-with-legacy-keys (ignore)))
      (should (equal (nreverse out)
                     (list (kkp--csi-escape ">0u") (kkp--csi-escape "<u")))))
    ;; This terminal already in legacy mode suppresses re-toggling.
    (let* ((out nil)
           (states (list (cons 'fake-term (kkp--make-state :enhancements 1
                                                           :legacy-active t)))))
      (cl-letf (((symbol-function 'kkp--terminal-state)
                 (lambda (term) (cdr (assq term states))))
                ((symbol-function 'send-string-to-terminal)
                 (lambda (s &optional _terminal) (push s out))))
        (kkp-with-legacy-keys (ignore)))
      (should-not out))))

(ert-deftest kkp-test/legacy-keys--multiple-terminals ()
  "Across two live terminals, each is toggled and balanced independently.
Nesting for a *different* terminal inside the body toggles that terminal
\(its own `kkp--state'); nesting for the *same* terminal does not re-toggle.
Writes are captured per terminal to check both the byte and the target."
  (let* ((selected 'term-a)
         (writes nil)                   ; reversed list of (TERMINAL . STRING)
         (states (list (cons 'term-a (kkp--make-state :enhancements 1))
                       (cons 'term-b (kkp--make-state :enhancements 1)))))
    (cl-letf (((symbol-function 'kkp--selected-terminal) (lambda () selected))
              ((symbol-function 'terminal-live-p) (lambda (_) t))
              ((symbol-function 'kkp--terminal-state)
               (lambda (term) (cdr (assq term states))))
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

;; ---------------------------------------------------------------------------
;; Key-translation regressions for closed issues
;; ---------------------------------------------------------------------------

(ert-deftest kkp-test/translate-bracketed-paste ()
  "Issue #7: a 200~ sequence dispatches to `xterm-translate-bracketed-paste'."
  (let ((called nil))
    (cl-letf (((symbol-function 'xterm-translate-bracketed-paste)
               (lambda (&rest _) (setq called t) 'pasted)))
      (should (eq (kkp--translate-terminal-input (kkp-test--events "200~"))
                  'pasted))
      (should called))))

(ert-deftest kkp-test/translate-tab-vs-ctrl-i ()
  "Issue #19: the Tab key (keycode 9) and C-i (keycode 105 + ctrl) differ."
  (should (equal (kkp--translate-terminal-input (kkp-test--events "9u"))
                 (kbd "<tab>")))
  (should (equal (kkp--translate-terminal-input (kkp-test--events "105;5u"))
                 (kbd "C-i")))
  (should-not (equal (kbd "<tab>") (kbd "C-i"))))

(ert-deftest kkp-test/translate-ctrl-q ()
  "Issue #11: C-q (keycode 113 + ctrl) translates to the C-q key."
  (should (equal (kkp--translate-terminal-input (kkp-test--events "113;5u"))
                 (kbd "C-q"))))

(ert-deftest kkp-test/translate-delete-vs-backspace ()
  "Issue #6: the Delete key (CSI 3~) and Backspace (keycode 127) differ."
  (should (equal (kkp--translate-terminal-input (kkp-test--events "3~"))
                 (kbd "<delete>")))
  (should (equal (kkp--translate-terminal-input (kkp-test--events "127u"))
                 (kbd "<backspace>")))
  (should-not (equal (kbd "<delete>") (kbd "<backspace>"))))

(ert-deftest kkp-test/translate-meta-backspace ()
  "Issue #13: M-<backspace> (keycode 127 + alt) decodes with the meta modifier,
and `kkp-alternatives-map' remaps it to M-DEL."
  (should (equal (kkp--translate-terminal-input (kkp-test--events "127;3u"))
                 (kbd "M-<backspace>")))
  (should (equal (lookup-key kkp-alternatives-map [M-backspace]) [?\M-\d])))

;; ---------------------------------------------------------------------------
;; Terminal-lifecycle regressions for closed issues
;; ---------------------------------------------------------------------------

(ert-deftest kkp-test/terminal-teardown-restores-terminal ()
  "Issues #23/#10/#6: teardown emits <u, restores `normal-erase-is-backspace',
runs the teardown hook, and marks the terminal inactive."
  (let* ((out nil)
         (erase-restored 'unset)
         (hook-ran nil)
         (state (kkp--make-state :enhancements 5 :previous-normal-erase 'saved))
         (kkp-terminal-teardown-complete-hook (list (lambda () (setq hook-ran t)))))
    (cl-letf (((symbol-function 'terminal-live-p) (lambda (_) t))
              ((symbol-function 'kkp--terminal-state) (lambda (_) state))
              ((symbol-function 'kkp-teardown-function-keys) (lambda (_) nil))
              ((symbol-function 'frames-on-display-list)
               (lambda (_) (list (selected-frame))))
              ((symbol-function 'normal-erase-is-backspace-mode)
               (lambda (arg) (setq erase-restored arg)))
              ((symbol-function 'define-key) (lambda (&rest _) nil))
              ((symbol-function 'send-string-to-terminal)
               (lambda (s &optional _terminal) (push s out))))
      (kkp--terminal-teardown 'fake-term))
    (should (member (kkp--csi-escape "<u") out))
    (should (eq erase-restored 'saved))
    (should hook-ran)
    (should-not (kkp--state-enhancements state))))

(ert-deftest kkp-test/suspend-tears-down-and-marks-suspended ()
  "Issue #10: suspending an active terminal tears it down and marks it suspended."
  (let ((state (kkp--make-state :enhancements 5))
        (torn nil))
    (cl-letf (((symbol-function 'kkp--selected-terminal) (lambda () 'fake-term))
              ((symbol-function 'kkp--terminal-state) (lambda (_) state))
              ((symbol-function 'kkp--ensure-state) (lambda (_) state))
              ((symbol-function 'kkp--terminal-teardown) (lambda (_) (setq torn t))))
      (kkp--suspend-in-terminal 'fake-term))
    (should (kkp--state-suspended state))
    (should torn)))

(ert-deftest kkp-test/resume-re-enables-suspended-terminal ()
  "Issue #10: resuming a suspended terminal clears the flag and re-enables KKP."
  (let ((state (kkp--make-state :suspended t))
        (re-enabled nil))
    (cl-letf (((symbol-function 'kkp--selected-terminal) (lambda () 'fake-term))
              ((symbol-function 'kkp--terminal-state) (lambda (_) state))
              ((symbol-function 'kkp-enable-in-terminal)
               (lambda (&rest _) (setq re-enabled t))))
      (kkp--resume-in-terminal 'fake-term))
    (should-not (kkp--state-suspended state))
    (should re-enabled)))

(ert-deftest kkp-test/setup-runs-completion-hook ()
  "Issue #15: a matching setup reply runs `kkp-terminal-setup-complete-hook'
and marks the terminal active."
  (let* ((reply (kkp-test--events "1u\e[?62c"))  ; flags=1, then device attributes, c
         (i 0)
         (hook-ran nil)
         (state (kkp--make-state))
         (kkp-terminal-setup-complete-hook (list (lambda () (setq hook-ran t)))))
    (cl-letf (((symbol-function 'read-event)
               (lambda (&rest _) (prog1 (nth i reply) (setq i (1+ i)))))
              ((symbol-function 'kkp--selected-terminal) (lambda () 'fake-term))
              ((symbol-function 'kkp--terminal-state) (lambda (_) state))
              ((symbol-function 'kkp--ensure-state) (lambda (_) state))
              ((symbol-function 'send-string-to-terminal) (lambda (&rest _) nil))
              ((symbol-function 'kkp-setup-function-keys) (lambda (_) nil))
              ((symbol-function 'frames-on-display-list)
               (lambda (_) (list (selected-frame))))
              ((symbol-function 'normal-erase-is-backspace-mode) (lambda (&rest _) nil))
              ((symbol-function 'define-key) (lambda (&rest _) nil))
              ((symbol-function 'terminal-parameter) (lambda (&rest _) nil)))
      (kkp--terminal-setup))
    (should hook-ran)
    (should (kkp--state-enhancements state))))

(provide 'kkp-tests)
;;; kkp-tests.el ends here
