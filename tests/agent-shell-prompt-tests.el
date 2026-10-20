;;; agent-shell-prompt-tests.el --- Tests for agent-shell-prompt -*- lexical-binding: t; -*-

(require 'ert)
(require 'cl-lib)
(require 'seq)
(require 'agent-shell-faces)
(require 'agent-shell-prompt)

;;; Code:

(defvar agent-shell-chat-mode)

(defmacro agent-shell-prompt-tests--with-shell (&rest body)
  "Run BODY in a temp shell buffer whose prompt is live and busy."
  (declare (indent 0))
  `(with-temp-buffer
     (setq-local agent-shell-chat-mode nil)
     (setq-local shell-maker--busy t)
     ,@body))

(defun agent-shell-prompt-tests--prompt (&optional text)
  "Insert a prompt displaying TEXT and make it the live prompt.
Returns its start position."
  (let ((start (point)))
    (insert (or text "Claude> "))
    (setq comint-last-prompt
          (cons (copy-marker start) (copy-marker (point))))
    start))

(defun agent-shell-prompt-tests--busy-overlay ()
  "Return the buffer's busy prompt overlay, or nil."
  agent-shell--prompt-busy-overlay)

(ert-deftest agent-shell-prompt-busy-face-only-test ()
  "With no busy string, only the face changes and the prompt text stays."
  (agent-shell-prompt-tests--with-shell
    (let ((start (agent-shell-prompt-tests--prompt)))
      (set-buffer-modified-p nil)
      (let ((agent-shell-prompt-busy-display nil))
        (agent-shell--refresh-busy-prompt))
      (let ((overlay (agent-shell-prompt-tests--busy-overlay)))
        (should overlay)
        (should (= (overlay-start overlay) start))
        (should (= (overlay-end overlay) (point-max)))
        (should (eq (overlay-get overlay 'face) 'agent-shell-prompt-busy))
        (should-not (overlay-get overlay 'display))
        (should (equal (buffer-string) "Claude> "))
        (should-not (buffer-modified-p))))))

(ert-deftest agent-shell-prompt-busy-string-only-test ()
  "A busy string replaces the live prompt's display, not its buffer text."
  (agent-shell-prompt-tests--with-shell
    (agent-shell-prompt-tests--prompt)
    (let ((agent-shell-prompt-busy-display "...working..."))
      (agent-shell--refresh-busy-prompt))
    (let ((display (overlay-get (agent-shell-prompt-tests--busy-overlay)
                                'display)))
      (should (equal (substring-no-properties display) "...working..."))
      (should (eq (get-text-property 0 'face display)
                  'agent-shell-prompt-busy))
    (should (equal (buffer-string) "Claude> ")))))

(ert-deftest agent-shell-prompt-busy-face-and-string-test ()
  "Face and string work together."
  (agent-shell-prompt-tests--with-shell
    (agent-shell-prompt-tests--prompt)
    (let ((agent-shell-prompt-busy-display "busy"))
      (agent-shell--refresh-busy-prompt))
    (let ((overlay (agent-shell-prompt-tests--busy-overlay)))
      (should (eq (overlay-get overlay 'face) 'agent-shell-prompt-busy))
      (should (equal (substring-no-properties (overlay-get overlay 'display))
                     "busy")))))

(ert-deftest agent-shell-prompt-busy-function-transforms-prompts-test ()
  "A function transforms each agent's own prompt."
  (agent-shell-prompt-tests--with-shell
    (let ((agent-shell-prompt-busy-display
           (lambda (prompt)
             (concat (string-remove-suffix "> " prompt) "...> "))))
      (dolist (prompt '("Pi> " "Claude> " "Codex> "))
        (erase-buffer)
        (setq comint-last-prompt nil)
        (agent-shell-prompt-tests--prompt prompt)
        (agent-shell--refresh-busy-prompt)
        (should (equal (substring-no-properties
                        (overlay-get (agent-shell-prompt-tests--busy-overlay)
                                     'display))
                       (concat (string-remove-suffix "> " prompt) "...> ")))
        (should (equal (buffer-string) prompt))))))

(ert-deftest agent-shell-prompt-busy-function-gets-printed-prompt-test ()
  "A function receives the prompt as printed, not as previously displayed."
  (agent-shell-prompt-tests--with-shell
    (let ((start (agent-shell-prompt-tests--prompt)))
      (add-text-properties start (point-max)
                           '(face agent-shell-prompt field output))
      (let ((agent-shell-prompt-busy-display "busy"))
        (agent-shell--refresh-busy-prompt))
      (should (equal (substring-no-properties
                      (overlay-get (agent-shell-prompt-tests--busy-overlay)
                                   'display))
                     "busy"))
      (let* ((seen nil)
             (agent-shell-prompt-busy-display
              (lambda (prompt)
                (setq seen prompt)
                nil)))
        (agent-shell--refresh-busy-prompt)
        (should (equal seen "Claude> "))
        (should-not (text-properties-at 0 seen))))))

(ert-deftest agent-shell-prompt-busy-function-nil-result-test ()
  "A nil return keeps the actual prompt text and applies only the face."
  (agent-shell-prompt-tests--with-shell
    (agent-shell-prompt-tests--prompt)
    (let ((agent-shell-prompt-busy-display (lambda (_prompt) nil)))
      (agent-shell--refresh-busy-prompt))
    (let ((overlay (agent-shell-prompt-tests--busy-overlay)))
      (should (eq (overlay-get overlay 'face) 'agent-shell-prompt-busy))
      (should-not (overlay-get overlay 'display))
      (should (equal (buffer-string) "Claude> ")))))

(ert-deftest agent-shell-prompt-busy-function-result-not-mutated-test ()
  "The busy face goes on a copy, leaving the returned string untouched."
  (agent-shell-prompt-tests--with-shell
    (agent-shell-prompt-tests--prompt)
    (let* ((returned "Pi...> ")
           (agent-shell-prompt-busy-display (lambda (_prompt) returned)))
      (agent-shell--refresh-busy-prompt)
      (let ((display (overlay-get (agent-shell-prompt-tests--busy-overlay)
                                  'display)))
        (should (equal display returned))
        (should (eq (get-text-property 0 'face display)
                    'agent-shell-prompt-busy)))
      (should (equal returned "Pi...> "))
      (should-not (text-properties-at 0 returned)))))

(ert-deftest agent-shell-prompt-busy-function-invalid-result-test ()
  "A function returning neither a string nor nil signals."
  (agent-shell-prompt-tests--with-shell
    (agent-shell-prompt-tests--prompt)
    (let ((agent-shell-prompt-busy-display (lambda (_prompt) 42)))
      (should-error (agent-shell--refresh-busy-prompt)))
    (should-not (agent-shell-prompt-tests--busy-overlay))))

(ert-deftest agent-shell-prompt-busy-invalid-value-test ()
  "An option value that is neither nil, a string nor a function signals."
  (agent-shell-prompt-tests--with-shell
    (agent-shell-prompt-tests--prompt)
    (let ((agent-shell-prompt-busy-display 'busy))
      (should-error (agent-shell--refresh-busy-prompt)))))

(ert-deftest agent-shell-prompt-busy-empty-string-test ()
  "An empty busy string hides the prompt rather than keeping its text."
  (agent-shell-prompt-tests--with-shell
    (agent-shell-prompt-tests--prompt)
    (let ((agent-shell-prompt-busy-display ""))
      (agent-shell--refresh-busy-prompt))
    (should (equal (overlay-get (agent-shell-prompt-tests--busy-overlay)
                                'display)
                   ""))))

(ert-deftest agent-shell-prompt-busy-keeps-typed-input-test ()
  "The replacement covers the prompt only, leaving typed input alone."
  (agent-shell-prompt-tests--with-shell
    (agent-shell-prompt-tests--prompt)
    (let ((input-start (point)))
      (insert "half-typed")
      (let ((agent-shell-prompt-busy-display "busy"))
        (agent-shell--refresh-busy-prompt))
      (should (= (overlay-end (agent-shell-prompt-tests--busy-overlay))
                 input-start))
      (should (equal (buffer-substring input-start (point-max)) "half-typed"))
      (should (equal (buffer-string) "Claude> half-typed")))))

(ert-deftest agent-shell-prompt-busy-follows-output-above-prompt-test ()
  "Output rendered above the prompt does not fall inside the busy display."
  (agent-shell-prompt-tests--with-shell
    (let ((agent-shell-prompt-busy-display "[busy]> "))
      (agent-shell-prompt-tests--prompt)
      (agent-shell--refresh-busy-prompt)
      (let ((overlay (agent-shell-prompt-tests--busy-overlay)))
        (should overlay)
        (agent-shell--with-buffer-narrowed-to (agent-shell--live-prompt-start)
          (insert "streamed answer\n"))
        ;; The prompt is now at 17 . 25, past the inserted output.  The
        ;; overlay followed it instead of staying at 1 . 25 and hiding it.
        (should (equal (cons (marker-position (car comint-last-prompt))
                             (marker-position (cdr comint-last-prompt)))
                       '(17 . 25)))
        (should (equal (cons (overlay-start overlay) (overlay-end overlay))
                       '(17 . 25)))
        ;; The busy display covers the prompt only, leaving the answer visible.
        (should (equal (buffer-substring-no-properties
                        (point-min) (overlay-start overlay))
                       "streamed answer\n"))
        ;; Typing at the prompt does not extend the overlay over the input.
        (let ((input-start (point-max)))
          (goto-char (point-max))
          (insert "typed")
          (should (= (overlay-end overlay) input-start))
          (should (equal (buffer-substring-no-properties input-start (point-max))
                         "typed"))
          (should (equal (buffer-string) "streamed answer\nClaude> typed")))))))

(ert-deftest agent-shell-prompt-busy-ignores-metacharacters-test ()
  "Busy strings and prompts are displayed literally, not as regexps."
  (agent-shell-prompt-tests--with-shell
    (agent-shell-prompt-tests--prompt "Agent[1]> ")
    (let ((agent-shell-prompt-busy-display ".*\\1 [busy]$"))
      (agent-shell--refresh-busy-prompt))
    (should (equal (substring-no-properties
                    (overlay-get (agent-shell-prompt-tests--busy-overlay)
                                 'display))
                   ".*\\1 [busy]$"))
    (should (equal (buffer-string) "Agent[1]> "))))

(ert-deftest agent-shell-prompt-busy-then-idle-test ()
  "Finishing a turn removes the busy appearance and restores the prompt."
  (agent-shell-prompt-tests--with-shell
    (add-hook 'shell-maker-finish-output-hook
              #'agent-shell--refresh-busy-prompt nil t)
    (agent-shell-prompt-tests--prompt)
    (let ((agent-shell-prompt-busy-display "busy"))
      (agent-shell--refresh-busy-prompt))
    (should (agent-shell-prompt-tests--busy-overlay))
    (setq shell-maker--busy nil)
    (agent-shell--finish-output :config nil :success nil)
    (should-not (agent-shell-prompt-tests--busy-overlay))
    (should (equal (buffer-string) "Claude> "))))

(ert-deftest agent-shell-prompt-busy-reprinted-prompt-test ()
  "Reprinting the prompt while busy moves the appearance to the new one."
  (agent-shell-prompt-tests--with-shell
    (let ((old-start (agent-shell-prompt-tests--prompt "Old> "))
          (agent-shell-prompt-busy-display "busy"))
      (agent-shell--refresh-busy-prompt)
      (let ((overlay (agent-shell-prompt-tests--busy-overlay)))
        (should (= (overlay-start overlay) old-start))
        (cl-letf (((symbol-function 'shell-maker--process) #'ignore)
                  ((symbol-function 'shell-maker-prompt)
                   (lambda (_config) "New> "))
                  ((symbol-function 'shell-maker--output-filter)
                   (lambda (_process string)
                     (let ((start (point)))
                       (insert string)
                       (setq comint-last-prompt
                             (cons (copy-marker start)
                                   (copy-marker (point))))))))
          (agent-shell--print-prompt))
        (should (eq overlay (agent-shell-prompt-tests--busy-overlay)))
        (should (= (overlay-start overlay)
                   (marker-position (car comint-last-prompt))))
        (should (= (overlay-end overlay) (point-max)))
        (should (equal (buffer-string) "Old> New> "))
        ;; The moved overlay still follows the live prompt and leaves output
        ;; above it and input below it outside the busy display.
        (agent-shell--with-buffer-narrowed-to (agent-shell--live-prompt-start)
          (insert "answer\n"))
        (should (= (overlay-start overlay)
                   (marker-position (car comint-last-prompt))))
        (should (= (overlay-end overlay)
                   (marker-position (cdr comint-last-prompt))))
        (should (equal (buffer-substring-no-properties
                        (point-min) (overlay-start overlay))
                       "Old> answer\n"))
        (let ((input-start (point-max)))
          (goto-char (point-max))
          (insert "typed")
          (should (= (overlay-end overlay) input-start))
          (should (equal (buffer-substring-no-properties input-start (point-max))
                         "typed")))))))

(ert-deftest agent-shell-prompt-busy-chat-mode-test ()
  "Chat mode removes the appearance; turning it off restores it."
  (agent-shell-prompt-tests--with-shell
    (agent-shell-prompt-tests--prompt)
    (let ((agent-shell-prompt-busy-display "busy"))
      (agent-shell--refresh-busy-prompt)
      (should (agent-shell-prompt-tests--busy-overlay))
      (setq agent-shell-chat-mode t)
      (agent-shell--refresh-busy-prompt)
      (should-not (agent-shell-prompt-tests--busy-overlay))
      (setq agent-shell-chat-mode nil)
      (agent-shell--refresh-busy-prompt)
      (should (agent-shell-prompt-tests--busy-overlay)))))

(ert-deftest agent-shell-prompt-busy-without-live-prompt-test ()
  "No busy appearance without a live prompt, as when prompts are disabled."
  (agent-shell-prompt-tests--with-shell
    (let ((agent-shell-prompt-busy-display "busy"))
      ;; No prompt at all.
      (setq comint-last-prompt nil)
      (agent-shell--refresh-busy-prompt)
      (should-not (agent-shell-prompt-tests--busy-overlay))
      ;; A zero-length prompt (a cleared buffer).
      (insert "Claude> ")
      (setq comint-last-prompt
            (cons (copy-marker 1) (copy-marker 1)))
      (agent-shell--refresh-busy-prompt)
      (should-not (agent-shell-prompt-tests--busy-overlay))
      ;; A stale prompt with agent output below it.
      (goto-char (point-max))
      (insert "\nanswer")
      (add-text-properties 9 (point-max) '(field output))
      (setq comint-last-prompt
            (cons (copy-marker 1) (copy-marker 9)))
      (agent-shell--refresh-busy-prompt)
      (should-not (agent-shell-prompt-tests--busy-overlay)))))

(ert-deftest agent-shell-prompt-busy-historical-prompts-test ()
  "Only the live prompt is styled; earlier prompts keep their own look."
  (agent-shell-prompt-tests--with-shell
    (let ((first-start (agent-shell-prompt-tests--prompt "First> "))
          (agent-shell-prompt-busy-display "busy"))
      (let ((first-end (point)))
        (insert "answer\n")
        (add-text-properties first-end (point-max) '(field output)))
      (let ((live-start (agent-shell-prompt-tests--prompt "Live> ")))
        (agent-shell--refresh-busy-prompt)
        (let ((overlay (agent-shell-prompt-tests--busy-overlay)))
          (should (= (overlay-start overlay) live-start))
          (should (= (point-min) first-start))
          (should-not (seq-find (lambda (candidate)
                                  (eq (overlay-get candidate 'face)
                                      'agent-shell-prompt-busy))
                                (overlays-at first-start))))))))

(ert-deftest agent-shell-prompt-busy-cleared-buffer-test ()
  "Clearing the buffer drops the appearance without touching markers."
  (agent-shell-prompt-tests--with-shell
    (agent-shell-prompt-tests--prompt)
    (let ((agent-shell-prompt-busy-display "busy"))
      (agent-shell--refresh-busy-prompt))
    (should (agent-shell-prompt-tests--busy-overlay))
    (erase-buffer)
    (agent-shell--refresh-busy-prompt)
    (should-not (agent-shell-prompt-tests--busy-overlay))))

(ert-deftest agent-shell-prompt-busy-clear-on-cleanup-test ()
  "Buffer cleanup removes the overlay and forgets it."
  (agent-shell-prompt-tests--with-shell
    (agent-shell-prompt-tests--prompt)
    (let ((agent-shell-prompt-busy-display "busy"))
      (agent-shell--refresh-busy-prompt))
    (should (agent-shell-prompt-tests--busy-overlay))
    (agent-shell--clear-busy-prompt)
    (should-not (agent-shell-prompt-tests--busy-overlay))
    (should-not (seq-find (lambda (overlay)
                            (eq (overlay-get overlay 'face)
                                'agent-shell-prompt-busy))
                          (overlays-in (point-min) (point-max))))))

(provide 'agent-shell-prompt-tests)

;;; agent-shell-prompt-tests.el ends here
