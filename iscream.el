(require 'ilisp)
(require 'bridge)
(provide 'emacs-eval)

(defvar *lisp-eval-buffer* "")
(defvar *backtrack-locations* nil)
(defvar *y-or-n-p-map* nil)

(add-hook
 'clisp-hook
 '(lambda ()
   (add-hook 'ilisp-init-hook '(lambda () (install-bridge)))))

(setq bridge-hook
      '(lambda ()
	(setq bridge-source-insert nil)
	(setq bridge-destination-insert nil)
	(setq bridge-handlers '((".*" . catch-lisp-eval)))))

(defun catch-lisp-eval (process string)
 (if string
     (setq *lisp-eval-buffer* (concat *lisp-eval-buffer* string))
     (let ((buffer (current-buffer))
	   (eval-string *lisp-eval-buffer*))
      (setq *lisp-eval-buffer* "")
      (condition-case err
	  (eval (read eval-string))
       (error (message "Bridge Error: %s on eval of: %s" err eval-string)))
      (set-buffer buffer))))

(defun y-or-n-p-begin ()
 (if (not *y-or-n-p-map*)
     (let ((map (make-keymap)))
      (fillarray map 'y-or-n-p-send-char)
      (define-key map "\C-g" 'interactive-keys-ilisp)
      (setq *y-or-n-p-map* map)))
 (use-local-map *y-or-n-p-map*))

(defun y-or-n-p-end ()
 (use-local-map ilisp-mode-map))

(defun y-or-n-p-send-char ()
 (interactive)
 (goto-char (point-max))
 (process-send-string (ilisp-process) (make-string 1 last-input-char)))

(defun push-end-marker ()
 (end-of-buffer)
 (setq *backtrack-locations* (cons (point) *backtrack-locations*)))

(defun pop-end-marker ()
 (if *backtrack-locations*
     (let ((location (car *backtrack-locations*)))
      (setq *backtrack-locations* (cdr *backtrack-locations*))
      (end-of-buffer)
      (delete-region location (point)))))
