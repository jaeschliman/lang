(defvar lang-process nil)

(defun lang-start-process ()
  (interactive)
  (setq lang-process (open-network-stream "lang-listener"
                                          "*lang-listener*"
                                          "localhost"
                                          8080 )))

(defun lang-ensure-process ()
  (when (or (not lang-process) (eq 'closed (process-status lang-process)))
    (lang-start-process)))

(defun lang-flash-region (start end) ;; cribbed from slime
  (let ((overlay (make-overlay start end)))
    (overlay-put overlay 'face 'secondary-selection)
    (run-with-timer 0.2 nil 'delete-overlay overlay)))

(defun lang-get-string-to-send ()
  (if (region-active-p)
      (buffer-substring-no-properties (region-beginning) (region-end))
    (let ((range (bounds-of-thing-at-point 'defun)))
      (lang-flash-region (car range) (cdr range))
      (buffer-substring-no-properties (car range) (cdr range)))))

(defun lang-send-dwim ()
  (interactive)
  (lang-ensure-process)
  (let ((s (lang-get-string-to-send)))
    (process-send-string lang-process s)))

;; (global-set-key (kbd "s-<return>") 'lang-send-dwim)

