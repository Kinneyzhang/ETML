(defun etml-test-quit-window ()
  (interactive)
  (let ((map (current-local-map)))
    (quit-window)
    (unbind-key "q" map)))

(defmacro etml-test-pop-buffer (buffer-name &rest body)
  (declare (indent defun))
  `(let ((buffer (get-buffer-create ,buffer-name)))
     (delete-other-windows)
     (switch-to-buffer buffer)
     (setq etml-demo-window-pixel (window-pixel-width))
     (with-current-buffer buffer
       (local-set-key "q" 'etml-test-quit-window)
       (erase-buffer)
       ,@body)))

(provide 'etml-test)
