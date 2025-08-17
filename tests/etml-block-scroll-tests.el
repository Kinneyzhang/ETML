(require 'etml-test)
(require 'etml-block)

(defvar etml-test-scroll-block nil)

(setq etml-test-scroll-block
      (etml-block
       :content (string-trim-right
                 (file-content
                  ;; "./text-zh-en_US.txt"
                  "./text-zh.txt"
                  )
                 "\n")
       :border t
       :height 20
       :width '(400)
       :margin '(1 . 1)
       :padding '(2 . 1)
       :overflow 'scroll
       :scroll-offset-y 14
       :scroll-bar-full t
       :scroll-bar-direction 'right
       :scroll-bar-color "grey"
       :scroll-bar-pixel 2
       :scroll-bar-gap 2
       ))

(defun etml-set-cursor ()
  "Set cursor type correctly in current buffer."
  (interactive)
  (run-with-idle-timer
   0.001 nil (lambda () (setq-local cursor-type t))))

(defun etml-block-scroll-render (buffer)
  (with-current-buffer buffer
    (kill-all-local-variables)
    (etml-block-caches-init)
    (let ((inhibit-read-only t)
          start end)
      (erase-buffer)
      (buffer-disable-undo)
      (setq-local text-scale-mode-step 0)
      (insert (etml-block-render
               etml-test-scroll-block))
      (auto-modal-mode -1)
      (etml-set-cursor)
      (read-only-mode 1))))

(defun etml-block-scroll-show (&optional force)
  (interactive)
  (let ((buffer (get-buffer-create "*etml-scroll-tests*")))
    (if-let ((win (get-buffer-window buffer)))
        (progn
          (select-window win)
          (when force
            (etml-block-scroll-render buffer)))
      (switch-to-buffer buffer)
      (etml-block-scroll-render buffer))))

(etml-block-scroll-show t)

(provide 'etml-block-scroll-tests)
