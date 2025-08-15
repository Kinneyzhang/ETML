(require 'etml-test)
(require 'etml-block)

(defvar etml-test-scroll-block
  (etml-block
   :content (string-trim-right
             (file-content
              ;; "./text-zh-en_US.txt"
              "./text-zh.txt"
              )
             "\n")
   :border nil
   :height 26
   :width '(400)
   :margin '(2 . 1)
   :padding '(2 . 1)
   :scroll-bar-full nil
   :scroll-bar-gap 2
   ))

(defun etml-test-scroll-down ()
  (interactive)
  (let ((block etml-test-scroll-block))
    (oset block scroll-offset-y
          (1+ (oref block scroll-offset-y)))
    (erase-buffer)
    (etml-test-scroll-render)))

(with-current-buffer "*etml-test-scroll*"
  (let (start end)
    (erase-buffer)
    (setq start (point))
    (etml-test-scroll-render)
    (setq end (point))
    (let ((map (make-sparse-keymap)))
      (define-key map "n" #'etml-block-scroll-down)
      (add-text-properties start end `(keymap ,map)))
    (goto-char (point-min))))

(defun etml-block-scroll-down ()
  (interactive)
  (save-excursion
    (let* ((props (text-properties-at (point)))
           (uuid (plist-get props 'etml-block-line))
           (offset (plist-get props 'etml-block-y-offset))
           (lines (gethash uuid etml-block-caches))
           (idx offset))
      (goto-char (point-min))
      (while-let ((match (text-property-search-forward
                          'etml-block-line uuid t))
                  (line (nth (1+ idx) lines)))
        (setq line (propertize
                    line 'etml-block-line uuid
                    'etml-block-y-offset (1+ offset)))
        (delete-region
         (prop-match-beginning match)
         (prop-match-end match))
        (goto-char (prop-match-beginning match))
        (insert line)
        (cl-incf idx 1)))))

(defun etml-test-max-offset ()
  (- (etml-block-content-height etml-test-scroll-block)
     (oref etml-test-scroll-block height)))

(defun etml-test-scroll-render ()
  (insert (etml-block-render etml-test-scroll-block)))

(defun etml-test-scroll ()
  (interactive)
  (etml-test-pop-buffer "*etml-test-scroll*"
    (dotimes (i (1+ (etml-test-max-offset)))
      (erase-buffer)
      (oset etml-test-scroll-block scroll-offset-y i)
      (etml-test-scroll-render)
      (goto-char (point-min))
      (sit-for 0.1))))

(defun etml-test-scroll-reverse ()
  (interactive)
  (etml-test-pop-buffer "*etml-test-scroll*"
    (dotimes (i (1+ (etml-test-max-offset)))
      (erase-buffer)
      (oset etml-test-scroll-block scroll-offset-y (- (etml-test-max-offset) i))
      (etml-test-scroll-render)
      (goto-char (point-min))
      (sit-for 0.05))))

;; (etml-block-content-height etml-test-scroll-block);; 62
;; (etml-test-scroll-n)
;; (setq etml-block-scroll-bar-full-p t
;;       etml-block-scroll-bar-pixel 2)
;; (setq etml-block-scroll-bar-gap-pixel 1)
;; (setq etml-block-scroll-bar-color '("#111" . "#fff"))
;; (setq etml-block-scroll-bar-direction 'right)
