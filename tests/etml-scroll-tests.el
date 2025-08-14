(require 'etml-test)

(defvar etml-test-scroll-block
  (etml-block
   :content (string-trim-right
             (file-content "./text-zh.txt") "\n")
   :border t
   :height 30
   :width '(400)
   :margin '(2 . 1)
   :padding '(2 . 1)))

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
