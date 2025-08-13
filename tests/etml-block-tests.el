(require 'etml-test)

(defvar etml-test-scroll-block
  (etml-block
   :content (string-trim-right
             (file-content "./text-zh.txt") "\n")
   :border t
   :height 30
   :width 26
   :margin '(1 . 1)
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
      (oset etml-test-scroll-block
            scroll-offset (cons 0 i))
      (etml-test-scroll-render)
      (goto-char (point-min))
      (sit-for 0.1))))

(defun etml-test-scroll-reverse ()
  (interactive)
  (etml-test-pop-buffer "*etml-test-scroll*"
    (dotimes (i (1+ (etml-test-max-offset)))
      (erase-buffer)
      (oset etml-test-scroll-block
            scroll-offset (cons 0 (- (etml-test-max-offset) i)))
      (etml-test-scroll-render)
      (goto-char (point-min))
      (sit-for 0.05))))
