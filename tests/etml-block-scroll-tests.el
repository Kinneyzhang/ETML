(require 'etml-test)
(require 'etml-block)

(defface etml-mono-face
  '((t :font "LXGW WenKai Mono" :height 150))
  "")

;; (defface etml-eng-face
;;   '((t :font "Noto Serif" :height 120))
;;   "")

(defvar etml-test-scroll-str1
  (propertize
   (string-trim-right (org-file-contents "./text-zh.txt") "\n")
   'face 'etml-mono-face))

(defvar etml-test-scroll-str2
  (propertize
   (string-trim-right (org-file-contents "./text-zh-en_US.txt") "\n")
   'face 'etml-mono-face))

(defvar etml-test-scroll-str3
  (propertize
   (string-trim-right (org-file-contents "./text-en_US.txt") "\n")
   'face 'etml-mono-face))

;; (ekp-clear-caches)

(defmacro etml-block-scroll-render (buffer &rest body)
  (declare (indent defun))
  `(let ((buffer (get-buffer-create ,buffer)))
     (delete-other-windows)
     (switch-to-buffer buffer)
     (with-current-buffer buffer
       (kill-all-local-variables)
       (etml-block-caches-init)
       (let ((inhibit-read-only t))
         (local-set-key "q" 'etml-test-quit-window)
         (erase-buffer)
         (buffer-disable-undo)
         (setq-local text-scale-mode-step 0)
         ,@body
         (when (fboundp 'auto-modal-mode)
           (auto-modal-mode -1))
         (read-only-mode 1)))))

(etml-block-scroll-render "*test-block-scroll*"
  (insert
   (etml-block-render
    (etml-block-concat
     (etml-block-stack
      (etml-block :content etml-test-scroll-str2
                  :height 12 :width '(240)
                  :border t
                  ;; :bgcolor '("#FFF9F0" . "#222222")
                  :margin '(1 . 1) :padding '(1 . 1)
                  :scroll-bar-direction 'left
                  :scroll-bar-gap 4)
      (etml-block :content etml-test-scroll-str2
                  :height 24 :width '(240)
                  :border t
                  ;; :bgcolor '("#FFF9F0" . "#222222")
                  :margin '(1 . 0) :padding '(1 . 1)
                  :scroll-bar-direction 'right
                  :scroll-bar-full t
                  :scroll-bar-gap 2))
     (etml-block :content etml-test-scroll-str1
                 :height 39 :width '(350)
                 :border t
                 ;; :bgcolor '("#FFF9F0" . "#222222")
                 :margin '(1 . 1) :padding '(1 . 1)
                 :scroll-bar-full t
                 :scroll-bar-pixel 2
                 :scroll-bar-gap 0)
     (etml-block-stack
      (etml-block :content etml-test-scroll-str1
                  :height 14 :width '(550)
                  :border nil
                  :margin '(1 . 1) :padding '(1 . 1)
                  :scroll-bar-full t
                  :scroll-bar-pixel 8
                  :scroll-bar-gap 0)
      (etml-block :content etml-test-scroll-str3
                  :height 22 :width '(550)
                  :border '(:top t :bottom t)
                  :margin '(1 . 0) :padding '(4 . 1)
                  :scroll-bar-direction 'left
                  :scroll-bar-full nil
                  :scroll-bar-pixel 3
                  :scroll-bar-color "orange"
                  :scroll-bar-gap 4)))))
  (goto-char (point-min)))

(provide 'etml-block-scroll-tests)
