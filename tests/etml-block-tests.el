(pop-buffer-insert 20
  (let ((block (etml-block
                :content "happyhackingemacs;happyhackingclaudecode;happyhackingvscode;happyhackingcursor;happyhackingvim.happyhackingemacs;happyhackingclaudecode;happyhackingvscode;happyhackingcursor;happyhackingvim."
                ;; :border '( :left t :top t
                ;;            :right ( :type scroll
                ;;                     :width 2)
                ;;            :bottom (:type scroll))
                :border '(:right (:type scroll :width 1))
                :height 4
                :width 30
                ;; :margin '(1 . 1)
                :padding '(1 . 1)
                )))
    (oset block scroll-offset
          '(0 . 2))
    (etml-block-render block)))

(etml-block-total-curr-pixel
 ;;etml-block-total-min-pixel
 ;;etml-block-total-max-pixel
 ;;etml-block-total-curr-pixel
 ;;etml-block-total-pixel
 (etml-block
  :content "happy hacking emacs;happy hacking claude code;happy hacking vscode;happy hacking cursor;happy hacking vim.
happy hacking emacs;happy hacking claude code;happy hacking vscode;happy hacking cursor;happy hacking vim."
  :border t
  :height 5
  :width 10))

(pop-buffer-insert 20
  (etml-lines-concat
   (list
    "emacs "
    (etml-pixel-border 2 1)
    (etml-pixel-border 1 4)))
  "\n"
  (etml-lines-concat
   (list
    "emacs "
    (etml-pixel-border 1 4)
    (etml-pixel-border 1 1)
    (etml-pixel-border 1 4))))

;; scroll-bar-height = block height - scroll num

(pop-buffer-insert 20
  (propertize
   "ddddddd" 'face
   '(:underline (:style  :position t))))

(pop-buffer-insert 20
  (propertize "ddd"
              'face '(:underline
                      (:style nil :position t))))

(pop-buffer-insert 10
  (etml-lines-concat
   (list "emacs "
         (etml-block-border 0 6 nil 'scroll 1 1))))

(etml-lines-concat (list scroll-string border-string))

;; 模拟水平 scroll bar: 组合 double-line 和 line

(pop-buffer-insert 20
  (etml-block-string
   :content ""
   ))
