(defun etml-block-parse-total-pixel (block slot)
  (when-let ((width (eval `(oref ,block ,slot))))
    (cond
     ((consp width) (car width))
     ((numberp width)
      (let ((content (oref block content)))
        (+ (etml-block-side-pixel block)
           (etml-width-pixel width content))))
     (t (error "Invalid format of block width: %S" width)))))

(defun etml-block-side-pixel (block &optional side)
  "Return the side pixel width of BLOCK. Side pixel is the
total of margin, padding and border pixel.

Defautly count both the left and right side. If SIDE equals
to a symbol 'left, count the left side only; if SIDE equals
to a symbol 'right, count the right side only."
  (let* ((y-scroll-p (etml-block-y-scroll-p block))
         ;; FIXME: recursive dependency
         ;; etml-block-y-scroll-p => content-height
         ;; content-height => content-width
         ;; content-width = total width - side-width
         ;; this is side-width
         (left-border (etml-block-single-border block :left))
         (right-border (etml-block-single-border block :right))
         (left-margin-pixel (etml-block-margin block :left))
         (right-margin-pixel (etml-block-margin block :right))
         (left-padding-pixel (etml-block-padding block :left))
         (right-padding-pixel (etml-block-padding block :right))
         (left-side-pixel (+ left-margin-pixel left-padding-pixel))
         (right-side-pixel (+ right-margin-pixel
                              right-padding-pixel)))
    ;; when border is a scroll bar, add extra width
    (setq left-side-pixel
          (+ left-side-pixel
             ;; when type is non-nil,
             ;; add extra pixel of scroll bar
             (if-let* ((pixel (plist-get left-border :width))
                       ((> pixel 0)))
                 ;; has left border
                 (if (and y-scroll-p
                          (eq 'left etml-block-scroll-bar-direction))
                     (if etml-block-scroll-bar-border-full-p
                         (+ 1 pixel etml-block-scroll-bar-pixel)
                       (+ pixel etml-block-scroll-bar-pixel))
                   pixel)
               ;; no left border
               (if (and y-scroll-p
                        (eq 'left etml-block-scroll-bar-direction))
                   etml-block-scroll-bar-pixel
                 0))))
    (setq right-side-pixel
          (+ right-side-pixel
             ;; when type is non-nil,
             ;; add extra pixel of scroll bar
             (if-let* ((pixel (plist-get right-border :width))
                       ((> pixel 0)))
                 ;; has right border
                 (if (and y-scroll-p
                          (eq 'right etml-block-scroll-bar-direction))
                     (if etml-block-scroll-bar-border-full-p
                         (+ 1 pixel etml-block-scroll-bar-pixel)
                       (+ pixel etml-block-scroll-bar-pixel))
                   pixel)
               ;; no right border
               (if (and y-scroll-p
                        (eq 'right etml-block-scroll-bar-direction))
                   etml-block-scroll-bar-pixel
                 0))))
    (pcase side
      ((pred null) (+ left-side-pixel right-side-pixel))
      ('left left-side-pixel)
      ('right right-side-pixel)
      (_ (error "Invalid format of side %S" side)))))

(defun etml-block-concat (strings &optional align text-align)
  "TEXT-ALIGN should be one of left,center,right.
ALIGN should be one of top,center,bottom."
  (let ((height (-max (-map #'block-string-linum strings))))
    (apply 'block-string-concat
           (mapcar (lambda (string)
                     (etml-lines-justify
                      (etml-lines-align string height align)
                      (string-pixel-width string)
                      text-align))
                   strings))))

(defun etml-block-stack (strings &optional align text-align)
  "ALIGN used for all blocks, TEXT-ALIGN used for text in a block."
  (let ((max-width (-max (-map #'string-pixel-width strings))))
    (mapconcat (lambda (string)
                 (etml-lines-justify
                  (etml-lines-justify
                   string (string-pixel-width string) text-align)
                  max-width align))
               strings "\n")))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defvar block-bgcolor-change-hook nil)

(defun etml-block-bgcolor-change-function (orig-fun &rest args)
  "Advice functon when load a theme."
  (let ((before-background-mode
         (frame-parameter nil 'background-mode))
        after-background-mode)
    (apply orig-fun args)
    (setq after-background-mode
          (frame-parameter nil 'background-mode))
    (unless (eq before-background-mode after-background-mode)
      (run-hooks 'block-bgcolor-change-hook))))

(advice-add #'counsel-load-theme
            :around #'block-bgcolor-change-function)
(advice-add #'load-theme :around #'block-bgcolor-change-function)
(add-hook #'block-bgcolor-change-hook #'block-text-render1)

;; 示例调用 (假设字符宽度为10像素)
(pop-buffer-insert 20
  ;; 使用当前frame字体度量
  (progn
    (ekp-clear-caches)
    (ekp-pixel-init 6 2 2 4 2 2 0 1 0)
    (etml-pixel-wrap
     "Ni-ka Ford has always known that she wanted to be an artist. But she wasn’t sure how to channel that passion until her final year as a studio art major in college. She remembers one day in an art studio when she was looking out the window at a tree. “And I was like, ‘Wow, the branches really look like veins in the body,’” she says. This inspiration led her to notice “a lot of similarities between our bodies and nature” and drew her to the field of medical illustration. Today her work distills medical complexity into illustrations and graphics that appear in journal articles, teaching materials and popular publications." 577))
  ;; (knuth-plass-justify
  ;;  "在使用 Emacs 进行开发时，很多用户会依赖 Org-mode 来管理他们的任务和笔记。For instance, with Org-mode, you can easily organize your projects and even export your notes to various formats like HTML or PDF. 此外，Emacs 的可定制性是它的一个巨大优势。You can write your own Emacs Lisp functions to automate repetitive tasks or extend the editor's functionality. 比如，通过编写简单的函数，你可以实现自动格式化代码、批量重命名文件，或者集成Git等版本控制工具。Emacs 的强大之处在于它的灵活性和扩展性，让用户可以根据自己的需求定制工作环境。"
  ;;  400)
  )

(pop-buffer-insert 10
  (let ((lst (make-list 3 (concat "emacs"))))
    (setf (car lst)
          (let ((str (car lst)))
            (add-face-text-property
             0 (length str) 'bold t str)
            str))
    (string-join lst " ")))

(pop-buffer-insert 10
  (let ((lst (make-list 3 (concat "emacs"))))
    (setf (car lst)
          (let ((str (copy-sequence (car lst))))
            (add-face-text-property
             0 (length str) 'bold t str)
            str))
    (string-join lst " ")))
