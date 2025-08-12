;; -*- lexical-binding: t -*-
;;; emacs text block
;; block 的 width 为 (num) 时，表示整个 block 的像素宽度
;; block 的 width 为 num 时，表示内容部分的字符的个数

(require 'etml-utils)

(seq-subseq '(1 2 3 4 5 6 7 8) 1 8)

(defclass etml-block ()
  ((content :initarg :content :initform "" :type string)
   (width :initarg :width :initform nil
          :documentation "block width or content char number")
   (min-width :initarg :min-width :initform nil)
   (max-width :initarg :max-width :initform nil)
   (justify :initarg :justify :initform 'left)
   (height :initarg :height :initform nil
           :documentation "content height")
   (min-height :initarg :min-height :initform 1)
   (max-height :initarg :max-height :initform nil)
   (align :initarg :align :initform 'top)
   (bgcolor :initarg :bgcolor :initform nil)
   (border :initarg :border :initform nil)
   (margin :initarg :margin :initform nil)
   (padding :initarg :padding :initform nil)
   (nowrap :initarg :nowrap :initform nil :type symbol
           :documentation "表示水平方向是否不换行，默认换行")
   (overflow
    :initarg :overflow :initform 'scroll
    :documentation "溢出时如何显示，会被解析为:(x-overflow . y-overflow)。
1. 当 nowrap=t 且内容长度超过 width 时，根据 overflow-x 处理溢出部分；
2. 当 height > 文本高度时，根据 overflow-y 处理溢出的行。
支持以下的值: visible(内容溢出到容器外), hidden(截断溢出部分),\
 scroll(默认：内容溢出时支持滚动)。")
   ;; (scroll-bar-size :initarg :scroll-size
   ;;                  :documentation "垂直滚动条的高度大小")
   (scroll-offset :initarg :offset :initform '(0 . 0)
                  :documentation "当有溢出时的水平和垂直方向的偏移量")))

;; (defun etml-block-parse-color (color default-color)
;;   "Parse all kinds of COLOR to the real color.

;; 1. If COLOR is t, return the DEFAULT-COLOR.
;; 2. If COLOR is nil, return nil.
;; 3. If COLOR is a string, return the string as color.
;; 4. If COLOR is a cons-cell, return the car of cons-cell
;;    in light theme and the cdr of cons-cell in dark theme."
;;   (if (plistp color)
;;       ;; do not parse '(:style double-line :color "grey")
;;       color
;;     (cond ((eq t color) default-color)
;;           ((null color) nil)
;;           ((stringp color) color)
;;           ((etml-atom-consp color)
;;            (pcase (etml-background-type)
;;              ('light (car color))
;;              ('dark (cdr color))))
;;           (t (error "Invalid format of border color %S" color)))))

;; (defun etml-block-parse-color (color default-color)
;;   "Parse all kinds of COLOR to the real color.

;; 1. If COLOR is t, return the DEFAULT-COLOR.
;; 2. If COLOR is nil, return nil.
;; 3. If COLOR is a string, return the string as color.
;; 4. If COLOR is a cons-cell, return the car of cons-cell
;;    in light theme and the cdr of cons-cell in dark theme."
;;   (cond ((eq t color) default-color)
;;         ((null color) nil)
;;         ((stringp color) color)
;;         ((etml-atom-consp color)
;;          (pcase (etml-background-type)
;;            ('light (car color))
;;            ('dark (cdr color))))
;;         (t (error "Invalid format of border color %S" color))))

(defun etml-block-parse-border (border)
  "Parse the BORDER data in a single direction and return a plist\
 like '(:type scroll :color \"#xxx\" :width 1). If type is nil, it's\
 a normal line border; if type is 'scroll, it's a scroll bar.

Width here is not the real width of border, it border type is 'scroll,\
it should add the extra 2 pixel width of scroll bar.

1. If BORDER is t, color is DEFAULT-BORDER, width is 1.
2. If BORDER is nil, return nil.
3. If BORDER is a string, color is the string, width is 1.
4. If BORDER is a cons-cell, color is the car of cons-cell
   when in light theme and the cdr of cons-cell when in dark theme.
 width is 1.
5. If BORDER is a plist, directly return it."
  (cond ((eq t border)
         (list :color (etml-default-border-color) :width 1))
        ((null border) (list :width 0))
        ((stringp border) (list :color color :width 1))
        ((plistp border) border)
        ((etml-atom-consp border)
         (pcase (etml-background-type)
           ('light (list :color (car border) :width 1))
           ('dark (list :color (cdr border) :width 1))))
        (t (error "Invalid format of border %S" border))))

(defun etml-block-single-border (block direction)
  "Parse the multiple format of border property in BLOCK
and return the border color in a given DIRECTION.

Border supports the following formats:

1. 't means: borders in all directions are set to the default color.
2. 'nil means: borders in all directions are not set.
3. a color string means: borders in all directions are set to that color.
4. a cons-cell with only two elements means: set the car of cons-cell as
   border color in light theme, set the cdr of cons-cell as border color
   in dark theme.
5. a plist means: set the border in each direction separately and the value
   of border in each direction also supports all kinds of formats above.
   Such as '(:left t :right nil :top \"red\" :bottom (\"red\" . \"green\"))"
  (let* ((border (oref block border))
         ;; border data in all directions.
         (default-color (etml-default-border-color))
         (plist
          (cond
           ((eq border t)
            ;; use default color, width is pixel,
            ;; width only supports in left and right border
            (list :left (list :color default-color :width 1)
                  :right (list :color default-color :width 1)
                  :top (list :color default-color)
                  :bottom (list :color default-color)))
           ((null border)
            ;; no border
            (list :left (list :width 0) :right (list :width 0)
                  :top (list :width 0) :bottom (list :width 0)))
           ((stringp border)
            ;; border is color
            (list :left (list :color border :width 1)
                  :right (list :color border :width 1)
                  :top (list :color border)
                  :bottom (list :color border)))
           ((plistp border) border)
           ((etml-atom-consp border)
            ;; '(light-color . dark-color)
            (let ((light-theme-color (car border))
                  (dark-theme-color (cdr border)))
              (pcase (etml-background-type)
                ('light
                 (list :left (list :color light-theme-color :width 1)
                       :right (list :color light-theme-color :width 1)
                       :top (list :color light-theme-color)
                       :bottom (list :color light-theme-color)))
                ('dark
                 (list :left (list :color dark-theme-color :width 1)
                       :right (list :color dark-theme-color :width 1)
                       :top (list :color dark-theme-color)
                       :bottom (list :color  dark-theme-color))))))
           (t (error "Invalid format of border: %S" border))))
         ;; border data in a single DIRECTION.
         (border (plist-get plist direction)))
    ;; 返回一个 cons-cell: (color . scroll)
    (etml-block-parse-border border)))

(defun etml-block-padding (block direction)
  "Return the padding of BLOCK in DIRECTION."
  (let* ((padding (oref block padding))
         (plist
          (cond
           ((eq padding t)
            (list :left '(4) :right '(4) :top 0 :bottom 0))
           ((null padding)
            (list :left 0 :right 0 :top 0 :bottom 0))
           ((plistp padding)
            (unless (plist-get padding :left)
              (plist-put padding :left 0))
            (unless (plist-get padding :right)
              (plist-put padding :right 0))
            (unless (plist-get padding :top)
              (plist-put padding :top 0))
            (unless (plist-get padding :bottom)
              (plist-put padding :bottom 0))
            padding)
           ((etml-atom-consp padding)
            (list :left (car padding) :right (car padding)
                  :top (cdr padding) :bottom (cdr padding)))
           (t (error "Invalid format of padding: %S" padding))))
         (width (plist-get plist direction)))
    (if (or (eq direction :left)
            (eq direction :right))
        ;; 左右的 padding 需要处理为像素
        (etml-width-pixel width)
      ;; top and bootom padding is the number of lines.
      width)))

(defun etml-block-margin (block direction)
  "Return the margin of BLOCK in DIRECTION."
  (let* ((margin (oref block margin))
         (plist
          (cond
           ((or (eq margin t) (null margin))
            (list :left 0 :right 0 :top 0 :bottom 0))
           ((plistp margin)
            (unless (plist-get margin :left)
              (plist-put margin :left 0))
            (unless (plist-get margin :right)
              (plist-put margin :right 0))
            (unless (plist-get margin :top)
              (plist-put margin :top 0))
            (unless (plist-get margin :bottom)
              (plist-put margin :bottom 0))
            margin)
           ((etml-atom-consp margin)
            (list :left (car margin) :right (car margin)
                  :top (cdr margin) :bottom (cdr margin)))
           (t (error "Invalid format of margin: %S" margin))))
         (width (plist-get plist direction)))
    (if (or (eq direction :left)
            (eq direction :right))
        (etml-width-pixel width)
      ;; top and bootom margin is the number of lines.
      width)))

(defun etml-block-side-pixel (block &optional side)
  "Return the side pixel width of BLOCK. Side pixel is the
total of margin, padding and border pixel.

Defautly count both the left and right side. If SIDE equals
to a symbol 'left, count the left side only; if SIDE equals
to a symbol 'right, count the right side only."
  (let* ((left-border (etml-block-single-border block :left))
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
             ;; add extra 2 pixel of scroll bar
             (if (plist-get left-border :type) 2 0)
             (plist-get left-border :width)))
    (setq right-side-pixel
          (+ right-side-pixel
             ;; when type is non-nil,
             ;; add extra 2 pixel of scroll bar
             (if (plist-get right-border :type) 2 0)
             (plist-get right-border :width)))
    (pcase side
      ((pred null) (+ left-side-pixel right-side-pixel))
      ('left left-side-pixel)
      ('right right-side-pixel)
      (_ (error "Invalid format of side %S" side)))))

(defun etml-block-get-pixel (block slot)
  (if-let ((width (eval `(oref ,block ,slot))))
      (cond
       ((consp width) (car width))
       ((numberp width)
        (let ((content (oref block content)))
          (+ (etml-block-side-pixel block)
             (etml-width-pixel width content))))
       (t (error "Invalid format of block width: %S" width)))
    ;; when :width is nil, total-pixel = content pixel + side pixel
    (if (eq slot 'min-width)
        20 ;; 不设置 min-width 时的默认最小宽度
      (+ (string-pixel-width (oref block content))
         (etml-block-side-pixel block)))))

(defun etml-block-total-min-pixel (block)
  "Return the total min pixel width of BLOCK."
  (etml-block-get-pixel block 'min-width))

(defun etml-block-total-max-pixel (block)
  "Return the total max pixel width of BLOCK."
  (etml-block-get-pixel block 'max-width))

(defun etml-block-total-curr-pixel (block)
  "Return the total current pixel width of BLOCK."
  (etml-block-get-pixel block 'width))

(defun etml-block-total-pixel (block)
  "Return the total pixel width of BLOCK."
  (max (etml-block-total-min-pixel block)
       (min (etml-block-total-curr-pixel block)
            (etml-block-total-max-pixel block))))

(defun etml-block-content-pixel (block)
  "Return the pixel width of BLOCK's content."
  (if (oref block width)
      (- (etml-block-total-pixel block)
         (etml-block-side-pixel block))
    ;; when :width is nil, use the real content pixel
    (string-pixel-width (oref block content))))

(defun etml-block-content (block)
  "Return the block content after justified and set width."
  (etml-lines-justify (oref block content)
                      (etml-block-content-pixel block)
                      (oref block justify)))

(defun etml-block-bgcolor (block)
  "Return the background color of BLOCK."
  (let ((color (oref block bgcolor)))
    (cond ((null color) nil)
          ((stringp color) color)
          ((etml-atom-consp color)
           (pcase (etml-background-type)
             ('light (car color))
             ('dark (cdr color))))
          (t (error "Invalid format of bgcolor: %S" color)))))

(defun etml-block-bottom-border-line (width border)
  (if border
      (etml-propertize (etml-pixel-spacing width)
                       `(face (:overline ,border)))
    (etml-pixel-spacing width)))

;; (mwheel-scroll EVENT &optional ARG)
;; FIXME: consider min-width and max-width!
(defun etml-block-render (block)
  "返回卡片当前面渲染后的文本"
  (let* ((total-pixel (etml-block-total-pixel block))
         (content (etml-block-content block))
         (content-lines
          (if (string-empty-p content)
              (list "")
            (split-string content "\n" t)))
         (content-height (etml-string-linum content))
         ;; process overflow, scroll-bar-size and scroll-offset
         ;; 判断是否溢出
         (height (oref block height))
         (min-height (or (oref block min-height) height))
         (max-height (or (oref block max-height) height))
         (shown-content-height
          (if height
              (min max-height (max height min-height))
            (let ((min-height (or min-height content-height))
                  (max-height (or max-height content-height)))
              (min max-height (max content-height min-height)))))
         ;; shown-content-height content-height
         ;; content-lines
         ;; (width (oref block width))
         ;; (min-width (oref block min-width))
         ;; (max-width (oref block max-width))
         
         ;; 计算了 width, min-width, max-width 之后的最终内容宽度
         (content-pixel (etml-block-content-pixel block))
         (final-width content-pixel)
         ;; 原始文本的宽度
         (original-content-width (string-pixel-width content))
         ;; 判断水平方是否溢出
         (x-overflow-p (and (> original-content-width final-width)
                            (oref block nowrap)))
         ;; 判断垂直方是否溢出
         (y-overflow-p (> content-height shown-content-height))
         ;; 解析 水平 和 垂直方向的溢出策略
         (overflow (let ((overflow (oref block overflow)))
                     (if (symbolp overflow)
                         (cons overflow overflow)
                       overflow)))
         (x-overflow (car overflow))
         (y-overflow (cdr overflow))
         ;; 溢出时，根据 overflow 策略处理: scroll / hidden / visible
         ;; 计算设置 scroll-bar-size, scroll-offset 和 content
         ;; (_ (when x-overflow-p ())) ;; FIXME
         (scroll-offset (oref block scroll-offset))
         (x-scroll-offset (car scroll-offset))
         ;; (y-scroll-bar-size nil)
         (y-scroll-offset (cdr scroll-offset))
         (y-scroll-bar-size 1)
         ;; 实际展示的多行内容
         (shown-content-lines content-lines)
         (_ (message "shown-content-lines:%S" shown-content-lines))
         (shown-content-rest-lines nil) ;; used in visible
         (_ (when y-overflow-p
              ;; 设置 shown-content-lines 和 y-scroll-bar-size
              (pcase y-overflow
                ('scroll
                 (let* ((y-overflow-num ;; 溢出的行数
                         (- content-height shown-content-height))
                        (start y-scroll-offset)
                        ;; 偏移量 <= 溢出的行数
                        (end (if (<= start y-overflow-num)
                                 (+ start shown-content-height)
                               content-height)))
                   (setq y-scroll-bar-size
                         (- shown-content-height y-overflow-num))
                   (setq shown-content-lines
                         (seq-subseq content-lines start end))))
                ('hidden
                 (setq shown-content-lines
                       (seq-subseq content-lines 0 shown-content-height)))
                ;; FIXME: how to display?
                ;; ('visible
                ;;  (setq shown-content-lines
                ;;        (seq-subseq content-lines 0 shown-content-height))
                ;;  (setq shown-content-rest-lines
                ;;        (seq-subseq content-lines shown-content-height
                ;;                    content-height)))
                )))
         
         ;; height 不包含 padding and margin
         ;; text includes content and empty lines
         (text (etml-lines-align
                (string-join shown-content-lines "\n")
                shown-content-height
                (oref block align)))
         (text-linum (etml-string-linum text))
         ;; 确定文本高度之后再设置水平方向的 margin, padding, border
         ;; add padding and border in left and right
         (left-margin (etml-block-margin block :left))
         (right-margin (etml-block-margin block :right))
         (left-margin-string (etml-pixel-spacing left-margin))
         (right-margin-string (etml-pixel-spacing right-margin))
         (left-padding (etml-block-padding block :left))
         (right-padding (etml-block-padding block :right))
         ;; process left and border
         (left-border (etml-block-single-border block :left))
         (left-border-width (plist-get left-border :width))
         (left-border-color (plist-get left-border :color))
         (left-border-type (plist-get left-border :type))
         (right-border (etml-block-single-border block :right))
         (right-border-width (plist-get right-border :width))
         (right-border-color (plist-get right-border :color))
         (right-border-type (plist-get right-border :type))
         (left-border-string (etml-block-border
                              left-border-width text-linum
                              left-border-color left-border-type
                              y-scroll-bar-size
                              y-scroll-offset))
         (right-border-string (etml-block-border
                               right-border-width text-linum
                               right-border-color right-border-type
                               y-scroll-bar-size
                               y-scroll-offset))
         ;; (_ (message "text:%S" text))
         (block-string
          (etml-lines-concat
           (list left-margin-string
                 left-border-string
                 (etml-pixel-spacing left-padding)
                 text
                 (etml-pixel-spacing right-padding)
                 right-border-string
                 right-margin-string)))
         (top-margin (etml-block-margin block :top))
         (bottom-margin (etml-block-margin block :bottom))
         (top-padding (etml-block-padding block :top))
         (bottom-padding (etml-block-padding block :bottom))
         (bottom-border (etml-block-single-border block :bottom))
         (top-border (etml-block-single-border block :top))
         (block-lines (split-string block-string "\n" t)))

    ;; (message "block-lines:%S" block-lines)
    
    ;; 设置上下 padding
    (if (and (= text-linum 1)
             (< top-padding 1) (< bottom-padding 1))
        ;; 特殊情况：只有一行，且上下 0 < padding < 1
        (unless (and (= top-padding 0) (= bottom-padding 0))
          (setf (car block-lines)
                (etml-propertize
                 (concat (car block-lines) "\n")
                 `(line-height ,(+ 1 top-padding bottom-padding)
                               display (raise ,bottom-padding)))))
      ;; 一般情况，padding > 1，设置像素行；padding < 1，设置 line-height
      ;; set top padding for start line
      (cond
       ((= top-padding 0) (ignore))
       ((< top-padding 1)
        (setf (car block-lines)
              (etml-propertize
               ;; first line already ends with \n here
               (concat (car block-lines) "\n")
               `(line-height ,(1+ top-padding)))))
       ((>= top-padding 1)
        (setq block-lines
              (append
               (make-list
                top-padding
                ;; padding 行需要加上左右边框及左右 margin
                (concat
                 left-margin-string
                 (when left-border
                   (etml-block-border
                    left-border-width 1 left-border-color
                    left-border-type))
                 (etml-pixel-spacing
                  (+ content-pixel left-padding right-padding))
                 (when right-border
                   (etml-block-border
                    right-border-width 1 right-border-color
                    right-border-type))
                 right-margin-string))
               block-lines))))
      ;; set bottom padding for end line
      (cond
       ((= bottom-padding 0) (ignore))
       ((< bottom-padding 1)
        (setf (car (last block-lines))
              (etml-propertize
               ;; last line always ends with \n
               (concat (car (last block-lines)) "\n")
               `(line-height (nil ,(1+ bottom-padding))))))
       ((>= bottom-padding 1)
        (setq block-lines
              (append
               block-lines
               (make-list
                bottom-padding
                ;; padding 行需要加上左右边框及margin
                (concat
                 left-margin-string
                 (when left-border
                   (etml-block-border
                    left-border-width 1 left-border-color
                    left-border-type))
                 (etml-pixel-spacing
                  (+ content-pixel left-padding right-padding))
                 (when right-border
                   (etml-block-border
                    right-border-width 1 right-border-color
                    right-border-type))
                 right-margin-string)))))))

    ;; 给每一行设置背景色, margin 部分不设置背景色
    (when-let ((bgcolor (etml-block-bgcolor block))
               (left-inc (if (> left-margin 0) 1 0))
               (right-dec (if (> right-margin 0) 1 0)))
      (setq block-lines
            (mapcar (lambda (line)
                      (if (string-match-p "\n$" line)
                          (etml-propertize
                           line `(face (:background ,bgcolor))
                           left-inc (- (length line) 1 right-dec))
                        (etml-propertize
                         line `(face (:background ,bgcolor))
                         left-inc (- (length line) right-dec))))
                    block-lines)))
    
    ;; 给首行设置上边框，不能包含左右 margin
    (when top-border
      ;; 属性设置要去除开头结尾的 margin 位置
      (let ((left-inc (if (> left-margin 0) 1 0))
            (right-dec (if (> right-margin 0) 1 0)))
        (setf (car block-lines)
              ;; make-list 创建的是同一个对象，需要 copy 新的 string
              (let ((line (car block-lines)))
                (if (string-match-p "\n$" line)
                    ;; 最后的换行符不能加 overline 属性
                    (etml-propertize
                     line `(face (:overline ,top-border))
                     left-inc (- (length line) 1 right-dec))
                  (etml-propertize
                   line `(face (:overline ,top-border))
                   left-inc (- (length line) right-dec)))))))
    
    ;; 新增下边框行，要pad上左右 margin
    (when bottom-border
      ;; 属性设置要去除开头结尾的 margin 位置
      (let ((left-inc (if (> left-margin 0) 1 0))
            (right-dec (if (> right-margin 0) 1 0)))
        (setf (car (last block-lines))
              ;; make-list 创建的是同一个对象，需要 copy 新的 string
              (let ((line (car (last block-lines)))
                    color style)
                ;; bottom border supports:
                ;;  '(:style double-line :color "grey")
                ;; when line overflow, set style to 'double-line
                (if (plistp bottom-border)
                    (progn
                      (setq color (plist-get bottom-border :color))
                      (setq style (plist-get bottom-border :style)))
                  (setq color bottom-border))
                (message "style:%S" style)
                (if (string-match-p "\n$" line)
                    ;; 最后的换行符不能加 overline 属性
                    (etml-propertize
                     line `(face (:underline (:color ,color :style ,style
                                                     :position t)))
                     left-inc (- (length line) 1 right-dec))
                  (etml-propertize
                   line `(face (:underline (:color ,color :style ,style
                                                   :position t)))
                   left-inc (- (length line) right-dec)))))))

    ;; ;; set top and bottom margins, shoud be integer!
    (setq block-lines
          (append
           (make-list
            top-margin (etml-pixel-spacing total-pixel))
           block-lines
           (make-list
            bottom-margin (etml-pixel-spacing total-pixel))))
    
    ;; !!必须要在最后连接所有行，因为中间直接字符串操作可能使 line-height 失效
    (setq block-string
          (mapconcat (lambda (line)
                       (if (string-match-p "\n$" line)
                           line
                         (concat line "\n")))
                     block-lines))

    (string-trim-right block-string "\n")))

;;;###autoload
(defun etml-block-concat (&rest blocks)
  "TEXT-ALIGN should be one of left,center,right.
ALIGN should be one of top,center,bottom."
  (etml-block :content (etml-lines-concat
                        (mapcar #'etml-block-render blocks))))

;;;###autoload
(defun etml-block-stack (&rest blocks)
  "ALIGN used for all blocks, TEXT-ALIGN used for text in a block."
  (etml-block :content (etml-lines-stack
                        (mapcar #'etml-block-render blocks))))

;;;###autoload
(defun etml-block-string (&rest kvs)
  (etml-block-render (apply #'etml-block kvs)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; (defun string-pixel-point (string pixel &optional right)
;;   ;; 寻找像素宽度不大于 pixel 的 point
;;   (let* ((chars (split-string string "" t))
;;          (pixels (mapcar #'string-pixel-width chars))
;;          (n 0) (sum 0))
;;     (when right
;;       (setq pixels (nreverse pixels)))
;;     (catch 'break
;;       (dolist (curr-pixel pixels)
;;         (if (> sum pixel)
;;             (throw 'break (if right
;;                               (- (length string) (1- n))
;;                             (1- n)))
;;           (cl-incf n 1)
;;           (setq sum (+ sum curr-pixel)))))))

;; ;; face 里面没有 :inverse-video 属性
;; (defun not-border-char (string &optional right lines)
;;   "获取非边框的字符，用于获取它的 properties"
;;   (seq-find
;;    (lambda (char)
;;      (let* ((char-face (get-char-property 0 'face char))
;;             (char-face (if (consp char-face)
;;                            char-face
;;                          (list char-face)))
;;             (merged-face (seq-mapcat
;;                           (lambda (e)
;;                             (if (consp e) e (list e)))
;;                           char-face)))
;;        (not (plist-get merged-face :inverse-video))))
;;    (let ((chars (split-string string "" t)))
;;      chars)))

;; (defun string-pixel-trim (string pixel type lines &optional right)
;;   "RIGHT 为 t，表示从右向左截取. type: top middle bottom
;; 返回的字符串带换行符"
;;   (let* ((point (string-pixel-point string pixel right))
;;          (new-string (if right
;;                          (substring string point)
;;                        (substring string 0 point)))
;;          (char (not-border-char new-string right lines))
;;          (properties (text-properties-and-remove
;;                       0 '(display) char))
;;          ;; ;; 首位行需要设置 line-height
;;          (line-height (text-properties-get properties 'line-height))
;;          ;; 空格的 face 只关心 backgroud，其他都不需要
;;          (face (text-properties-get properties 'face))
;;          (bgcolor (plist-get face :background))
;;          (new-pixel (string-pixel-width new-string))
;;          (rest-pixel (- pixel new-pixel))
;;          ;; 需要给空格设置背景色
;;          (rest-space (etml-pixel-spacing rest-pixel))
;;          ;; 给补齐的 space 设置本行文本相同的 properties
;;          (rest-space (etml-propertize rest-space properties))
;;          (new-string (if right
;;                          (concat rest-space new-string)
;;                        (concat new-string rest-space))))
;;     (cond ((eq type 'top)
;;            ;; 首行设置 line-height
;;            (propertize (concat new-string "\n")
;;                        'line-height line-height))
;;           ((eq type 'bottom)
;;            ;; 尾行设置 line-height
;;            (propertize (concat new-string "\n")
;;                        'line-height line-height))
;;           ((eq type 'middle)
;;            (concat new-string "\n")))))

;; (defun etml-block-cut-pixel (block content width &optional from-end)
;;   "Return the cut pixel of BLOCK. The cut pixel will be used in
;; `block-cut'.

;; If WIDTH is a cons-cell, return the car of it as the cut pixel of
;; BLOCK. If WIDTH is a integer, cut the BLOCK to WIDTH number of chars
;; in CONTENT and return the pixel of the cut block.
;; If FROM-END is non-nil, count from the end of block, otherwise from
;; the start."
;;   (let ((etml-pixels (etml-block-total-pixel block)))
;;     (cond
;;      ((consp width) (min block-pixels (car width)))
;;      ((numberp width)
;;       (let ((cut-pixels (etml-string-nchar-pixel
;;                          content width from-end)))
;;         (if from-end
;;             (+ cut-pixels (etml-block-side-pixel block 'right))
;;           (+ cut-pixels (etml-block-side-pixel block 'left)))))
;;      (t (error "Invalid format of width %S" width)))))

;; ;;; FIXME: add a new funcntion, block-lines-justify-cut
;; ;;; cut string by char and Justify it.

;; (defun etml-block-cut (block start-line end-line width &optional right)
;;   ;; start-line 从 1 开始
;;   (when (< end-line start-line)
;;     (error "start-line should be less than end-line"))
;;   (catch 'return
;;     (let* ((etml-block-string (etml-block-render block))
;;            ;; FIXME: why 1-
;;            (linum (1- (length (split-string block-string "\n")))))
;;       (when (< end-line 1) (throw 'return ""))
;;       (when (> start-line linum) (throw 'return ""))
;;       (when (< start-line 1) (setq start-line 1))
;;       (when (> end-line linum) (setq end-line linum))
;;       (let* ((string (oref block content))
;;              ;; bottom-border 需要单独获取的原因是它不属于最后一行本身的属性
;;              (bottom-border (etml-block-single-border block :bottom))
;;              (cut-lines (seq-subseq (split-string block-string "\n")
;;                                     (1- start-line) end-line))
;;              (content (string-trim (car cut-lines)))
;;              ;; FIXME: bug (message "%s" content) 会改变光标的位置
;;              (cut-pixel (etml-block-cut-pixel block content width t))
;;              (etml-pixel (etml-block-total-pixel block))
;;              ;; 用于裁剪的像素宽度，不能超过 block 的总宽度
;;              (cut-pixel (if (> cut-pixel block-pixel)
;;                             block-pixel
;;                           cut-pixel))
;;              ;; block的首行
;;              first-line first-line-string
;;              ;; block的尾行
;;              last-line last-line-string
;;              (middle-lines cut-lines) middle-lines-string)
;;         ;; 首行因为有 line-height 属性需要特殊处理
;;         ;; FIXME: 会改变 buffer 的 point
;;         (when (= start-line 1)
;;           (setq first-line (car cut-lines))
;;           (setq first-line-string
;;                 (string-pixel-trim first-line cut-pixel
;;                                    'top cut-lines right))
;;           (setq middle-lines (cdr cut-lines)))
;;         ;; 尾行因为有 line-height 属性 并且 要加上下边框的行
;;         (when (= end-line linum)
;;           (if (= linum 1)
;;               (progn
;;                 ;; 只有一行时，也要设置下边框
;;                 (setq last-line-string "")
;;                 (setq first-line-string
;;                       (concat
;;                        (string-pixel-trim first-line cut-pixel
;;                                           'bottom cut-lines right)
;;                        (etml-block-bottom-border-line cut-pixel
;;                                                  bottom-border))))
;;             (setq last-line (car (last cut-lines)))
;;             (setq last-line-string
;;                   (concat
;;                    (string-pixel-trim last-line cut-pixel
;;                                       'bottom cut-lines right)
;;                    (etml-block-bottom-border-line cut-pixel bottom-border)))
;;             (setq middle-lines (seq-drop-last middle-lines))))
;;         (when middle-lines
;;           (setq middle-lines-string
;;                 (mapconcat (lambda (line)
;;                              (string-pixel-trim line cut-pixel
;;                                                 'middle cut-lines right))
;;                            middle-lines "")))
;;         (concat first-line-string
;;                 middle-lines-string
;;                 last-line-string)))))

;; (defun etml-block-arrange ( below-block below-coordinate
;;                        above-block above-coordinate)
;;   "功能：
;; 连接两个 block 并形成新的 block。如果有重叠部分，above-blocka
;; 在 below-block 上面。below-coordinate 和 above-coordinate
;; 分别是两个 block 中的相交点的位置。点的格式是 (line . width)，
;; line 表示从上到下的第几行，width 从左到右的多少宽度。宽度支持两种，
;; cons-cell 数字表示像素值；单独的数字表示字符数。

;; 实现：

;; "
;;   (let* ((below-total-lines (etml-block-line-height below-block))
;;          (below-total-pixels (etml-pixel-width below-block))
;;          (above-total-lines (etml-block-line-height above-block))
;;          (above-total-pixels (etml-pixel-width above-block))

;;          (below-line (car below-coordinate))
;;          (above-line (car above-coordinate))
;;          ;; parse width
;;          (below-pixel (etml-width-pixel (cdr below-coordinate)
;;                                           below-block-content))
;;          (above-pixel (cdr above-coordinate))
;;          ;; coordinate 是 above block 左上角的点在 below 的位置
;;          (coordinate `(,(- below-line above-line)
;;                        . ,(- below-width above-width))))
;;     coordinate))

;; (etml-block-arrange nil '(0 . 400)
;;                nil '(0 . 0))

(provide 'etml-block)
