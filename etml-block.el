;; -*- lexical-binding: t -*-
;;; emacs text block
;; block 的 width 为 (num) 时，表示整个 block 的像素宽度
;; block 的 width 为 num 时，表示内容部分的字符的个数

(require 'etml-utils)

;; (defvar etml-block-scroll-bar-pixel 2
;;   "Pixel width of scroll bar in vertical.")

;; (defvar etml-block-scroll-bar-color '("#111" . "#fff")
;;   "Color of scroll bar in vertical. It's a cons-cell,
;; the car of it is color in light theme; the cdr of it is
;; color in dark theme.")

;; (defvar etml-block-scroll-bar-direction 'right
;;   "The direction of scroll bar, could be set as 'left or 'right.")

;; (defvar etml-block-scroll-bar-full-p nil
;;   "在有border的时候生效，确定是否用两边border包裹的 scroll bar.")

;; (defvar etml-block-scroll-bar-gap-pixel 1
;;   "The gap pixel between scroll bar and border when has border.
;; If etml-block-scroll-bar-full-p is non-nil, set gaps at both.")

(defvar etml-block-caches nil
  "Caches of etml-block, it should be set as
 a buffer-local variable.")

(defun etml-block-caches-init (&optional buffer-or-name)
  "Initialize `etml-block-caches' in BUFFER-OR-NAME."
  (with-current-buffer (or (and buffer-or-name
                                (get-buffer-create buffer-or-name))
                           (current-buffer))
    (setq-local
     etml-block-caches
     (make-hash-table
      :test 'equal :size 100 :rehash-size 1.5 :weakness nil))))

(defclass etml-block ()
  ((uuid :initarg :uuid :initform (org-id-uuid))
   (content :initarg :content :initform "" :type string)
   (width :initarg :width :initform nil
          :documentation "content pixel width or char number.
If it's a pixel, it should be a cons-cell (<n-pixel>); if it's
the number of char, it should be a <number>.")
   (min-width :initarg :min-width :initform nil
              :documentation "min width of content, format is
the same with width.")
   (max-width :initarg :max-width :initform nil
              :documentation "max width of content, format is
the same with width.")
   (justify :initarg :justify :initform 'left)
   (height :initarg :height :initform nil
           :documentation "content height")
   (min-height :initarg :min-height :initform 1
               :documentation "content min height")
   (max-height :initarg :max-height :initform nil
               :documentation "content max height")
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
   (scroll-offset-x
    :initarg :scroll-offset-x :initform 0
    :documentation "当有溢出时的水平方向的偏移量")
   (scroll-offset-y
    :initarg :scroll-offset-y :initform 0
    :documentation "当有溢出时的垂直方向的偏移量")
   (scroll-bar-pixel :initarg :scroll-bar-pixel
                     :initform 2)
   (scroll-bar-color :initarg :scroll-bar-color
                     :initform '("#111" . "#fff"))
   (scroll-bar-direction :initarg :scroll-bar-direction
                         :initform 'right)
   (scroll-bar-gap
    :initarg :scroll-bar-gap
    :initform 1
    :documentation
    "The gap pixel between scroll bar and border \
when has border. If scroll-bar-full is non-nil,\
set gaps at both.")
   (scroll-bar-full
    :initarg :scroll-bar-full
    :initform nil
    :documentation
    "在有border的时候生效，确定是否用两边border包裹的 scroll bar.")))

(defun etml-block-parse-border (border)
  "Parse the BORDER data in a single direction and return a plist\
 like '(:type scroll :color \"#xxx\" :width 1). If type is nil, it's\
 a normal line border; if type is 'scroll, it's a scroll bar.

Width here is not the real width of border, if border type is 'scroll,it should add the extra pixel width of scroll bar.

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
        ((stringp border)
         (list :color (etml-default-border-color) :width 1))
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
                  :top (list :color default-color :width 1)
                  :bottom (list :color default-color :width 1)))
           ((null border)
            ;; no border
            (list :left (list :width 0) :right (list :width 0)
                  :top (list :width 0) :bottom (list :width 0)))
           ((stringp border)
            ;; border is color
            (list :left (list :color border :width 1)
                  :right (list :color border :width 1)
                  :top (list :color border :width 1)
                  :bottom (list :color border :width 1)))
           ((plistp border) border)
           ((etml-atom-consp border)
            ;; '(light-color . dark-color)
            (let ((light-theme-color (car border))
                  (dark-theme-color (cdr border)))
              (pcase (etml-background-type)
                ('light
                 (list :left (list :color light-theme-color :width 1)
                       :right (list :color light-theme-color :width 1)
                       :top (list :color light-theme-color :width 1)
                       :bottom (list :color light-theme-color
                                     :width 1)))
                ('dark
                 (list :left (list :color dark-theme-color :width 1)
                       :right (list :color dark-theme-color :width 1)
                       :top (list :color dark-theme-color :width 1)
                       :bottom (list :color dark-theme-color
                                     :width 1))))))
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
        (etml-width-pixel width (oref block content))
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
        (etml-width-pixel width (oref block content))
      ;; top and bootom margin is the number of lines.
      width)))

(defun etml-block-parse-overflow (overflow)
  (cond
   ((symbolp overflow) (cons overflow overflow))
   ((consp overflow) overflow)
   (t (error "Invalid format of overflow: %S" overflow))))

(defun etml-block-y-scroll-p (block)
  "Return if BLOCK has scroll bar in vertical."
  (let* ((overflow (cdr (etml-block-parse-overflow
                         (oref block overflow))))
         (height (oref block height))
         (min-height (or (oref block min-height) height))
         (max-height (or (oref block max-height) height))
         (content-height (etml-block-content-height block))
         (shown-content-height
          (if height
              (min max-height (max height min-height))
            (let ((min-height (or min-height content-height))
                  (max-height (or max-height content-height)))
              (min max-height (max content-height min-height))))))
    (and (eq overflow 'scroll)
         (> content-height shown-content-height))))

(defun etml-block-side-pixel (block &optional side)
  "Return the side pixel width of BLOCK. Side pixel is the
total of margin, padding and border pixel.

Defautly count both the left and right side. If SIDE equals
to a symbol 'left, count the left side only; if SIDE equals
to a symbol 'right, count the right side only."
  (let* ((y-scroll-p (etml-block-y-scroll-p block))
         (scroll-bar-pixel (oref block scroll-bar-pixel))
         (scroll-bar-gap (oref block scroll-bar-gap))
         (scroll-bar-direction (oref block scroll-bar-direction))
         (scroll-bar-full (oref block scroll-bar-full))
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
                 (if (and
                      y-scroll-p
                      (eq 'left scroll-bar-direction))
                     (if scroll-bar-full
                         (+ 1 pixel scroll-bar-pixel
                            (* 2 scroll-bar-gap))
                       (+ pixel scroll-bar-pixel
                          scroll-bar-gap))
                   pixel)
               ;; no left border
               (if (and y-scroll-p
                        (eq 'left scroll-bar-direction))
                   scroll-bar-pixel
                 0))))
    (setq right-side-pixel
          (+ right-side-pixel
             ;; when type is non-nil,
             ;; add extra pixel of scroll bar
             (if-let* ((pixel (plist-get right-border :width))
                       ((> pixel 0)))
                 ;; has right border
                 (if (and
                      y-scroll-p
                      (eq 'right scroll-bar-direction))
                     (if scroll-bar-full
                         (+ 1 pixel scroll-bar-pixel
                            (* 2 scroll-bar-gap))
                       (+ pixel scroll-bar-pixel
                          scroll-bar-gap))
                   pixel)
               ;; no right border
               (if (and y-scroll-p
                        (eq 'right scroll-bar-direction))
                   scroll-bar-pixel
                 0))))
    (pcase side
      ((pred null) (+ left-side-pixel right-side-pixel))
      ('left left-side-pixel)
      ('right right-side-pixel)
      (_ (error "Invalid format of side %S" side)))))

(defun etml-block-parse-total-pixel (block slot)
  (when-let ((width (eval `(oref ,block ,slot))))
    (+ (etml-width-pixel width (oref block content))
       (etml-block-side-pixel block))))

(defun etml-block-total-curr-pixel (block)
  "Return the total current pixel width of BLOCK."
  (if-let ((width (etml-block-parse-total-pixel
                   block 'width)))
      width
    ;; if width is not set, use: content width + side pixel
    (+ (string-pixel-width (oref block content))
       (etml-block-side-pixel block))))

(defun etml-block-total-min-pixel (block)
  "Return the total min pixel width of BLOCK."
  (if-let ((min-width (etml-block-parse-total-pixel
                       block 'min-width)))
      min-width
    ;; if min-width is not set, use a default one.
    20))

(defun etml-block-total-max-pixel (block)
  "Return the total max pixel width of BLOCK."
  (if-let ((max-width (etml-block-parse-total-pixel
                       block 'max-width)))
      max-width
    ;; if max-width is not set, use width
    (etml-block-total-curr-pixel block)))

(defun etml-block-total-pixel (block)
  "Return the total pixel width of BLOCK."
  (max (etml-block-total-min-pixel block)
       (min (etml-block-total-curr-pixel block)
            (etml-block-total-max-pixel block))))

(defun etml-block-content-pixel (block)
  "Return the pixel width of BLOCK's content."
  (if-let ((width (oref block width)))
      (etml-width-pixel width (oref block content))
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

(defun etml-block-content-height (block)
  "文本设置了宽度之后的原始高度。"
  (etml-string-linum (etml-block-content block)))

(defun etml-block-border
    (block n-pixel height &optional border-color
           type scroll-offset
           scroll-bar-height scroll-bar-steps)
  "Return the left or right border of block. The border has
 N-PIXEL width and HEIGHT lines tall and color is COLOR.

If TYPE is nil, it's a normal line border;
If type is 'scroll, it's a scroll bar. Use SCROLL-BAR-HEIGHT,
 SCROLL-OFFSET and SCROLL-BAR-STEPS to format the scroll bar."
  ;; SCROLL-BAR-STEPS 是一个列表：
  ;; 它决定了每隔几个 offset scroll-bar 移动一个高度
  ;; 也就是 scroll-bar 上面显示的 offset 的高度
  (let ((border-string (etml-pixel-border
                        n-pixel height border-color))
        (scroll-string ""))
    ;; need to set scroll bar
    (if (eq type 'scroll)
        (let ((scroll-bar-pixel (oref block scroll-bar-pixel))
              (scroll-bar-color (etml-block-scroll-bar-color block))
              (scroll-bar-full (oref block scroll-bar-full))
              (scroll-bar-direction (oref block scroll-bar-direction))
              (scroll-bar-gap (oref block scroll-bar-gap)))
          (if scroll-bar-height
              ;; 内容部分的滚动条
              ;; scroll-offset 是滚动时的偏移量
              ;; shown-offset 是 scroll-bar 上面展示的 offset 的高度
              ;; 根据 scroll-offset 和 scroll-bar-step 计算 shown-offset
              (let* ((prefixs
                      ;; 计算前缀和
                      (let ((prefix 0))
                        (mapcar (lambda (n)
                                  (setq prefix (+ prefix n)))
                                scroll-bar-steps)))
                     (shown-offset (etml--num-in-prefixs
                                    scroll-offset prefixs))
                     (uuid (oref block uuid)))
                ;; 滚动条字符串
                (setq scroll-string
                      (concat
                       ;; blank before scroll bar
                       (when (> shown-offset 0)
                         (concat (etml-block-blank
                                  scroll-bar-pixel
                                  shown-offset)
                                 "\n"))
                       ;; scroll bar
                       ;; 在滚动条的第一行和最后一行添加 uuid 属性;
                       ;; shown-offset + 1 时，第一行替换为空，
                       ;; 最后一行的下一行替换为 scroll-bar;
                       ;; shown-offset - 1 时相反
                       (let* ((scroll-bar (etml-pixel-border
                                           scroll-bar-pixel
                                           scroll-bar-height
                                           scroll-bar-color))
                              (lst (split-string scroll-bar "\n" t)))
                         (setf (car lst)
                               (propertize
                                (car lst)
                                'etml-block-scroll-bar-start uuid
                                ;; scroll bar 首行存储 prefixs
                                ;; 用于计算 shown-offset
                                'etml-block-scroll-prefixs prefixs))
                         (setf (car (last lst))
                               (propertize
                                (car (last lst))
                                'etml-block-scroll-bar-end
                                uuid))
                         (string-join lst "\n"))
                       ;; blank after scroll bar
                       (when-let* ((rest-height
                                    (- height (or scroll-bar-height 0)
                                       shown-offset))
                                   ((> rest-height 0)))
                         (concat "\n" (etml-block-blank
                                       scroll-bar-pixel
                                       rest-height)))))
                ;; 设置 etml-block-scroll-bar，用于滚动时识别 scroll bar
                (setq scroll-string
                      (mapconcat
                       (lambda (line)
                         (propertize
                          line 'etml-block-scroll-bar uuid))
                       (split-string scroll-string "\n" t)
                       "\n")))
            ;; padding部分滚动条填充
            ;; case: pad scroll border in padding line and height is 1
            (setq scroll-string (etml-block-blank
                                 scroll-bar-pixel)))
          (if scroll-bar-full
              (etml-lines-concat
               (list border-string
                     (etml-pixel-spacing scroll-bar-gap)
                     scroll-string
                     (etml-pixel-spacing scroll-bar-gap)
                     border-string))
            (etml-lines-concat
             (cond ((eq scroll-bar-direction 'left)
                    (list border-string
                          (etml-pixel-spacing scroll-bar-gap)
                          scroll-string))
                   ((eq scroll-bar-direction 'right)
                    (list scroll-string
                          (etml-pixel-spacing scroll-bar-gap)
                          border-string))))))
      border-string)))

(defun etml-block-render (block)
  "返回卡片当前面渲染后的文本"
  (let* ((uuid (oref block uuid))
         (total-pixel (etml-block-total-pixel block))
         (content (etml-block-content block))
         (content-lines
          (if (string-empty-p content)
              (list "")
            (split-string content "\n" t)))
         (content-height (etml-string-linum content))
         ;; process overflow, scroll-bar-height and scroll-offset
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
         (overflow
          (etml-block-parse-overflow (oref block overflow)))
         (x-overflow (car overflow))
         (y-overflow (cdr overflow))
         ;; 溢出时，根据 overflow 策略处理: scroll / hidden / visible
         ;; 计算设置 scroll-bar-height, scroll-offset 和 content
         (x-scroll-offset (oref block scroll-offset-x))
         ;; FIXME: 处理水平方向溢出
         (y-scroll-offset (oref block scroll-offset-y))
         (y-scroll-bar-height 1) ;; 滚动条的高度
         (y-scroll-bar-steps nil) ;; 滚动几次对应一个显示的offset
         ;; 实际展示的多行内容
         (shown-content-lines content-lines)
         (shown-content-rest-lines nil) ;; used in visible
         (left-border-type nil)
         (right-border-type nil)

         (top-margin (etml-block-margin block :top))
         (bottom-margin (etml-block-margin block :bottom))
         (top-padding-height (etml-block-padding block :top))
         (bottom-padding-height (etml-block-padding block :bottom))
         (top-border (etml-block-single-border block :top))
         (bottom-border (etml-block-single-border block :bottom))
         (top-border-pixel (plist-get top-border :width))
         (bottom-border-pixel (plist-get bottom-border :width))
         (top-border-color (plist-get top-border :color))
         (bottom-border-color (plist-get bottom-border :color))
         (_ (when y-overflow-p
              ;; 设置 shown-content-lines 和 y-scroll-bar-height
              (pcase y-overflow
                ('scroll
                 ;; 垂直方向溢出，需要设置 type=scroll
                 (let* ((y-overflow-num ;; 溢出的行数
                         (- content-height shown-content-height))
                        ;; 偏移量不能大于溢出的行数
                        (final-y-scroll-offset (min y-scroll-offset
                                                    y-overflow-num))
                        (start final-y-scroll-offset)
                        ;; 偏移量 <= 溢出的行数
                        (end (if (<= start y-overflow-num)
                                 (+ start shown-content-height)
                               content-height))
                        (scroll-bar-direction
                         (oref block scroll-bar-direction)))
                   ;; 修正超出范围的 offset
                   (setq y-scroll-offset final-y-scroll-offset)
                   (cond
                    ((eq scroll-bar-direction 'right)
                     (setq right-border-type 'scroll))
                    ((eq scroll-bar-direction 'left)
                     (setq left-border-type 'scroll))
                    (t (error "Invalid format of scroll-bar-direction:%S" scroll-bar-direction)))
                   (if (> shown-content-height y-overflow-num)
                       (progn
                         ;; 内容展示高度 > 溢出高度时，计算滚动条高度
                         ;; 确保:一次滚动对应滚动条的一次移动
                         (setq y-scroll-bar-height
                               (- shown-content-height
                                  y-overflow-num))
                         (setq y-scroll-bar-steps
                               ;; 可以滚动的高度 = 展示内容高度 - 1
                               (make-list
                                (1- shown-content-height) 1)))
                     ;; 溢出高度 > 内容展示高度时，滚动条高度始终为1
                     ;; 需要确定几次滚动对应滚动条的一次移动
                     (setq y-scroll-bar-height 1)
                     (setq y-scroll-bar-steps
                           (let ((each (/ y-overflow-num
                                          (1- shown-content-height)))
                                 (rest
                                  (% y-overflow-num
                                     (1- shown-content-height))))
                             (seq-map-indexed
                              (lambda (elem idx)
                                (if (< idx rest)
                                    (1+ elem)
                                  elem))
                              (make-list
                               ;; 可以滚动的高度 = 展示内容高度 - 1
                               (1- shown-content-height) each)))))
                   ;; 给每行文本缓存 block 相关的边界信息和文本内容，用于滚动
                   ;; 边界信息用于在滚动时设置首行和尾行的边框:
                   ;; 判断：无 top/bottom padding
                   ;; 且有 top/bottom border
                   (puthash
                    uuid
                    (list
                     :lines shown-content-lines
                     :bgcolor (etml-block-bgcolor block)
                     :shown-lines-height shown-content-height
                     :top-border-pixel top-border-pixel
                     :bottom-border-pixel bottom-border-pixel
                     :top-padding-height top-padding-height
                     :bottom-padding-height bottom-padding-height
                     :top-border-color top-border-color
                     :bottom-border-color bottom-border-color)
                    etml-block-caches)
                   (setq shown-content-lines
                         (seq-subseq shown-content-lines start end))
                   (setq shown-content-lines
                         (seq-map-indexed
                          (lambda (line idx)
                            (propertize
                             line
                             'etml-block-line uuid
                             ;; etml-block-line-idx 是当前行相对于展示
                             ;; 的文本的偏移量。
                             ;; 用于判断是否为当前展示的首行或尾行
                             'etml-block-line-idx
                             (format "%s:%s" uuid idx)
                             ;; etml-block-y-offset 是当前行相对于
                             ;; 所有行文本的偏移量
                             'etml-block-y-offset y-scroll-offset
                             'etml-block-y-height shown-content-height
                             'keymap (etml-block-scroll-map)))
                          shown-content-lines))))
                ('hidden
                 (setq shown-content-lines
                       (seq-subseq content-lines
                                   0 shown-content-height)))
                ;; FIXME: how to display?
                ;; ('visible
                ;;  (setq shown-content-lines
                ;;        (seq-subseq content-lines
                ;;                    0 shown-content-height))
                ;;  (setq shown-content-rest-lines
                ;;        (seq-subseq content-lines
                ;;                    shown-content-height
                ;;                    content-height)))
                (_ (error "Invalid format of y-overflow: %S"
                          y-overflow)))))

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
         (right-border (etml-block-single-border block :right))
         (right-border-width (plist-get right-border :width))
         (right-border-color (plist-get right-border :color))
         (left-border-string (etml-block-border
                              block
                              left-border-width text-linum
                              left-border-color
                              left-border-type
                              y-scroll-offset
                              y-scroll-bar-height
                              y-scroll-bar-steps))
         (right-border-string (etml-block-border
                               block
                               right-border-width text-linum
                               right-border-color
                               right-border-type
                               y-scroll-offset
                               y-scroll-bar-height
                               y-scroll-bar-steps))
         (block-string
          (etml-lines-concat
           (list left-margin-string
                 left-border-string
                 (etml-pixel-spacing left-padding)
                 text
                 (etml-pixel-spacing right-padding)
                 right-border-string
                 right-margin-string)))
         (block-lines (split-string block-string "\n" t)))

    ;; 设置上下 padding
    (if (and (= text-linum 1)
             (< top-padding-height 1) (< bottom-padding-height 1))
        ;; 特殊情况：只有一行，且上下 0 < padding < 1
        (unless (and (= top-padding-height 0)
                     (= bottom-padding-height 0))
          (setf (car block-lines)
                (etml-propertize
                 (concat (car block-lines) "\n")
                 `( line-height ,(+ 1 top-padding-height
                                    bottom-padding-height)
                    display (raise ,bottom-padding-height)))))
      ;; 一般情况，padding > 1，设置像素行；padding < 1，设置 line-height
      ;; set top padding for start line
      (cond
       ((= top-padding-height 0) (ignore))
       ((< top-padding-height 1)
        (setf (car block-lines)
              (etml-propertize
               ;; first line already ends with \n here
               (concat (car block-lines) "\n")
               `(line-height ,(1+ top-padding-height)))))
       ((>= top-padding-height 1)
        (setq block-lines
              (append
               (make-list
                top-padding-height
                ;; padding 行需要加上左右边框及左右 margin
                (concat
                 left-margin-string
                 (when left-border
                   (etml-block-border
                    block left-border-width 1 left-border-color
                    left-border-type 0))
                 (etml-pixel-spacing
                  (+ content-pixel left-padding right-padding))
                 (when right-border
                   (etml-block-border
                    block right-border-width 1 right-border-color
                    right-border-type 0))
                 right-margin-string))
               block-lines))))
      ;; set bottom padding for end line
      (cond
       ((= bottom-padding-height 0) (ignore))
       ((< bottom-padding-height 1)
        (setf (car (last block-lines))
              (etml-propertize
               ;; last line always ends with \n
               (concat (car (last block-lines)) "\n")
               `(line-height (nil ,(1+ bottom-padding-height))))))
       ((>= bottom-padding-height 1)
        (setq block-lines
              (append
               block-lines
               (make-list
                bottom-padding-height
                ;; padding 行需要加上左右边框及margin
                (concat
                 left-margin-string
                 (when left-border
                   (etml-block-border
                    block left-border-width 1 left-border-color
                    left-border-type 0))
                 (etml-pixel-spacing
                  (+ content-pixel left-padding right-padding))
                 (when right-border
                   (etml-block-border
                    block right-border-width 1 right-border-color
                    right-border-type 0))
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
    (when (> top-border-pixel 0)
      ;; 属性设置要去除开头结尾的 margin 位置
      (let ((left-inc (if (> left-margin 0) 1 0))
            (right-dec (if (> right-margin 0) 1 0))
            (color (plist-get top-border :color)))
        (setf (car block-lines)
              ;; make-list 创建的是同一个对象，需要 copy 新的 string
              (let ((line (car block-lines)))
                (if (string-match-p "\n$" line)
                    ;; 最后的换行符不能加 overline 属性
                    (etml-propertize
                     line `(face (:overline ,color))
                     left-inc (- (length line) 1 right-dec))
                  (etml-propertize
                   line `(face (:overline ,color))
                   left-inc (- (length line) right-dec)))))))

    ;; 新增下边框行，要pad上左右 margin
    (when (> bottom-border-pixel 0) 
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
                (if (string-match-p "\n$" line)
                    ;; 最后的换行符不能加 overline 属性
                    (etml-propertize
                     line `(face (:underline ( :color ,color
                                               :style ,style
                                               :position t)))
                     left-inc (- (length line) 1 right-dec))
                  (etml-propertize
                   line `(face (:underline ( :color ,color
                                             :style ,style
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
;; etml block scroll

(defvar etml-block-scroll-down-keys '("n" [wheel-down]))
(defvar etml-block-scroll-up-keys '("p" [wheel-up]))
;; (defvar etml-block-redraw-keys '("g"))

(defun etml-block-scroll-map ()
  (let ((map (make-sparse-keymap)))
    (dolist (key etml-block-scroll-down-keys)
      (define-key map key #'etml-block-scroll-down))
    (dolist (key etml-block-scroll-up-keys)
      (define-key map key #'etml-block-scroll-up))
    map))

(defun etml-block-scroll-bar-color (block)
  (let ((color (oref block scroll-bar-color)))
    (when (stringp color)
      (setq color (cons color color)))
    (pcase (etml-background-type)
      ('light (car color))
      ('dark (cdr color)))))

(defun etml-block-scroll (up-or-down)
  (when-let* ((props (text-properties-at (point)))
              ;; 当前光标所在行 idx，用于滚动后保持光标位置
              (line-idx (plist-get props 'etml-block-line-idx))
              (uuid (plist-get props 'etml-block-line))
              (offset (plist-get props 'etml-block-y-offset)))
    (when-let* ((inhibit-read-only t)
                (caches (gethash uuid etml-block-caches))
                (content-shown-height
                 (plist-get caches :shown-lines-height))
                (top-padding-height
                 (plist-get caches :top-padding-height))
                (bottom-padding-height
                 (plist-get caches :bottom-padding-height))
                (top-border-pixel
                 (plist-get caches :top-border-pixel))
                (bottom-border-pixel
                 (plist-get caches :bottom-border-pixel))
                (lines (plist-get caches :lines))
                (content-total-height (length lines))
                (max-offset (- content-total-height
                               content-shown-height))
                (all-content-idx offset)
                (new-offset
                 ;; content-height - shown-content-height
                 (cond ((eq 'down up-or-down) (1+ offset))
                       ((eq 'up up-or-down) (1- offset))))
                (shown-content-idx 0))
      (when (and (>= new-offset 0)
                 (<= new-offset max-offset))
        (let ((bgcolor (plist-get caches :bgcolor)))
          (goto-char (point-min))
          ;; 根据 uuid 属性搜索并将每一行替换为滚动后的新的文本
          (while-let ((match (text-property-search-forward
                              'etml-block-line uuid t))
                      (start (prop-match-beginning match))
                      (end (prop-match-end match))
                      (line (nth
                             (cond
                              ((eq 'down up-or-down)
                               (1+ all-content-idx))
                              ((eq 'up up-or-down)
                               (1- all-content-idx)))
                             lines)))
            (setq line (propertize
                        line
                        'etml-block-line uuid
                        'etml-block-line-idx
                        (format "%s:%s" uuid shown-content-idx)
                        'etml-block-y-offset new-offset
                        'etml-block-y-height content-shown-height
                        'keymap (etml-block-scroll-map)))
            ;; padding < 1 and has top/bottom border
            ;; should set top and bottom border
            (cond
             ((= shown-content-idx 0)
              (when (and (< top-padding-height 1)
                         (> top-border-pixel 0))
                (setq line
                      (propertize
                       line 'face
                       `(:overline
                         ,(plist-get caches :top-border-color))))))
             ((= shown-content-idx (1- content-shown-height))
              (when (and (< bottom-padding-height 1)
                         (> bottom-border-pixel 0))
                (setq line
                      (propertize
                       line 'face
                       `(:underline
                         ( :color ,(plist-get caches
                                              :top-border-color)
                           :position t)))))))
            (delete-region start end)
            (goto-char start)
            (if bgcolor
                (insert (propertize
                         line 'face `(:background ,bgcolor)))
              (insert line))
            (cl-incf all-content-idx 1)
            (cl-incf shown-content-idx 1)))
        ;; 根据 uuid 属性搜索并设置滚动条的首行和尾行
        (goto-char (point-min))
        (when-let*
            ((scroll-top-match
              (text-property-search-forward
               'etml-block-scroll-bar-start uuid t))
             (scroll-top-start
              (prop-match-beginning scroll-top-match))
             (scroll-top-end
              (prop-match-end scroll-top-match))
             (scroll-bar-color
              (save-excursion
                (goto-char scroll-top-start)
                (let ((plist
                       (seq-mapcat
                        (lambda (elem)
                          (if (consp elem) elem (list elem)))
                        (plist-get (text-properties-at (point))
                                   'face))))
                  (plist-get plist :foreground))))
             (prefixs
              (plist-get
               (text-properties-at scroll-top-start)
               'etml-block-scroll-prefixs))
             (prev-shown-offset (etml--num-in-prefixs
                                 offset prefixs))
             (curr-shown-offset (etml--num-in-prefixs
                                 new-offset prefixs))
             ;; shown-offset 有变化时，才移动滚动条
             ((not (= prev-shown-offset
                      curr-shown-offset)))
             ;; (one-line-scroll-bar
             ;;  (buffer-substring scroll-top-start
             ;;                    scroll-top-end))
             ;; (scroll-bar-pixel (string-pixel-width
             ;;                    one-line-scroll-bar))
             )
          (save-excursion
            (goto-char (point-min))
            (when-let* ((scroll-bottom-match
                         (text-property-search-forward
                          'etml-block-scroll-bar-end uuid t))
                        (scroll-bottom-start
                         (prop-match-beginning scroll-bottom-match))
                        (scroll-bottom-end
                         (prop-match-end scroll-bottom-match)))
              (cond
               ;; 向下滚动
               ((eq 'down up-or-down)
                (goto-char scroll-top-start)
                ;; 首行 face 和 etml-block-scroll-bar-start 属性清空
                (add-text-properties
                 (point) (1+ (point))
                 '( face nil
                    etml-block-scroll-bar-start nil
                    etml-block-scroll-prefixs nil))
                ;; 如果 padding < 1 且有上边框，需要保留上边框 face
                (when (and (< top-padding-height 1)
                           (= prev-shown-offset 0)
                           (> top-border-pixel 0))
                  (add-text-properties
                   (point) (1+ (point))
                   `(face (:overline
                           ,(plist-get
                             caches :top-border-color)))))
                ;; 第二行设置为新的首行
                ;; (next-logical-line 1)
                (goto-char scroll-top-end)
                (when-let* ((match (text-property-search-forward
                                    'etml-block-scroll-bar uuid t))
                            (start (prop-match-beginning match))
                            (end (prop-match-end match)))
                  (add-text-properties
                   start end
                   `( etml-block-scroll-bar-start ,uuid
                      etml-block-scroll-prefixs ,prefixs)))
                ;; 尾行的 etml-block-scroll-bar-start 属性清空
                (add-text-properties
                 scroll-bottom-start scroll-bottom-end
                 '(etml-block-scroll-bar-end nil))
                ;; 尾行的下一行设置为滚动条，并设置为新的尾行 uuid 属性
                (goto-char scroll-bottom-end)
                (when-let* ((match (text-property-search-forward
                                    'etml-block-scroll-bar uuid t))
                            (start (prop-match-beginning match))
                            (end (prop-match-end match)))
                  (add-text-properties
                   start end
                   `( face ( :inverse-video t
                             :foreground ,scroll-bar-color)
                      etml-block-scroll-bar-end ,uuid))))
               ;; 向上滚动
               ((eq 'up up-or-down)
                ;; 尾行 face 和 etml-block-scroll-bar-start 属性清空
                (add-text-properties
                 scroll-bottom-start scroll-bottom-end
                 '(face nil etml-block-scroll-bar-end nil))
                ;; 如果 padding < 1 且有下边框，需要保留下边框 face
                (when (and (< bottom-padding-height 1)
                           (= prev-shown-offset
                              (1- content-shown-height))
                           (> bottom-border-pixel 0))
                  (add-text-properties
                   scroll-bottom-start scroll-bottom-end
                   `(face
                     (:underline
                      ( :position t
                        :color ,(plist-get
                                 caches :bottom-border-color))))))
                ;; 倒数第二行设置新的尾行
                (goto-char scroll-bottom-start)
                (when-let* ((match (text-property-search-backward
                                    'etml-block-scroll-bar uuid t))
                            (start (prop-match-beginning match))
                            (end (prop-match-end match)))
                  (add-text-properties
                   start end
                   `(etml-block-scroll-bar-end ,uuid)))
                ;; 首行的 etml-block-scroll-bar-start 属性清空
                (add-text-properties
                 scroll-top-start scroll-top-end
                 '( etml-block-scroll-bar-start nil
                    etml-block-scroll-prefixs nil))
                ;; 首行的上一行设置为滚动条，并设置为新的首行
                (goto-char scroll-top-start)
                (when-let* ((match (text-property-search-backward
                                    'etml-block-scroll-bar uuid t))
                            (start (prop-match-beginning match))
                            (end (prop-match-end match)))
                  (add-text-properties
                   start end
                   `( face ( :inverse-video t
                             :foreground ,scroll-bar-color)
                      etml-block-scroll-bar-start ,uuid
                      etml-block-scroll-prefixs ,prefixs))))))))))
    (goto-char (point-min))
    ;; 滚动之后，重定向光标到当前行文本的开头
    (when-let ((match (text-property-search-forward
                       'etml-block-line-idx line-idx t)))
      (goto-char (prop-match-beginning match)))))

;;;###autoload
(defun etml-block-scroll-up ()
  (interactive)
  (etml-block-scroll 'up))

;;;###autoload
(defun etml-block-scroll-down ()
  (interactive)
  (etml-block-scroll 'down))

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
