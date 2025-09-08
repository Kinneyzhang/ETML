;; -*- lexical-binding: t -*-

(require 'eieio)
(require 'ekp)
(require 'etml-block)

;;; type check functions

(defun etml-flex-number-vector-p (value)
  "检查类型: 单个值为整数; 多个值为向量，向量中的每个元素为整数"
  (or (numberp value)
      (and (vectorp value)
           (seq-every-p #'numberp value))))

(defun etml-flex-number-auto-vector-p (value)
  "检查类型: 单个值为整数或 'auto; 多个值为向量，向量中的每个元素为 整数或 'auto。"
  (or (or (numberp value)
          (eq 'auto value))
      (and (vectorp value)
           (seq-every-p (lambda (e)
                          (or (numberp e)
                              (eq 'auto e)))
                        value))))

(defun etml-flex-items-flex-p (value)
  "检查 item-flex 的类型: 单个值为列表，列表形式为 '(0 1 auto)，第一第二个元素为整数，
第三个元素为 'auto 或整数；多个值为向量，向量中每个元素为列表，列表中的元素类型同上。"
  (let ((check-lst (lambda (lst)
                     (and (integerp (nth 0 lst))
                          (integerp (nth 1 lst))
                          (let ((el (nth 2 lst)))
                            (or (eq 'auto el)
                                (integerp el)
                                (and (consp el)
                                     (= 1 (length el))
                                     (integerp (car el)))))))))
    (cond
     ((symbolp value) (or (eq 'none value) (eq 'auto value)))
     ((listp value) (funcall check-lst value))
     ((vectorp value) (seq-every-p check-lst value)))))

(defun etml-flex-items-align-p (value)
  "检查类型: 单个值为symbol; 多个值为向量，向量中的每个元素为symbol"
  (let ((check-sym
         (lambda (sym)
           (member sym '( auto flex-start flex-end
                          center baseline stretch)))))
    (cond
     ((symbolp value) (funcall check-sym value))
     ((vectorp value) (seq-every-p check-sym value)))))

;;; type check functions ends.

(defclass etml-flex-item (etml-block)
  ((self :initarg :self :type etml-block
         :documentation "item 本身。")
   (order :initarg :order :initform 0 :type integer
          :documentation "item 在 flex 容器中顺序。")
   (basis :initarg :basis :initform 'auto :type symbol
          :documentation "item 在​​分配空间前​​的​​初始尺寸。")
   (grow :initarg :grow :initform 0 :type integer
         :documentation "item 在​​容器有剩余空间时​​的​​增长系数​​。")
   (shrink :initarg :shrink :initform 1 :type integer
           :documentation "item 在​​容器空间不足时​​的​​缩小系数。")
   ;; 避免与 etml-block 的 align 属性冲突
   (cross-align
    :initarg :cross-align :initform 'auto :type symbol
    :documentation "item 在交叉轴的对齐方式，覆盖容器的 items-align 属性。")))

;; horizontal unit is pixel.
;; vertical unit is line number.
;; main-axis main-start main-end main-units
;; cross-axis cross-start cross-end cross-units

(defclass etml-flex (etml-block)
  ((display :initarg :display :initform 'flex :type symbol
            :documentation "容器是块级元素还是内联元素。")
   (direction :initarg :direction :initform 'row :type symbol
              :documentation "主轴方向。")
   (wrap :initarg :wrap :initform 'nowrap :type symbol
         :documentation "换行方式。")
   (content-justify
    :initarg :content-justify :initform 'flex-start :type symbol
    :documentation "主轴对齐方式。")
   (content-align
    :initarg :content-align :initform 'stretch :type symbol
    :documentation "多个主轴在垂直方向的对齐方式。")
   (row-gap :initarg :row-gap :initform 0
            :type number :documentation "多行之间的 gap。")
   (column-gap :initarg :column-gap :initform 0
               :type number :documentation "多列之间的 gap。")
   (items :initarg :items :type (list etml-flex-item)
          :documentation "子项目列表")
   (items-align
    :initarg :items-align :initform 'stretch :type symbol
    :documentation "所有 items 在交叉轴的对齐方式。"))
  "ETML flex layout model.")

;; (defun etml-item-total-units ()
;;   "返回的 item 的总宽度。"

;;   )

(defun etml-flex-parse-basis (item flex)
  ;; 对于左右方向排列的文本，宽度会影响高度，但是高度不会影响宽度
  ;; min(max)-width 优先级比 min(max)-content 高
  "返回 item 的基础宽度和最小宽度"
  (let* ((direction (oref flex direction))
         (basis (oref item basis))
         (block (oref item self))
         (content (oref block content))
         (item-min-units (seq-min (ekp-boxes-widths content)))
         curr-units min-units max-units)
    ;; basis 是 item 的初始长度，包含 margin, padding !
    (pcase direction
      ((or 'row 'row-reverse)
       (list (pcase basis
               ;; FIXME: number 需在最小和最大长度范围内
               ((pred 'numberp) basis)
               ('auto
                (setq curr-units (etml-block-total-pixel item)))
               ((or 'content 'max-content)
                (setq max-units
                      (+ (etml-block-side-pixel item)
                         (if-let ((max-width (oref item max-width)))
                             (min max-width
                                  (string-pixel-width content))
                           (string-pixel-width content)))))
               ('min-content
                (setq min-units
                      (+ (etml-block-side-pixel item)
                         (if-let ((min-width (oref item min-width)))
                             (max min-width item-min-units)
                           item-min-units))))
               ('fit-content
                (min max-units (max min-units curr-units))))
             min-units max-units))
      ((or 'column 'column-reverse)
       (list (pcase basis
               ;; FIXME: number 需在最小和最大长度范围内
               ((pred 'numberp) basis)
               ('auto
                (setq curr-units (etml-block-total-height item)))
               ((or 'content 'max-content)
                (oset item width nil)
                ;; `etml-block-total-height' 已经考虑了 max-height
                (setq max-units (etml-block-total-height item)))
               ('min-content
                (oset item width item-min-units)
                ;; `etml-block-total-height' 已经考虑了 min-height
                (setq min-units (etml-block-total-height item)))
               ('fit-content
                (min max-units (max min-units curr-units))))
             1 max-units)))))

(defun etml-flex--cross-edge-units (item flex)
  "根据 align，计算需要在 item 交叉轴开头和结尾增加的单元数。"
  (let* ((align (oref item cross-align))
         (align
          (if (or (null align) (eq 'auto align))
              (or (oref flex items-align) 'stretch)
            align))
         (direction (oref flex direction))
         ;; 交叉轴方向的总单位数
         (total-cross-units
          (pcase direction
            ((or 'row 'row-reverse)
             ;; flex height or max height in items
             (or (oref flex height)
                 (seq-max (mapcar #'etml-block-total-height
                                  (oref flex items)))))
            ((or 'column 'column-reverse)
             ;; flex width or max width in items
             (or (oref flex width)
                 (seq-max (mapcar #'etml-block-total-pixel
                                  (oref flex items)))))))
         (item-cross-units
          (pcase direction
            ((or 'row 'row-reverse)
             (etml-block-total-height item))
            ((or 'column 'column-reverse)
             (etml-block-total-pixel item)))))
    (pcase align
      ((or 'stretch 'normal)
       (let* ((rest-units (- total-cross-units item-cross-units))
              (start-units (/ rest-units 2))
              (end-units (- rest-units start-units)))
         (cons start-units end-units)))
      ('flex-start
       (cons 0 (- total-cross-units item-cross-units)))
      ('flex-end
       (cons (- total-cross-units item-cross-units) 0))
      ;; ('baseline
      ;;  ;; FIXME: 暂不支持，后续优化
      ;;  ;; 项目的第一行文本基线与容器中所有其他项目的基线对齐。
      ;;  ;; 若项目无文本，则以其底部边缘为基线。
      ;;  )
      )))

;; basis 决定 items 的基础宽度
;; grow or shrink

(defun etml-flex-items-grow (items-plists flex-units items-units
                                          direction)
  "按照 grow 拉伸并设置子项长度"
  (let* ((items (etml-plists-get items-plists :item))
         (rest-units (abs (- flex-units items-units)))
         (grows (etml-plists-get items-plists :grow))
         (grows-sum (apply '+ grows))
         (average-grow (if (> grows-sum 0)
                           (/ rest-units grows-sum)
                         0))
         (rest-num (if (> grows-sum 0)
                       (% rest-units grows-sum)
                     0))
         (rest-idx 0))
    ;; 设置 items 的主轴方向 grow 后的新的长度
    (seq-map-indexed
     (lambda (grow idx)
       (let* ((base-units (nth idx items-units))
              (item (nth idx items))
              final-units)
         (if (= grow 0)
             ;; grow=0 不拉伸
             (setq final-units base-units)
           ;; grow>0 按比例拉伸
           (setq final-units
                 (+ base-units (* grow average-grow)
                    (if (< rest-idx rest-num) 1 0)))
           (cl-incf rest-idx 1))
         ;; 按照方向设置 item 的新的主轴方向长度
         (pcase direction
           ;; 关键点:
           ;; 1. basis units 是 item block 总长度
           ;; 2. item block 的 width/height 属性是内容的长度
           ((or 'row 'row-reverse)
            (oset item width
                  (- final-units (etml-block-side-pixel item))))
           ((or 'column 'column-reverse)
            (oset item height
                  (- final-units (etml-block-side-height item)))))))
     grows)))

;; 当前最新的总宽度
;; (etml-flex-items-total-units items)

(defun etml-flex-items-shrink (items-plists flex-units items-units
                                            direction)
  "按照 shrink 缩减并设置子项长度"
  (let* ((items (etml-plists-get items-plists :item))
         ;; shrink 后不能小于 最小宽度 或 最小高度(1)
         (rest-units (- flex-units items-units))
         (min-units-lst (etml-plists-get items-plists :min-units))
         (shrinks (etml-plists-get items-plists :shrink))
         (shrinks-sum (apply '+ shrinks))
         (average-shrink (if (> shrinks-sum 0)
                             (/ rest-units shrinks-sum)
                           0))
         (rest-num (if (> shrinks-sum 0)
                       (% rest-units shrinks-sum)
                     0))
         (rest-idx 0))
    (seq-map-indexed
     (lambda (shrink idx)
       (let* ((base-units (nth idx items-units))
              (item (nth idx items))
              (min-units (nth idx min-units-lst))
              final-units)
         (if (= shrink 0)
             ;; shrink=0 不缩减
             (setq final-units base-units)
           ;; shrink>0 按比例缩减，但不能小于最小长度
           (setq final-units
                 ;; FIXME: 当缩减后小于最小长度时，
                 ;; 其余 items 重新分配这未能被缩减的长度
                 (max min-units
                      (- (- base-units (* shrink average-shrink))
                         (if (< rest-idx rest-num) 1 0))))
           (cl-incf rest-idx 1))
         (pcase direction
           ;; 关键点:
           ;; 1. basis units 是 item block 总长度
           ;; 2. item block 的 width/height 属性是内容的长度
           ((or 'row 'row-reverse) 
            (oset item width
                  (- final-units
                     (etml-block-side-pixel item))))
           ((or 'column 'column-reverse)
            (oset item height
                  (- final-units
                     (etml-block-side-height item)))))))
     shrinks)))

(defun etml-flex-items-adjust (items-plists flex)
  "根据容器长度、items长度、shrink、grow 等动态调整子项长度。"
  (let* ((flex-units (etml-flex-main-units flex))
         (direction (oref flex direction))
         (items-units (etml-flex-items-total-units flex)))
    ;; 容器长度 >= 子项总长度: 拉伸
    (if (>= flex-units items-units)
        (etml-flex-items-grow
         items-plists flex-units items-units direction)
      ;; 容器长度 < 子项总长度: 缩减
      (etml-flex-items-shrink
       items-plists flex-units items-units direction))))

(defun etml-flex-main-units (flex)
  "Flex 容器的内容部分的主轴长度"
  (pcase (oref flex direction)
    ((or 'row 'row-reverse)
     (or (etml-width-pixel (oref flex width))
         (etml-width-pixel (oref flex max-width))))
    ((or 'column 'column-reverse)
     (or (oref flex height)
         (oref flex max-height)))))

(defun etml-flex-items-total-units (flex)
  "获取当前左右子项最新的长度总和"
  (apply #'+ (mapcar (lambda (item)
                       (pcase direction
                         ((or 'row 'row-reverse)
                          (+ (etml-block-side-pixel item)
                             (etml-width-pixel (oref item width))))
                         ((or 'column 'column-reverse)
                          (+ (etml-block-side-height item)
                             (oref item height)))))
                     (oref flex items))))

(defun etml-flex-content-justify (items-num rest-units content-justify gap)
  ;; rest-units 是不包含基础 gap 的
  (pcase content-justify
    ('flex-start (append (make-list items-num gap) (list rest-units)))
    ('flex-end (append (list rest-units) (make-list items-num gap)))
    ('center (append (list (/ rest-units 2))
                     (make-list (1- items-num) gap)
                     (list (+ (/ rest-units 2)
                              (% rest-units 2)))))
    ('space-between
     (append (list 0)
             (etml-split-size rest-units (1- items-num) gap)
             (list 0)))
    ('space-around
     (let* ((around-units-lst
             (etml-split-size rest-units (* 2 items-num)))
            (start-units (seq-first around-units-lst))
            (end-units (seq-drop around-units-lst (1- (* 2 items-num))))
            (middle-units-lst (seq-subseq around-units-lst
                                          1 (1- (* 2 items-num)))))
       (append start-units
               ;; 合并中间的每2个，并加上 gap
               (mapcar (lambda (cons)
                         (+ gap (car cons) (or (cdr cons) 0)))
                       (seq-partition middle-units-lst 2))
               end-units)))
    ('space-evenly
     (etml-split-size rest-units (1+ items-num) gap 1 items-num))))

(defun etml-flex-render (flex)
  (let* ((display (oref flex display))
         (direction (oref flex direction))
         (content-justify (oref flex content-justify))
         (content-align (oref flex content-align))
         (items-align (oref flex items-align))
         (items (items (oref flex items)))
         ;; 根据 order 重新排列 items
         (items (seq-sort (lambda (item1 item2)
                            (< (oref item1 order)
                               (oref item2 order)))
                          items))
         (items-plists
          (mapcar (lambda (item)
                    (let ((lst (etml-flex-parse-basis item flex)))
                      (list :item item
                            :base-units (nth 0 lst)
                            :min-units (nth 1 lst)
                            :max-units (nth 2 lst)
                            :order (oref item order)
                            :grow (oref item grow)
                            :shrink (oref item shrink)
                            :align (oref item cross-align))))
                  items))
         (items-units-lst (etml-plists-get items-plists :base-units))
         ;; 当前使用 base-units 计算初始的总宽度
         ;; 后续动态调整中更多的使用实时计算出来的 items 总宽度
         (items-units (apply '+ items-units-lst))
         ;; 用于记录每行的 items 的数量
         (wrap-lst (list (length items-units-lst))))
    (if-let*
        ;; flex 容器设置了主轴方向的长度
        ((flex-units (etml-flex-main-units flex))
         (rest-units (- flex-units items-units)))
        (if (>= rest-units 0)
            ;; 容器长度 >= items总长度，考虑 grow，不换行
            (etml-flex-items-grow
             items-plists flex-units items-units direction)
          ;; 容器长度 < items总长度，考虑 shrink 和 换行
          (etml-flex-items-shrink
           items-plists flex-units items-units direction)
          (let* ((items-units (etml-flex-items-total-units items)))
            (unless (and (eq 'nowarp (oref flex wrap))
                         (<= items-units flex-units))
              ;; 缩减之后仍然超过容器长度的，且非 nowrap，
              ;; 计算换行点
              (setq wrap-lst (etml-flex-line-breaks
                              flex-units items-units-lst))
              (let ((prev 0))
                (dolist (num wrap-lst)
                  ;; 当前主轴方向的新的 items
                  (let ((sub-items-plists
                         (seq-subseq items-plists
                                     prev (+ prev num))))
                    ;; 重新计算每行的 item 长度
                    (etml-flex-items-adjust sub-items-plists flex)
                    (setq prev (+ prev num))))))))
      ;; flex 容器未设置主轴方向的长度，items 不换行
      ;; grow=0 不拉伸；grow>0 拉伸值最大宽度 (min max-width max-content)
      (let ((grows (etml-plists-get items-plists :grow))
            final-units)
        (seq-map-indexed
         (lambda (grow idx)
           (let ((item (nth idx items))
                 (items-plist (nth idx items-plists)))
             (if (= grow 0)
                 ;; 不拉伸，设置为 base-units
                 (setq final-units (plist-get items-plist :base-units))
               ;; 拉伸到 (min max-width max-content)
               (setq final-units (plist-get items-plist :max-units)))
             (pcase direction
               ((or 'row 'row-reverse)
                (oset item width
                      (- final-units
                         (etml-block-side-pixel item))))
               ((or 'column 'column-reverse)
                (oset item height
                      (- final-units
                         (etml-block-side-height item)))))))
         grows)))
    ;; FIXME: 还未考虑 row-gap, column-gap
    (let ((prev 0) justify-gaps align-gaps)
      (dolist (num wrap-lst)
        ;; 当前主轴方向的新的 items
        (let* ((sub-items-plists
                (seq-subseq items-plists
                            prev (+ prev num)))
               (items-num (length sub-items-plists))
               (flex-units (etml-flex-main-units flex))
               (items-units (etml-flex-items-total-units flex)))
          (if (> flex-units items-units)
              ;; 每个主轴方向有剩余空间时，考虑 content-justify
              (push (etml-flex-content-justify
                     items-num
                     (- flex-units items-units)
                     content-justify 0) ;; FIXME: 考虑 gap
                    justify-gaps)
            ;; FIXME: shoud add gap
            (make-list (1+ items-num) 0))
          (setq prev (+ prev num)))))

    ;; consider content-align
    
    ;; consider cross-align: 
    ;; (etml-flex--cross-edge-units item flex)
    ;; 非 scretch 时，连接的后的 items 的高度可能不一致
    ;; 但是 block-concat 默认会使用空白像素行填充
    ;; 此时换行的 item 排列需要使用 block-cut 嵌入到短的 items 里
    ;; (记录每个 item 的宽和高，用于瀑布流的排列！)
    ;; 如果宽度允许嵌入的话
    
    ;; 最后连接 items block 时，考虑 direction, wrap 的方向
    ))

(provide 'etml-flex)
