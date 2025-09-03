;; -*- lexical-binding: t -*-

(require 'eieio)
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
   (align :initarg :align :initform 'auto :type symbol
          :documentation "item 在交叉轴的对齐方式，覆盖容器的 items-align 属性。")))

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
   (items :initarg :items :type (list etml-flex-item)
          :documentation "子项目列表")
   (items-align
    :initarg :items-align :initform 'stretch :type symbol
    :documentation "所有 items 在交叉轴的对齐方式。"))
  "ETML flex layout model.")

(defun etml-flex-parse-basis (item)
  "Parse basis of ITEM to pixel width."
  (let* ((basis (oref item basis))
         (block (oref item self))
         (content (oref block content))
         curr-content-pixel
         min-content-pixel
         max-content-pixel)
    (cond
     ((numberp basis) basis)
     ((eq 'auto basis)
      (setq curr-content-pixel
            (etml-block-total-pixel item)))
     ((or (eq 'content basis)
          (eq 'max-content basis))
      (setq max-content-pixel
            (string-pixel-width content)))
     ;; FIXME: if word with hyphenate
     ((eq 'min-content basis)
      (setq min-content-pixel
            (+ (etml-block-side-pixel item)
               (seq-min (ekp-boxes-widths content)))))
     ((eq 'fit-content basis)
      (min max-content-pixel
           (max min-content-pixel curr-content-pixel))))))

(defun etml-flex-render (flex)
  (let* ((display (oref flex display))
         (direction (oref flex direction))
         (wrap (oref flex wrap))
         (content-justify (oref flex content-justify))
         (content-align (oref flex content-align))
         (items-align (oref flex items-align))
         (items (oref flex items))
         (items-data-lst
          (mapcar (lambda (item)
                    (list :pixel (etml-flex-parse-basis item)
                          :order (oref item order)
                          :grow (oref item grow)
                          :shrink (oref item shrink)
                          :align (oref item align)))
                  items))
         (blocks (oref flex items))
         (num (length items-data-lst))
         (items-order (oref flex items-align))
         ;; 单个值转换为每个block的值
         (items-align (let ((aligns (oref flex items-align)))
                        (if (vectorp aligns)
                            aligns
                          (make-vector num aligns))))
         (items-flex
          (let ((flexs (oref flex items-flex)))
            (cond
             ((symbolp flexs)
              (pcase flexs
                ('none '(0 0 auto))
                ('auto '(1 1 auto))
                (_ (error "Invalid format of items-flex!"))))
             ((vectorp flexs) flexs)
             ((consp flexs) (make-vector num flexs)))))
         (blocks-widths
          ;; if flex-basis is number or cons-cell
          ;; use it to override block total width
          (seq-map-indexed
           (lambda (item-flex idx)
             (let ((basis (nth 2 item-flex))
                   (block (aref blocks idx)))
               (cond
                ((eq 'auto basis)
                 ;; 保持原始宽度
                 (etml-block-total-pixel block))
                ((integerp basis)
                 ;; 指定个数的字符宽度
                 (oset block width basis)
                 (etml-block-total-pixel block))
                ((consp basis)
                 ;; 指定像素宽度
                 (oset block width (list basis))
                 basis))))
           items-flex))
         (blocks-width (apply #'+ blocks-widths))
         ;; FIXME: support both nchar and pixel width
         ;; convert to pixel width
         (flex-width (oref flex width)))
    ;; If flex block has width:
    ;;   consider grow or shrink width of blocks
    ;; If flex block hasn't width:
    ;;   keep the original width of blocks
    (when flex-width
      (if (<= blocks-width flex-width)
          ;; If total width of all blocks <= flex block width,
          ;; should consider grow in item-flex
          (let* ((rest-pixel (- flex-width blocks-widths))
                 (items-grows (seq-map (lambda (item-flex)
                                         (nth 0 item-flex))
                                       items-flex))
                 (total-grows (apply #'+ items-grows))
                 (average-pixel (unless (= total-grows 0)
                                  (/ rest-pixel total-grows)))
                 (pixel-n (unless (= total-grows 0)
                            (% rest-pixel total-grows))))
            ;; set the pixel width of all blocks by items-grows
            (when (and (> rest-pixel 0) average-pixel)
              (seq-map-indexed
               (lambda (item-grow idx)
                 (let* ((block (aref blocks idx))
                        (pixel (etml-block-total-pixel block)))
                   (when (> item-grow 0)
                     ;; item-grow = 0: keep the original
                     ;; item-grow > 0: grow by ratio
                     (setq pixel (+ pixel
                                    (* average-pixel item-grow)
                                    (if (< idx pixel-n) 1 0)))
                     (oset block width (list pixel)))))
               items-grows)))
        ;; total width of all blocks > flex block width
        ;; should consider shrink in item-flex
        (let* ((rest-pixel (- blocks-widths flex-width))
               (items-shrinks (seq-map (lambda (item-flex)
                                         (nth 1 item-flex))
                                       items-flex))
               (total-shrinks (apply #'+ items-shrinks))
               (average-pixel (unless (= total-shrinks 0)
                                (/ rest-pixel total-shrinks)))
               (pixel-n (unless (= total-shrinks 0)
                          (% rest-pixel total-shrinks))))
          ;; set the pixel width of all blocks by items-shrinks
          (when (and (> rest-pixel 0) average-pixel)
            (seq-map-indexed
             (lambda (item-shrink idx)
               (let* ((block (aref blocks idx))
                      (pixel (etml-block-total-pixel block)))
                 (when (> item-shrink 0)
                   ;; item-shrink = 0: keep the original
                   ;; item-shrink > 0: shrink by ratio
                   (setq pixel (+ pixel
                                  (* average-pixel item-shrink)
                                  (if (< idx pixel-n) 1 0)))
                   (oset block width (list pixel)))))
             items-shrinks))))
      ;; After shrink, if total width of blocks still less
      ;; than flex width, consider content-justify,
      ;; consider wrap
      ;; block 已设置 min-width
      )
    ))


;; (etml-flex :items-flex [(0 1 auto) (0 2 auto)]
;;            :items-shrink [1 2 3]
;;            :items-basis [auto 1 2]
;;            :items-align [baseline auto]
;;            )
