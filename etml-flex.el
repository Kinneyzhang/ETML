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

(defclass etml-flex (etml-block)
  ((display :initarg :display :initform 'flex :type symbol
            :documentation "Display type: flex or inline-flex")
   (direction :initarg :direction :initform 'row :type symbol
              :documentation "主轴方向: row, row-reverse, column,\
 column-reverse")
   (wrap :initarg :wrap :initform 'nowrap :type symbol
         :documentation "换行方式: nowrap, wrap, wrap-reverse")
   (content-justify
    :initarg :content-justify :initform 'flex-start :type symbol
    :documentation "主轴对齐: flex-start, flex-end, center,\
 space-between, space-around, space-evenly")
   (content-align
    :initarg :content-align :initform 'stretch :type symbol
    :documentation "多行对齐: stretch, flex-start, flex-end,\
 center, space-between, space-around")
   ;; (items-align
   ;;    :initarg :items-align :initform 'stretch :type symbol
   ;;    :documentation "交叉轴对齐: flex-start, flex-end, center,\
   ;; baseline, stretch")
   (items :initarg :items :type (vector etml-block)
          :documentation "包含的子项目列表")
   ;; 使用 下面的选项为将每个 item 设置。单个值，表示应用到所有 items 上；
   ;; 列表表示分别应用应用到每个 items
   (items-order :initarg :items-order :initform nil
                :type (or null (vector integer))
                :documentation "nil means use the default order.")
   (items-flex :initarg :items-flex :initform 'none
               :type (satisfies etml-flex-items-flex-p)
               :documentation "合并 flex-grow, flex-shrink,\
 flex-basis。flex: [grow] [shrink] [basis]. 'auto means (1 1 auto),
 'none means (0 0 auto).")
   ;; (item-grow :initarg :item-grow :initform 0
   ;;              :type (satisfies etml-flex-number-vector-p)
   ;;              :documentation "定义项目在​​容器有剩余空间时​​的​​放大比例​​。\
   ;; 非负数字，默认值为 0（不放大）。")
   ;;   (item-shrink :initarg :item-shrink :initform 1
   ;;                :type (satisfies etml-flex-number-vector-p)
   ;;                :documentation "定义项目在​​容器空间不足时​​的​​缩小比例。\
   ;; 非负数字，默认值为 1（允许缩小）。")
   ;;   (item-basis :initarg :item-basis :initform 'auto
   ;;               :type (satisfies etml-flex-number-auto-vector-p)
   ;;               :documentation "定义项目在​​分配空间前​​的​​初始尺寸。\
   ;; auto（默认，项目内容大小）或具体值（px, %, em等）。")
   (items-align :initarg :items-align :initform 'auto
                :type (satisfies etml-flex-items-align-p)
                :documentation "覆盖容器的 align-items属性，控制单个\
 项目的​​垂直。对齐方式支持: auto / flex-start / flex-end / center /\
 baseline,stretch."))
  "ETML flex layout model.")

(defun etml-flex-render (flex)
  (let* ((display (oref flex display))
         (direction (oref flex direction))
         (wrap (oref flex wrap))
         (content-justify (oref flex content-justify))
         (content-align (oref flex content-align))
         ;; FIXME:已有 item-flex 则不再使用
         ;; item-grow, item-shrink, item-basis
         ;; (item-grow (oref flex item-grow))
         ;; (item-shrink (oref flex item-shrink))
         ;; (item-basis (oref flex item-basis))
         ;; items are etml-block objects
         (blocks (oref flex items))
         (num (length blocks))
         (items-order (oref flex items-align))
         ;; 单个值转换为每个block的值
         (items-align (let ((aligns (oref flex items-align)))
                        (if (vectorp aligns)
                            aligns
                          (make-vector num aligns))))
         (items-flex (let ((flexs (oref flex items-flex)))
                       (if (vectorp flexs)
                           flexs
                         (make-vector num flexs))))
         (blocks-widths
          ;; if flex-basis is number or cons-cell
          ;; use it to override block total width
          (seq-map-indexed
           (lambda (item-flex idx)
             (let ((basis (nth 2 item-flex))
                   (block (aref blocks idx)))
               (cond
                ((eq 'auto basis)
                 (etml-block-total-pixel block))
                ((integerp basis)
                 (oset block width basis)
                 (etml-block-total-pixel block))
                ((consp basis)
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
