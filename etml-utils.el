;; -*- lexical-binding: t -*-

(require 'etml-pixel)

(defun etml-split-size (size n &optional extra start end)
  "将 SIZE 分为 N 等份，并且在 start 到 end 位置加上额外的数字 extra"
  (let ((each-size (/ size n))
        (rest-num (% size n)))
    (seq-map-indexed (lambda (size idx)
                       (let ((s (if (< idx rest-num)
                                    (1+ size)
                                  size))
                             (start (or start 0))
                             (end (or end n)))
                         (if (and extra (<= start idx) (> end idx))
                             (+ extra s)
                           s)))
                     (make-list n each-size))))

(defun etml-plists-get (plist-seq prop)
  (mapcar (lambda (plst)
            (plist-get plst prop))
          plist-seq))

(defun etml-background-type ()
  (frame-parameter nil 'background-mode))

(defun etml-atom-consp (obj)
  "只有两个元素的 cons 类型判断"
  (and (consp obj)
       (cdr obj)
       (atom (cdr obj))
       (atom (car obj))))

(defun etml-propertize (string properties
                               &optional start end)
  "不会覆盖原有的属性，返回新的字符串。"
  ;; 防止是 make-list 创建的元素，它们都属于同一个对象
  ;; 最好先复制一份字符串
  (let* ((string (copy-sequence string))
         (start (or start 0))
         (end (or end (length string))))
    (while properties
      (let ((prop (pop properties))
            (value (pop properties)))
        (pcase prop
          ('face (add-face-text-property
                  start end value t string))
          ('display (add-display-text-property
                     start end (car value) (cadr value)
                     string))
          (_ (put-text-property
              start end prop value string)))))
    string))

(defun etml-remove-face-attributes (start end removed-attributes)
  "将 scroll bar 的 face 清空，变为像素空格"
  ;; 局限性: 用 start 的 face 属性处理之后，渲染到所有字符上面
  ;; 因此只能来处理取消 scroll bar 的属性
  ;; 先取出 face properties
  (let ((face-properties (get-text-property start 'face))
        not-kv-faces)
    (when (consp face-properties)
      (let ((face-properties (seq-mapcat (lambda (elem)
                                           (if (consp elem)
                                               elem
                                             (list elem)))
                                         face-properties)))
        ;; face-properties 开头可能会有非 kv 形式的face，需要保留
        (catch 'break
          (while-let ((not-kv-face (car face-properties)))
            (if (not (keywordp not-kv-face))
                (progn
                  (when (facep not-kv-face)
                    (push not-kv-face not-kv-faces))
                  (pop face-properties))
              (throw 'break nil))))
        ;; 剩下的是一个 plist
        (dolist (attribute removed-attributes)
          (plist-put face-properties attribute nil))
        ;; 合并 not-kv-faces 和 kv-faces
        (add-text-properties
         start end
         `(face ,(append not-kv-faces face-properties)))))))

;; (defun etml-add-text-properties (start end properties)
;;   "前提是 start 和 end 之间的所有文本属性相同，支持将属性的中属性值清空。
;; 比如 '(face (:inverse-video nil :foreground nil))"
;;   (while properties
;;     (let ((prop (pop properties))
;;           (value (pop properties)))
;;       (let ((ori-value (get-text-property start prop)))
;;         ;; 去除前面的非 plist 属性部分
;;         ;; value 和 ori-values 比对合并
;;         (if (consp ori-props)
;;             (seq-mapcat (lambda (elem)
;;                           (if (consp elem)
;;                               elem
;;                             (list elem)))
;;                         ori-props)
;;           ori-value)
;;         value
;;         )
;;       (cond
;;        ((eq prop 'face))
;;        )
;;       )))

(defun etml-width-pixel (width &optional content)
  "Return the pixel of etml width. If WIDTH is a cons-cell,
 use the car of it as pixel. If WIDTH is a integer, use the
 pixel width number of chars in CONTENT as pixel. If the number
 of chars in CONTENT is less than WIDTH, use pixel of space to
 pad the rest width."
  (cond
   ((consp width) (car width))
   ((numberp width)
    (if content
        (or (etml-string-nchar-pixel content width)
            (+ (string-pixel-width content)
               ;; 内容小于 width 个字符时，用空格补齐剩余字符计算像素
               (* (- width (length content))
                  (string-pixel-width " "))))
      (* width (string-pixel-width " "))))
   (t (error "Invalid format of width %S" width))))

(defun etml-default-border-color ()
  (frame-parameter nil 'foreground-color))

(defun etml-pixel-border (n-pixel height &optional color)
  (if (or (= 0 n-pixel) (= 0 height))
      ""
    (let* ((color (if (eq t color)
                      (etml-default-border-color)
                    (or color (etml-default-border-color))))
           (string (propertize
                    " " 'face
                    `(:inverse-video t :foreground ,color)
                    'display `(space :width (,n-pixel)))))
      (etml-string-duplines string height))))

(defun etml--num-in-prefixs (num prefixs)
  "Find the position of the smallest number in PREFIXS
 that is greater than NUM. Position counts from 1.
If NUM is greater than all elements, return (1+ (length PREFIXS))."
  (cond
   ((= num 0) 0)
   ((> num (car (last prefixs))) (length prefixs))
   (t (let ((pos 1))
        (while (and prefixs (< (car prefixs) num))
          (setq prefixs (cdr prefixs))
          (setq pos (1+ pos)))
        pos))))

(defun etml--num-in-lst (num lst)
  "计算 NUM 落在 LST 中的第几个元素的前缀和区间内。"
  (let* ((prefix 0)
         ;; 计算前缀和
         (prefixs (mapcar (lambda (n)
                            (setq prefix (+ prefix n)))
                          lst)))
    (etml--num-in-prefixs num prefixs)))

(defun etml-flex-line-breaks (flex-units items-units-lst)
  "计算 FLEX 布局中每行的元素个数。
FLEX-UNITS 是容器最大宽度，ITEMS-UNITS-LST 是项目宽度列表。
返回每行元素个数的列表。"
  (let ((current-sum 0)
        (current-count 0)
        (result '())
        (index 0)
        (len (length items-units-lst)))
    (while (< index len)
      (let ((current-width (nth index items-units-lst)))
        ;; 如果当前项目本身已经超过或等于最大宽度，它必须单独成行
        (if (>= current-width flex-units)
            (progn
              ;; 如果当前已经有累计的项目，先记录它们
              (when (> current-count 0)
                (setq result (cons current-count result))
                (setq current-sum 0)
                (setq current-count 0))
              ;; 记录这个超大项目
              (setq result (cons 1 result))
              (setq index (1+ index)))
          ;; 常规情况：尝试将当前项目添加到当前行
          (if (<= (+ current-sum current-width) flex-units)
              (progn
                (setq current-sum (+ current-sum current-width))
                (setq current-count (1+ current-count))
                (setq index (1+ index)))
            ;; 当前行已满，记录当前行的项目数
            (setq result (cons current-count result))
            (setq current-sum 0)
            (setq current-count 0)))))
    ;; 处理最后一行（如果有未记录的项目）
    (when (> current-count 0)
      (setq result (cons current-count result)))
    ;; 返回反转后的结果（顺序与行的顺序一致）
    (reverse result)))

(defun etml-string-nchar-pixel (string n &optional from-end)
  "Return the pixel width of n char in STRING from start
by default. If FROM-END is non-nil, count from the end."
  (let ((strlen (length string)))
    (unless (< strlen n)
      (if from-end
          (string-pixel-width (substring string (- strlen n)))
        (string-pixel-width (substring string 0 n))))))

(defun etml-block-blank (pixel &optional height)
  (if (= pixel 0)
      ""
    (etml-string-duplines
     (propertize " " 'display `(space :width (,pixel)))
     (or height 1))))

(defun etml-string-linum (string &optional omit-nulls)
  (length (split-string string "\n" omit-nulls)))

(defun etml-string-join (sequences)
  (string-join sequences "\n"))

(defun etml-string-duplines (string length)
  (string-join (make-list length string) "\n"))

(defun etml-lines-pad (string height &optional offset padstr)
  "Pad the string to a height of HEIGHT, with the string offset by OFFSET
lines from the top, using PADSTR to fill blank lines."
  (if-let* ((linum (etml-string-linum string))
            ((> height linum))
            (rest (- height linum))
            (top (etml-line-offset rest (or offset 0)))
            (bottom (- rest top))
            (padstr (or padstr ""))
            (top-string
             (if (> top 0)
                 (concat (etml-string-duplines padstr top) "\n")
               ""))
            (bottom-string
             (if (> bottom 0)
                 (concat "\n" (etml-string-duplines padstr bottom))
               "")))
      (concat top-string string bottom-string)
    string))

(defun etml-string-concat (&rest strings)
  (let* ((height (-max (-map #'etml-string-linum strings)))
         (strings (mapcar (lambda (string)
                            (etml-lines-pad string height))
                          strings)))
    (etml-string-join
     (apply 'cl-mapcar #'concat
            (mapcar (lambda (string)
                      (etml-maplines 'identity string))
                    strings)))))

(defun etml-map-collect (list map-func collect-func
                              &optional arglist)
  "Apply MAP-FUNC to LIST, then apply COLLECT-FUNC with ARGLIST
to the result of map function.

The first argument of COLLECT-FUNC function is a list and the rest
are ARGLIST."
  (apply collect-func (mapcar map-func list) (or arglist '())))

(defun etml-maplines (function string &optional collect-func &rest arglist)
  "Apply FUNCTION to each line of STRING. If COLLECT-FUNC is non-nil,
apply this function to the list and ARGLIST are arguments of this
COLLECT-FUNC; otherwise return a list of lines."
  (let ((lines (split-string string "\n")))
    (if collect-func
        (etml-map-collect lines function collect-func arglist)
      (mapcar function lines))))

(defun etml-line-offset (total offset)
  "When OFFSET is a positive number, there is a offset pixel distance
from start. When OFFSET is a negative number, there is a OFFSET pixel
from the end. Return the offset from start."
  (cond ((>= offset 0) (min offset total))
        ((< offset 0) (max 0 (+ offset total)))))

(defun etml-line-align-offset (total justify-or-align)
  "Return the offset when align with JUSTIFY-OR-ALIGN."
  (pcase justify-or-align
    ((or 'left 'top) (etml-line-offset total 0))
    ((or 'right 'bottom) (etml-line-offset total total))
    ('center (etml-line-offset total (/ total 2)))
    (_ (error "Invalid format of align: %S" align))))

(defun etml-lines-justify (string pixel &optional justify)
  "Justify each line in STRING to PIXEL by JUSTIFY, JUSTIFY
should be one of the symbols 'left', 'center', 'right'.
When JUSTIFY is nil, set it to 'left' by default."
  (let ((justify (or justify 'left)))
    (etml-maplines (lambda (line)
                     (etml-pixel-typeset line pixel justify))
                   string 'string-join "\n")))

(defun etml-lines-align (string height &optional align)
  (etml-lines-pad
   string height
   (etml-line-align-offset
    (- height (etml-string-linum string)) (or align 'top))))

(defun etml-lines-concat (strings &optional align text-align)
  "TEXT-ALIGN should be one of left,center,right.
ALIGN should be one of top,center,bottom."
  (let ((max-height (-max (-map #'etml-string-linum strings))))
    (apply 'etml-string-concat
           (mapcar (lambda (string)
                     (etml-lines-justify
                      (etml-lines-align string max-height align)
                      (string-pixel-width string)
                      text-align))
                   strings))))

(defun etml-lines-stack (strings &optional align text-align)
  "ALIGN used for all blocks, TEXT-ALIGN used for text in a block."
  (let ((max-width (-max (-map #'string-pixel-width strings))))
    (mapconcat (lambda (string)
                 (etml-lines-justify
                  ;; 再 justify 整个 block
                  (etml-lines-justify
                   ;; 先 justify 文本
                   string (string-pixel-width string) text-align)
                  max-width align))
               strings "\n")))

(defun etml-frame-bgcolor ()
  (face-attribute 'default :background))

(provide 'etml-utils)
