;; -*- lexical-binding: t -*-

(require 'block-pixel)

(defun block-background-type ()
  (frame-parameter nil 'background-mode))

(defun block-atom-consp (obj)
  "只有两个元素的 cons 类型判断"
  (and (consp obj)
       (cdr obj)
       (atom (cdr obj))))

(defun block-propertize (string properties
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

(defun block-width-pixel (width)
  (cond
   ((consp width) (car width))
   ((numberp width) (* width (string-pixel-width " ")))
   (t (error "Invalid format of width %S" width))))

(defun block-pixel-border (n-pixel height &optional color)
  (if (or (= 0 n-pixel) (= 0 height))
      ""
    (let* ((color (if (eq t color)
                      (block-default-border-color)
                    (or color (block-default-border-color))))
           (string (propertize
                    " " 'face
                    `(:inverse-video t :foreground ,color)
                    'display `(space :width (,n-pixel)))))
      (block-string-duplines string height))))

(defun block-string-nchar-pixel (string n &optional from-end)
  "Return the pixel width of n char in STRING from start
by default. If FROM-END is non-nil, count from the end."
  (let ((strlen (length string)))
    (unless (< strlen n)
      (if from-end
          (string-pixel-width (substring string (- strlen n)))
        (string-pixel-width (substring string 0 n))))))

(defun block-default-border-color ()
  (frame-parameter nil 'foreground-color))

(defun block-blank (pixel &optional height)
  (if (= pixel 0)
      ""
    (block-string-duplines
     (propertize " " 'display `(space :width (,pixel)))
     (or height 1))))

(defun block-string-linum (string &optional omit-nulls)
  (length (split-string string "\n" omit-nulls)))

(defun block-string-join (sequences)
  (string-join sequences "\n"))

(defun block-string-duplines (string length)
  (string-join (make-list length string) "\n"))

(defun block-lines-pad (string height &optional offset padstr)
  "Pad the string to a height of HEIGHT, with the string offset by OFFSET
lines from the top, using PADSTR to fill blank lines."
  (if-let* ((linum (block-string-linum string))
            ((> height linum))
            (rest (- height linum))
            (top (block-line-offset rest (or offset 0)))
            (bottom (- rest top))
            (padstr (or padstr ""))
            (top-string
             (if (> top 0)
                 (concat (block-string-duplines padstr top) "\n")
               ""))
            (bottom-string
             (if (> bottom 0)
                 (concat "\n" (block-string-duplines padstr bottom))
               "")))
      (concat top-string string bottom-string)
    string))

(defun block-string-concat (&rest strings)
  (let* ((height (-max (-map #'block-string-linum strings)))
         (strings (mapcar (lambda (string)
                            (block-lines-pad string height))
                          strings)))
    (block-string-join
     (apply 'cl-mapcar #'concat
            (mapcar (lambda (string)
                      (block-maplines 'identity string))
                    strings)))))

(defun block-map-collect (list map-func collect-func
                               &optional arglist)
  "Apply MAP-FUNC to LIST, then apply COLLECT-FUNC with ARGLIST
to the result of map function.

The first argument of COLLECT-FUNC function is a list and the rest
are ARGLIST."
  (apply collect-func (mapcar map-func list) (or arglist '())))

(defun block-maplines (function string &optional collect-func &rest arglist)
  "Apply FUNCTION to each line of STRING. If COLLECT-FUNC is non-nil,
apply this function to the list and ARGLIST are arguments of this
COLLECT-FUNC; otherwise return a list of lines."
  (let ((lines (split-string string "\n")))
    (if collect-func
        (block-map-collect lines function collect-func arglist)
      (mapcar function lines))))

(defun block-line-offset (total offset)
  "When OFFSET is a positive number, there is a offset pixel distance
from start. When OFFSET is a negative number, there is a OFFSET pixel
from the end. Return the offset from start."
  (cond ((>= offset 0) (min offset total))
        ((< offset 0) (max 0 (+ offset total)))))

(defun block-line-align-offset (total justify-or-align)
  "Return the offset when align with JUSTIFY-OR-ALIGN."
  (pcase justify-or-align
    ((or 'left 'top) (block-line-offset total 0))
    ((or 'right 'bottom) (block-line-offset total total))
    ('center (block-line-offset total (/ total 2)))
    (_ (error "Invalid format of align: %S" align))))

(defun block-lines-justify (string pixel &optional justify)
  "Justify each line in STRING to PIXEL by JUSTIFY, JUSTIFY
should be one of the symbols 'left', 'center', 'right'.
When JUSTIFY is nil, set it to 'left' by default."
  (let ((justify (or justify 'left)))
    (block-maplines (lambda (line)
                      (block-pixel-typeset line pixel justify))
                    string 'string-join "\n")))

;; (defun block-lines-justify (string pixel &optional justify)
;;   "Justify each line in STRING to PIXEL by JUSTIFY, JUSTIFY
;; should be one of the symbols 'left', 'center', 'right'.
;; When JUSTIFY is nil, set it to 'left' by default."
;;   (let ((justify (or justify 'left)))
;;     (block-pixel-typeset  pixel justify)))

(defun block-lines-align (string height &optional align)
  (block-lines-pad
   string height
   (block-line-align-offset
    (- height (block-string-linum string)) (or align 'top))))

(defun block-lines-concat (strings &optional align text-align)
  "TEXT-ALIGN should be one of left,center,right.
ALIGN should be one of top,center,bottom."
  (let ((max-height (-max (-map #'block-string-linum strings))))
    (apply 'block-string-concat
           (mapcar (lambda (string)
                     (block-lines-justify
                      (block-lines-align string max-height align)
                      (string-pixel-width string)
                      text-align))
                   strings))))

(defun block-lines-stack (strings &optional align text-align)
  "ALIGN used for all blocks, TEXT-ALIGN used for text in a block."
  (let ((max-width (-max (-map #'string-pixel-width strings))))
    (mapconcat (lambda (string)
                 (block-lines-justify
                  ;; 再 justify 整个 block
                  (block-lines-justify
                   ;; 先 justify 文本
                   string (string-pixel-width string) text-align)
                  max-width align))
               strings "\n")))

(provide 'block-utils)
