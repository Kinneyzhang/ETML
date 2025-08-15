;; -*- lexical-binding: t -*-

(require 'etml-pixel)

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

(defun etml--num-in-prefixs (num lst)
  "Find the position of the smallest number in LST
 that is greater than NUM. Position counts from 1.
If NUM is greater than all elements, return (1+ (length LST))."
  (cond
   ((= num 0) 0)
   ((> num (car (last lst))) (length lst))
   (t (let ((pos 1))
        (while (and lst (< (car lst) num))
          (setq lst (cdr lst))
          (setq pos (1+ pos)))
        pos))))

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

(provide 'etml-utils)
