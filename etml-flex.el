(require 'eieio)

(defclass etml-flex ()
  ((display
    :initarg :display :initform 'flex :type symbol
    :documentation "Display type: flex or inline-flex")
   (direction
    :initarg :direction :initform 'row :type symbol
    :documentation "主轴方向: row, row-reverse, column,\
 column-reverse")
   (wrap
    :initarg :wrap :initform 'nowrap :type symbol
    :documentation "换行方式: nowrap, wrap, wrap-reverse")
   (content-justify
    :initarg :content-justify :initform 'flex-start :type symbol
    :documentation "主轴对齐: flex-start, flex-end, center,\
 space-between, space-around, space-evenly")
   (content-align
    :initarg :content-align :initform 'stretch :type symbol
    :documentation "多行对齐: stretch, flex-start, flex-end,\
 center, space-between, space-around")
   (items-align
    :initarg :items-align :initform 'stretch :type symbol
    :documentation "交叉轴对齐: flex-start, flex-end, center,\
 baseline, stretch")
   (items :initarg :items :initform nil :type list
          :documentation "包含的子项目列表"))
  "ETML flex layout model.")

(defun etml-flex-render (flex)
  (let ((blocks (oref flex :items)))
    
    ))
