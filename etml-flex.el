(require 'eieio)

;; 定义 Flex 容器类
(defclass css-flex-container ()
  ((display :initarg :display
            :initform 'flex
            :type symbol
            :documentation "Display type: flex or inline-flex")
   (flex-direction :initarg :direction
                   :initform 'row
                   :type symbol
                   :documentation "主轴方向: row, row-reverse, column, column-reverse")
   (flex-wrap :initarg :wrap
              :initform 'nowrap
              :type symbol
              :documentation "换行方式: nowrap, wrap, wrap-reverse")
   (justify-content :initarg :justify
                    :initform 'flex-start
                    :type symbol
                    :documentation "主轴对齐: flex-start, flex-end, center, space-between, space-around, space-evenly")
   (align-items :initarg :align
                :initform 'stretch
                :type symbol
                :documentation "交叉轴对齐: stretch, flex-start, flex-end, center, baseline")
   (align-content :initarg :align-content
                  :initform 'stretch
                  :type symbol
                  :documentation "多行对齐: stretch, flex-start, flex-end, center, space-between, space-around")
   (items :initarg :items
          :initform nil
          :type list
          :documentation "包含的子项目列表"))
  "CSS Flex 布局容器类")

;; 定义 Flex 项目类
(defclass css-flex-item ()
  ((order :initarg :order
          :initform 0
          :type integer
          :documentation "项目排序")
   (flex-grow :initarg :grow
              :initform 0
              :type number
              :documentation "放大比例")
   (flex-shrink :initarg :shrink
                :initform 1
                :type number
                :documentation "缩小比例")
   (flex-basis :initarg :basis
               :initform 'auto
               :type (or number symbol)
               :documentation "初始大小: auto, <number>px, <number>%, content")
   (flex :initarg :flex
         :initform nil
         :type list
         :documentation "简写属性: (grow shrink basis)")
   (align-self :initarg :align-self
               :initform 'auto
               :type symbol
               :documentation "单独对齐方式: auto, flex-start, flex-end, center, baseline, stretch")
   (content :initarg :content
            :initform ""
            :type string
            :documentation "项目内容"))
  "CSS Flex 布局项目类")

;; 辅助函数：验证属性值合法性
(defun validate-flex-value (prop value valid-values)
  "验证 Flex 属性值是否合法"
  (unless (memq value valid-values)
    (error "Invalid value `%s` for property `%s`" value prop))
  t)

;; 重定义 init 方法进行属性验证
(cl-defmethod initialize-instance :after ((cont css-flex-container) &rest _)
  (with-slots (display flex-direction flex-wrap justify-content align-items align-content) cont
    (validate-flex-value 'display display '(flex inline-flex))
    (validate-flex-value 'flex-direction flex-direction '(row row-reverse column column-reverse))
    (validate-flex-value 'flex-wrap flex-wrap '(nowrap wrap wrap-reverse))
    (validate-flex-value 'justify-content justify-content 
                         '(flex-start flex-end center space-between space-around space-evenly))
    (validate-flex-value 'align-items align-items 
                         '(stretch flex-start flex-end center baseline))
    (validate-flex-value 'align-content align-content 
                         '(stretch flex-start flex-end center space-between space-around))))

;; 添加项目到容器的方法
(cl-defmethod add-flex-item ((cont css-flex-container) item)
  "添加项目到 Flex 容器"
  (if (object-of-class-p item 'css-flex-item)
      (push item (oref cont items))
    (error "Not a valid flex item: %S" item))
  item)

;; 生成 CSS 代码的方法
(cl-defmethod generate-flex-css ((cont css-flex-container))
  "生成 Flex 容器的 CSS 代码"
  (with-slots (display flex-direction flex-wrap justify-content align-items align-content) cont
    (concat 
     (format ".container {\n  display: %s;\n" display)
     (format "  flex-direction: %s;\n" flex-direction)
     (format "  flex-wrap: %s;\n" flex-wrap)
     (format "  justify-content: %s;\n" justify-content)
     (format "  align-items: %s;\n" align-items)
     (when (and (memq flex-wrap '(wrap wrap-reverse)) 
                (not (eq align-content 'stretch)))
       (format "  align-content: %s;\n" align-content))
     "}\n\n")))

(cl-defmethod generate-flex-css ((item css-flex-item))
  "生成 Flex 项目的 CSS 代码"
  (with-slots (order flex-grow flex-shrink flex-basis flex align-self) item
    (concat
     ".item {\n"
     (format "  order: %d;\n" order)
     (if flex
         (format "  flex: %d %d %s;\n" (nth 0 flex) (nth 1 flex) (nth 2 flex))
       (concat
        (format "  flex-grow: %d;\n" flex-grow)
        (format "  flex-shrink: %d;\n" flex-shrink)
        (format "  flex-basis: %s;\n" flex-basis)))
     (unless (eq align-self 'auto)
       (format "  align-self: %s;\n" align-self))
     "}\n")))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; 创建 Flex 容器实例
(setq my-container (make-instance 'css-flex-container
                                  :display 'flex
                                  :direction 'row
                                  :wrap 'wrap
                                  :justify 'space-between
                                  :align 'center))

;; 创建 Flex 项目实例
(setq item1 (make-instance 'css-flex-item
                           :grow 1
                           :basis 200
                           :align-self 'flex-start))

(setq item2 (make-instance 'css-flex-item
                           :grow 2
                           :shrink 0
                           :order 1
                           :flex '(2 1 auto)))

;; 添加项目到容器
(add-flex-item my-container item1)
(add-flex-item my-container item2)

;; 生成 CSS 代码
(generate-flex-css my-container)
(generate-flex-css item1)
