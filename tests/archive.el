(defun etml-block-scroll-show (&optional force)
  (interactive)
  (let ((buffer (get-buffer-create "*etml-scroll-tests*")))
    (if-let ((win (get-buffer-window buffer)))
        (progn
          (select-window win)
          (when force
            (etml-block-scroll-render buffer)))
      (switch-to-buffer buffer)
      (etml-block-scroll-render buffer))))

(defun etml-set-cursor ()
  "Set cursor type correctly in current buffer."
  (interactive)
  (run-with-idle-timer
   0.001 nil (lambda () (setq-local cursor-type t))))

(defun etml-block-parse-total-pixel (block slot)
  (when-let ((width (eval `(oref ,block ,slot))))
    (cond
     ((consp width) (car width))
     ((numberp width)
      (let ((content (oref block content)))
        (+ (etml-block-side-pixel block)
           (etml-width-pixel width content))))
     (t (error "Invalid format of block width: %S" width)))))

(defun etml-block-side-pixel (block &optional side)
  "Return the side pixel width of BLOCK. Side pixel is the
total of margin, padding and border pixel.

Defautly count both the left and right side. If SIDE equals
to a symbol 'left, count the left side only; if SIDE equals
to a symbol 'right, count the right side only."
  (let* ((y-scroll-p (etml-block-y-scroll-p block))
         ;; FIXME: recursive dependency
         ;; etml-block-y-scroll-p => content-height
         ;; content-height => content-width
         ;; content-width = total width - side-width
         ;; this is side-width
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
                 (if (and y-scroll-p
                          (eq 'left etml-block-scroll-bar-direction))
                     (if etml-block-scroll-bar-border-full-p
                         (+ 1 pixel etml-block-scroll-bar-pixel)
                       (+ pixel etml-block-scroll-bar-pixel))
                   pixel)
               ;; no left border
               (if (and y-scroll-p
                        (eq 'left etml-block-scroll-bar-direction))
                   etml-block-scroll-bar-pixel
                 0))))
    (setq right-side-pixel
          (+ right-side-pixel
             ;; when type is non-nil,
             ;; add extra pixel of scroll bar
             (if-let* ((pixel (plist-get right-border :width))
                       ((> pixel 0)))
                 ;; has right border
                 (if (and y-scroll-p
                          (eq 'right etml-block-scroll-bar-direction))
                     (if etml-block-scroll-bar-border-full-p
                         (+ 1 pixel etml-block-scroll-bar-pixel)
                       (+ pixel etml-block-scroll-bar-pixel))
                   pixel)
               ;; no right border
               (if (and y-scroll-p
                        (eq 'right etml-block-scroll-bar-direction))
                   etml-block-scroll-bar-pixel
                 0))))
    (pcase side
      ((pred null) (+ left-side-pixel right-side-pixel))
      ('left left-side-pixel)
      ('right right-side-pixel)
      (_ (error "Invalid format of side %S" side)))))

(defun etml-block-concat (strings &optional align text-align)
  "TEXT-ALIGN should be one of left,center,right.
ALIGN should be one of top,center,bottom."
  (let ((height (-max (-map #'block-string-linum strings))))
    (apply 'block-string-concat
           (mapcar (lambda (string)
                     (etml-lines-justify
                      (etml-lines-align string height align)
                      (string-pixel-width string)
                      text-align))
                   strings))))

(defun etml-block-stack (strings &optional align text-align)
  "ALIGN used for all blocks, TEXT-ALIGN used for text in a block."
  (let ((max-width (-max (-map #'string-pixel-width strings))))
    (mapconcat (lambda (string)
                 (etml-lines-justify
                  (etml-lines-justify
                   string (string-pixel-width string) text-align)
                  max-width align))
               strings "\n")))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defvar block-bgcolor-change-hook nil)

(defun etml-block-bgcolor-change-function (orig-fun &rest args)
  "Advice functon when load a theme."
  (let ((before-background-mode
         (frame-parameter nil 'background-mode))
        after-background-mode)
    (apply orig-fun args)
    (setq after-background-mode
          (frame-parameter nil 'background-mode))
    (unless (eq before-background-mode after-background-mode)
      (run-hooks 'block-bgcolor-change-hook))))

(advice-add #'counsel-load-theme
            :around #'block-bgcolor-change-function)
(advice-add #'load-theme :around #'block-bgcolor-change-function)
(add-hook #'block-bgcolor-change-hook #'block-text-render1)

;; 示例调用 (假设字符宽度为10像素)
(pop-buffer-insert 20
  ;; 使用当前frame字体度量
  (progn
    (ekp-clear-caches)
    (ekp-pixel-init 6 2 2 4 2 2 0 1 0)
    (etml-pixel-wrap
     "Ni-ka Ford has always known that she wanted to be an artist. But she wasn’t sure how to channel that passion until her final year as a studio art major in college. She remembers one day in an art studio when she was looking out the window at a tree. “And I was like, ‘Wow, the branches really look like veins in the body,’” she says. This inspiration led her to notice “a lot of similarities between our bodies and nature” and drew her to the field of medical illustration. Today her work distills medical complexity into illustrations and graphics that appear in journal articles, teaching materials and popular publications." 577))
  ;; (knuth-plass-justify
  ;;  "在使用 Emacs 进行开发时，很多用户会依赖 Org-mode 来管理他们的任务和笔记。For instance, with Org-mode, you can easily organize your projects and even export your notes to various formats like HTML or PDF. 此外，Emacs 的可定制性是它的一个巨大优势。You can write your own Emacs Lisp functions to automate repetitive tasks or extend the editor's functionality. 比如，通过编写简单的函数，你可以实现自动格式化代码、批量重命名文件，或者集成Git等版本控制工具。Emacs 的强大之处在于它的灵活性和扩展性，让用户可以根据自己的需求定制工作环境。"
  ;;  400)
  )

(pop-buffer-insert 10
  (let ((lst (make-list 3 (concat "emacs"))))
    (setf (car lst)
          (let ((str (car lst)))
            (add-face-text-property
             0 (length str) 'bold t str)
            str))
    (string-join lst " ")))

(pop-buffer-insert 10
  (let ((lst (make-list 3 (concat "emacs"))))
    (setf (car lst)
          (let ((str (copy-sequence (car lst))))
            (add-face-text-property
             0 (length str) 'bold t str)
            str))
    (string-join lst " ")))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -*- lexical-binding: t -*-

(require 'etml-block)
(require 'etml-test)

(defvar etml-demo-window-pixel nil)

(defvar etml-demo-header-str
  (propertize "Emacs China MISC Demo" 'face '(:height 1.8)))

(defvar etml-demo-tabs-str
  (string-join '("Forum" "Book" "Elpa" "FAQ" "Donate" "Fork Us") "   "))

(defvar etml-demo-banner-str
  "各位 Emacs 道友，大家好。（如果邮件注册没有收到激活邮件，请添加QQ群: 820941363, 群主会帮忙手动激活帐号。另外，尽可能不要使用 outlook 邮箱，容易收不到邮件）在通过电邮、新闻组或者聊天室提出技术问题前，请检查你有没有做到：阅读手册，试着自己找答案。（C-h C-h， 有问题，问 Emacs）；在网上搜索；使用 Emacs China 站内搜索（本论坛的站内搜索做的相当不错，中英文都支持）。关于提问的艺术，完整版本请参考：https://mp.weixin.qq.com/s/q461so9lWk4FKJGZ-p7Vcg")

(defvar etml-demo-sidebar-str
  (string-join
   '("≡ 类别" "  Emacs-general" "  闲聊灌水" "  Org-mode"
     "  Spacemacs" "  Programming" "  Emacs-lisp"
     "  Meta" "  年度热帖"
     "\n≡ 标签" "  org" "  elisp" "  spacemacs"
     "  windows" "  doom"
     "\n≡ 更多" "  用户" "  关于" "  常见问题"
     "  群组" "  徽章")
   "\n"))

(defvar etml-demo-sidebar-pixel
  (floor (* 1.5 (string-pixel-width etml-demo-sidebar-str))))

(defvar etml-demo-card-str1
  (propertize "作为神之编辑器（Editor of the Gods），Emacs 早已超越了普通文本编辑器的范畴。它是由​​Richard Stallman​​于1976年创建的​​GNU项目​核心组件，其名字源自 Editor MACroS​​。在过去的半个世纪里，Emacs演化成了一个​​self-documenting, customizable, extensible​​的生态系统，用户可通过​​Emacs Lisp (elisp)​​ 重新定义编辑行为。M-x 是每个Emacer的魔法咒语——按下Alt（或Meta键）加x即可召唤任意命令，比如M-x butterfly这样的复活节彩蛋。中国开发者常戏称其为“​​永远的操作系统​​”，因为你可以通过org-mode管理TODO list、用magit操作Git仓库、甚至用EMMS播放MP3音乐。在Unix哲学中，Emacs坚持“一个编辑器统治所有​​”（One Editor to Rule Them All）的理念，这与VSCode等现代编辑器形成鲜明对比。" 'face '(:family "Trebuchet MS")))

(defvar etml-demo-card-str2
  (propertize
   "Developed initially by Richard Stallman in the 1970s as part of the GNU Project and continuously refined for over four decades, GNU Emascs transcends conventional text editor categorization. At its architectural core, it operates as a dynamically extensible Lisp virtual machine optimized for symbolic computation and text transformation, enabling unparalleled workflow customization across programming, technical writing, scientific research, and system administration."
   'face '(:family "Trebuchet MS")))

;; "Cascadia Next SC"
;; Verdana
;; Trebuchet MS

(defvar etml-demo-card-str3
  (propertize "Les paléocosmologistes contemporains affrontent un désarroi paradigmatique depuis la découverte fortuite des microfossiles astrobiologiques interstellaires dans les météorites carbonées d'Orgueil. Ces structures cryptocristallines microtubulaires, interprétées par certains comme des nanoorganismes exoplanétaires fossiles."
              'face '(:family "Comic Sans MS")))

(defun etml-demo-body (content)
  (etml-block :content content
              :width `(,(- etml-demo-window-pixel 10)) :justify 'left
              ;; :height (- (window-body-height) 2) :align 'top
              ;; :border t
              ;; #F2F2F2 #FFF9F0 #FFF7E8 #FBF2ED #F0F7F2
              :bgcolor '("#FFF9F0" . "#444")
              :padding `(:left 2 :right 2 :bottom 1)
              :margin '(:left 2 :right 2 :top 1)))

(defun etml-demo-body-pixel ()
  (etml-block-content-pixel (etml-demo-body "")))

(defun etml-demo-header ()
  (let ((block (etml-block
                :content etml-demo-header-str
                :width `(,(etml-demo-body-pixel)) :justify 'center
                :margin '(:top 1))))
    (etml-block :content (etml-block-render block)
                :width `(,(etml-block-total-pixel block))
                :justify 'center)))

(defun etml-demo-tabs ()
  (etml-block :content etml-demo-tabs-str
              :width `(,(floor (* 0.6 (etml-demo-body-pixel))))
              :border '(:bottom ("#7D9EC0" . "#40E0D0"))
              :margin '(:bottom 1 :top 1)))

(defun etml-demo-login ()
  (etml-block :content "注册 登陆"
              :width `(,(floor (* 0.3 (etml-demo-body-pixel))))
              :justify 'right
              :margin '(:bottom 1 :top 1)
              :padding '(:right 2)
              :border '(:bottom ("#7D9EC0" . "#40E0D0"))))

(defun etml-demo-sidebar ()
  (etml-block :content etml-demo-sidebar-str
              :width `(,etml-demo-sidebar-pixel)
              :padding '(:top 1 :bottom 1 :left 1)
              :border "#666"))

(defun etml-demo-main-pixel ()
  (- (etml-demo-body-pixel)
     (etml-block-total-pixel (etml-demo-sidebar))))

(defun etml-demo-main (content)
  (etml-block :content content
              :width `(,(etml-demo-main-pixel))
              :padding '(:left 2 :right 2)))

(defun etml-demo-main-content-pixel ()
  (etml-block-content-pixel (etml-demo-main "")))

(defun etml-demo-banner ()
  (etml-block :content etml-demo-banner-str
              :width `(,(etml-demo-main-content-pixel))
              :padding '(:left 2 :right 2 :top 1 :bottom 1)
              :margin '(:left 2 :right 2)
              :bgcolor '("#87CEEB" . "#1e3a8a")))

(defun etml-demo-cardx-pixel ()
  (floor (* 0.4 (etml-demo-main-content-pixel))))

(defun etml-demo-cardx (str)
  (etml-block :content str
              :width `(,(etml-demo-cardx-pixel))
              :padding '(:left 0 :right 2 :top 0 :bottom 0)
              :margin '(:left 2 :right 0 :top 1)
              :border '(:right t)))

(defun etml-demo-cardy-pixel ()
  (floor (* 0.6 (etml-demo-main-content-pixel))))

(defun etml-demo-cardy (str)
  (etml-block :content str
              :width `(,(etml-demo-cardy-pixel))
              :margin '(:top 1 :right 1)
              :padding '(:left 1 :right 1 :top 0 :bottom 0)))

(defun etml-demo-rest (str)
  (etml-block :content str
              :width `(,(etml-demo-cardy-pixel))
              :justify 'center
              :margin '(:top 1 :right 1)
              :padding '(:left 1 :right 1 :top 0 :bottom 0)))

(defvar etml-demo-card-str4
  "Emacs 最大的魅力（或门槛）在于​​极致的可定制性​​。通过 Emacs Lisp，你可以修改几乎​​任何​​行为和外观，打造出独一无二、完全符合你工作流需求的工具。别人的 Emacs 和你的可能截然不同。")

(defvar etml-demo-card-str5
  (propertize
   "Claude Code is an agentic coding tool that lives in your terminal, understands your codebase, and helps you code faster by executing routine tasks, explaining complex code, and handling git workflows -- all through natural language commands. Use it in your terminal, IDE, or tag @claude on Github."
   'face '(:family "Cascadia Next SC")))

(defun etml-demo-show ()
  (interactive)
  (etml-test-pop-buffer "*etml-demo*"
    (insert
     (etml-block-render
      (etml-demo-body
       (etml-block-render
        (etml-block-stack
         (etml-demo-header)
         (etml-block-concat (etml-demo-tabs) (etml-demo-login))
         ;; (etml-block-concat
         ;;  (etml-demo-sidebar)
         ;;  (etml-demo-main
         ;;   (etml-block-render
         ;;    (etml-block-stack
         ;;     (etml-demo-banner)
         ;;     (etml-block-concat
         ;;      (etml-demo-cardx etml-demo-card-str1)
         ;;      (etml-block-stack
         ;;       (etml-demo-cardy etml-demo-card-str2)
         ;;       (etml-demo-cardy etml-demo-card-str3)
         ;;       (etml-demo-rest
         ;;        (etml-block-render
         ;;         (etml-block-concat
         ;;          (etml-block :content etml-demo-card-str4
         ;;                      :width `(,(floor (* 0.4 (etml-demo-cardy-pixel))))
         ;;                      :justify 'center
         ;;                      :border "grey"
         ;;                      :padding '(:left 1 :right 1))
         ;;          (etml-block :content "   ")
         ;;          (etml-block :content etml-demo-card-str5
         ;;                      :width `(,(floor (* 0.5 (etml-demo-cardy-pixel))))
         ;;                      :justify 'center
         ;;                      :border "grey"
         ;;                      :padding '(:left 1 :right 1)))))))))))
         )))))
    (goto-char (point-min))))
