;; -*- lexical-binding: t -*-

(require 'block)

(defvar block-demo-window-pixel nil)

(defmacro block-pop-buffer (buffer-name &rest body)
  (declare (indent defun))
  `(let ((buffer (get-buffer-create ,buffer-name)))
     (delete-other-windows)
     (setq block-demo-window-pixel (window-pixel-width))
     (with-current-buffer buffer
       (local-set-key "q" 'quit-window)
       (erase-buffer)
       ,@body)
     (switch-to-buffer buffer)))

(defvar block-demo-header-str
  (propertize "Emacs China MISC Demo" 'face '(:height 1.8)))

(defvar block-demo-tabs-str
  (string-join '("Forum" "Book" "Elpa" "FAQ" "Donate" "Fork Us") "   "))

(defvar block-demo-banner-str
  "各位 Emacs 道友，大家好。（如果邮件注册没有收到激活邮件，请添加QQ群: 820941363, 群主会帮忙手动激活帐号。另外，尽可能不要使用 outlook 邮箱，容易收不到邮件）在通过电邮、新闻组或者聊天室提出技术问题前，请检查你有没有做到：阅读手册，试着自己找答案。（C-h C-h， 有问题，问 Emacs）；在网上搜索；使用 Emacs China 站内搜索（本论坛的站内搜索做的相当不错，中英文都支持）。关于提问的艺术，完整版本请参考：https://mp.weixin.qq.com/s/q461so9lWk4FKJGZ-p7Vcg")

(defvar block-demo-sidebar-str
  (string-join
   '("≡ 类别" "  Emacs-general" "  闲聊灌水" "  Org-mode"
     "  Spacemacs" "  Programming" "  Emacs-lisp"
     "  Meta" "  年度热帖"
     "\n≡ 标签" "  org" "  elisp" "  spacemacs"
     "  windows" "  doom"
     "\n≡ 更多" "  用户" "  关于" "  常见问题"
     "  群组" "  徽章")
   "\n"))

(defvar block-demo-sidebar-pixel
  (floor (* 1.5 (string-pixel-width block-demo-sidebar-str))))

(defvar block-demo-card-str1
  "作为神之编辑器（Editor of the Gods），Emacs 早已超越了普通文本编辑器的范畴。它是由​​Richard Stallman​​于1976年创建的​​GNU项目​核心组件，其名字源自 Editor MACroS​​。在过去的半个世纪里，Emacs演化成了一个​​self-documenting, customizable, extensible​​的生态系统，用户可通过​​Emacs Lisp (elisp)​​ 重新定义编辑行为。M-x 是每个Emacer的魔法咒语——按下Alt（或Meta键）加x即可召唤任意命令，比如M-x butterfly这样的复活节彩蛋。中国开发者常戏称其为“​​永远的操作系统​​”，因为你可以通过org-mode管理TODO list、用magit操作Git仓库、甚至用EMMS播放MP3音乐。在Unix哲学中，Emacs坚持“一个编辑器统治所有​​”（One Editor to Rule Them All）的理念，这与VS Code等现代编辑器形成鲜明对比。")

(defvar block-demo-card-str2
  (propertize
   "Developed initially by Richard Stallman in the 1970s as part of the GNU Project and continuously refined for over four decades, GNU Emascs transcends conventional text editor categorization. At its architectural core, it operates as a dynamically extensible Lisp virtual machine optimized for symbolic computation and text transformation, enabling unparalleled workflow customization across programming, technical writing, scientific research, and system administration."
   'face '(:family "Cascadia Next SC")))

(defvar block-demo-card-str3
  (propertize "Les paléocosmologistes contemporains affrontent un désarroi paradigmatique depuis la découverte fortuite des microfossiles astrobiologiques interstellaires dans les météorites carbonées d'Orgueil. Ces structures cryptocristallines microtubulaires, interprétées par certains comme des nanoorganismes exoplanétaires fossiles."
              'face '(:family "Cascadia Next SC")))

(defun block-demo-body (content)
  (block :content content
         :width `(,block-demo-window-pixel) :justify 'left
         :height (- (window-body-height) 8) :align 'top
         :border '("#7D9EC0" . "#40E0D0")
         :padding `(:left 2 :right 2 :bottom 1)
         :margin '(:left 1 :top 1)))

(defun block-demo-body-pixel ()
  (block-content-pixel (block-demo-body nil)))

(defun block-demo-header ()
  (block :content
         (block-string
          :content block-demo-header-str
          :width `(,(block-demo-body-pixel)) :justify 'center
          :margin '(:top 1))
         :width `(,(block-demo-body-pixel))
         :justify 'center))

(defun block-demo-tabs ()
  (block :content block-demo-tabs-str
         :width `(,(floor (* 0.7 (block-demo-body-pixel))))
         :border '(:bottom ("#7D9EC0" . "#40E0D0"))
         :margin '(:bottom 1)))

(defun block-demo-login ()
  (block :content "注册 登陆"
         :width `(,(floor (* 0.3 (block-demo-body-pixel))))
         :justify 'right
         :padding '(:right 5)
         :border '(:bottom ("#7D9EC0" . "#40E0D0"))))

(defun block-demo-sidebar ()
  (block :content block-demo-sidebar-str
         :width `(,block-demo-sidebar-pixel)
         :padding '(:top 1 :bottom 1 :left 1)
         :border "#666"))

(defun block-demo-main-pixel ()
  (- (block-demo-body-pixel)
     (block-total-pixel (block-demo-sidebar))))

(defun block-demo-main (content)
  (block :content content
         :width `(,(block-demo-main-pixel))
         :padding '(:left 2 :right 2)))

(defun block-demo-main-content-pixel ()
  (block-content-pixel (block-demo-main nil)))

(defun block-demo-banner ()
  (block :content block-demo-banner-str
         :width `(,(block-demo-main-content-pixel))
         :padding '(:left 2 :right 2 :top 1 :bottom 1)
         :margin '(:left 2 :right 2)
         :bgcolor '("#87CEEB" . "#1e3a8a")))

(defun block-demo-cardx-pixel ()
  (floor (* 0.4 (block-demo-main-content-pixel))))

(defun block-demo-cardx (str)
  (block :content str
         :width `(,(block-demo-cardx-pixel))
         :padding '(:left 2 :right 2 :top 0 :bottom 0)
         :margin '(:left 2 :right 0 :top 1)
         :border '(:right t)))

(defun block-demo-cardy-pixel ()
  (floor (* 0.6 (block-demo-main-content-pixel))))

(defun block-demo-cardy (str)
  (block :content str
         :width `(,(block-demo-cardy-pixel))
         :margin '(:top 1 :right 1)
         :padding '(:left 1 :right 1 :top 0 :bottom 0)))

(defun block-demo-rest (str)
  (block :content str
         :width `(,(block-demo-cardy-pixel))
         :justify 'center
         :margin '(:top 1 :right 1)
         :padding '(:left 1 :right 1 :top 0 :bottom 0)))

(defvar block-demo-card-str4
  "Emacs 最大的魅力（或门槛）在于​​极致的可定制性​​。通过 Emacs Lisp，你可以修改几乎​​任何​​行为和外观，打造出独一无二、完全符合你工作流需求的工具。别人的 Emacs 和你的可能截然不同。")

(defvar block-demo-card-str5
  (propertize
   "Claude Code is an agentic coding tool that lives in your terminal, understands your codebase, and helps you code faster by executing routine tasks, explaining complex code, and handling git workflows -- all through natural language commands. Use it in your terminal, IDE, or tag @claude on Github."
   'face '(:family "Times New Roman")))

(defun block-demo-show ()
  (interactive)
  (block-pop-buffer "*block-demo*"
    (insert
     (block-render
      (block-demo-body
       (block-render (block-stack
                      (block-demo-header)
                      (block-concat (block-demo-tabs) (block-demo-login))
                      (block-concat (block-demo-sidebar)
                                    (block-demo-main
                                     (block-render
                                      (block-stack (block-demo-banner)
                                                   (block-concat
                                                    (block-demo-cardx block-demo-card-str1)
                                                    (block :content "")
                                                    (block-stack
                                                     (block-demo-cardy block-demo-card-str2)
                                                     (block-demo-cardy block-demo-card-str3)
                                                     (block-demo-rest
                                                      (block-render
                                                       (block-concat
                                                        (block :content block-demo-card-str4
                                                               :width `(,(floor (* 0.4 (block-demo-cardy-pixel))))
                                                               :justify 'center
                                                               :border "grey"
                                                               :padding '(:left 1 :right 1))
                                                        (block :content "   ")
                                                        (block :content block-demo-card-str5
                                                               :width `(,(floor (* 0.4 (block-demo-cardy-pixel))))
                                                               :justify 'center
                                                               :border t
                                                               :padding '(:left 1 :right 1))))))))))))))))
    (goto-char (point-min))))
