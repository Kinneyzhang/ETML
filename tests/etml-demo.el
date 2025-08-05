;; -*- lexical-binding: t -*-

(require 'etml-block)

(defvar etml-demo-window-pixel nil)

(defmacro etml-pop-buffer (buffer-name &rest body)
  (declare (indent defun))
  `(let ((buffer (get-buffer-create ,buffer-name)))
     (delete-other-windows)
     (setq etml-demo-window-pixel (window-pixel-width))
     (with-current-buffer buffer
       (local-set-key "q" 'quit-window)
       (erase-buffer)
       ,@body)
     (switch-to-buffer buffer)))

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
              :width `(,etml-demo-window-pixel) :justify 'left
              :height (- (window-body-height) 8) :align 'top
              ;; :border '("#7D9EC0" . "#40E0D0")
              :bgcolor "#444"
              :padding `(:left 2 :right 2 :bottom 1)
              :margin '(:left 6 :right 6 :top 2)))

(defun etml-demo-body-pixel ()
  (etml-block-content-pixel (etml-demo-body "")))

(defun etml-demo-header ()
  (etml-block :content
              (etml-block-string
               :content etml-demo-header-str
               :width `(,(etml-demo-body-pixel)) :justify 'center
               :margin '(:top 1))
              :width `(,(etml-demo-body-pixel))
              :justify 'center))

(defun etml-demo-tabs ()
  (etml-block :content etml-demo-tabs-str
              :width `(,(floor (* 0.7 (etml-demo-body-pixel))))
              :border '(:bottom ("#7D9EC0" . "#40E0D0"))
              :margin '(:bottom 1 :top 1)))

(defun etml-demo-login ()
  (etml-block :content "注册 登陆"
              :width `(,(floor (* 0.3 (etml-demo-body-pixel))))
              :justify 'right
              :margin '(:bottom 1 :top 1)
              :padding '(:right 5)
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
              :padding '(:left 2 :right 2 :top 0 :bottom 0)
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
  (etml-pop-buffer "*etml-demo*"
    (insert
     (etml-block-render
      (etml-demo-body
       (etml-block-render
        (etml-block-stack
         (etml-demo-header)
         (etml-block-concat (etml-demo-tabs) (etml-demo-login))
         (etml-block-concat
          (etml-demo-sidebar)
          (etml-demo-main
           (etml-block-render
            (etml-block-stack
             (etml-demo-banner)
             (etml-block-concat
              (etml-demo-cardx etml-demo-card-str1)
              (etml-block-stack
               (etml-demo-cardy etml-demo-card-str2)
               (etml-demo-cardy etml-demo-card-str3)
               (etml-demo-rest
                (etml-block-render
                 (etml-block-concat
                  (etml-block :content etml-demo-card-str4
                              :width `(,(floor (* 0.4 (etml-demo-cardy-pixel))))
                              :justify 'center
                              :border "grey"
                              :padding '(:left 1 :right 1))
                  (etml-block :content "   ")
                  (etml-block :content etml-demo-card-str5
                              :width `(,(floor (* 0.4 (etml-demo-cardy-pixel))))
                              :justify 'center
                              :border t
                              :padding '(:left 1 :right 1))))))))))))))))
    (goto-char (point-min))))
