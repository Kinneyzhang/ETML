(require 'etml-block)
(require 'etml-test)

(defvar etml-block-basic-str1
  "作为神之编辑器（Editor of the Gods），Emacs 早已超越了普通文本编辑器的范畴。它是由​​Richard Stallman​​于1976年创建的​​GNU项目​核心组件，其名字源自 Editor MACroS​​。在过去的半个世纪里，Emacs演化成了一个​​self-documenting, customizable, extensible​​的生态系统，用户可通过​​Emacs Lisp (elisp)​​ 重新定义编辑行为。M-x 是每个Emacer的魔法咒语——按下Alt（或Meta键）加x即可召唤任意命令，比如M-x butterfly这样的复活节彩蛋。中国开发者常戏称其为“​​永远的操作系统​​”，因为你可以通过org-mode管理TODO list、用magit操作Git仓库、甚至用EMMS播放MP3音乐。在Unix哲学中，Emacs坚持“一个编辑器统治所有​​”（One Editor to Rule Them All）的理念，这与VS Code等现代编辑器形成鲜明对比。C-x C-f打开文件，C-x C-s保存文档，看似复杂的组合键一旦形成​​肌肉记忆​​，效率就会呈指数级飙升。著名Python库Black的开发者曾公开表示：​\"My .emacs is my second brain.\"​")

(etml-test-pop-buffer "*test-basic-block*"
  (insert
   (etml-block-render
    (etml-block
     :content etml-block-basic-str1
     :width 35
     :border '("#aaaaaa" . "lightGreen")
     :bgcolor '("#FFF9F0" . "#222222")
     :padding '(1 . 1)
     :margin '(1 . 1)))))

(provide 'etml-block-basic-tests)
