(require 'block-pixel)

(pop-buffer-insert 5
  (etml-pixel-pad "happy hacking emacs" 20 50) "\n"
  (etml-pixel-pad "happy hacking emacs" 0 50))

(pop-buffer-insert 10
  (etml-pixel-reach "happy hacking emacs" 400) "\n"
  (etml-pixel-reach "happy hacking emacs" 400 0 'left) "\n"
  (etml-pixel-reach "happy hacking emacs" 400 0 'right) "\n"
  (etml-pixel-reach "happy hacking emacs" 400 30 'left) "\n"
  (etml-pixel-reach "happy hacking emacs" 400 -30 'left) "\n"
  (etml-pixel-reach "happy hacking emacs" 400 30 'right) "\n"
  (etml-pixel-reach "happy hacking emacs" 400 -30 'right))

(pop-buffer-insert 10
  (etml-pixel-align "happy hacking emacs" 400) "\n"
  (etml-pixel-align "happy hacking emacs" 400 'left) "\n"
  (etml-pixel-align "happy hacking emacs" 400 'center) "\n"
  (etml-pixel-align "happy hacking emacs" 400 'right))

(pop-buffer-insert 10
  (etml-pixel-center "happy hacking emacs" 500) "\n"
  (etml-pixel-left "happy hacking emacs" 500) "\n"
  (etml-pixel-right "happy hacking emacs" 500))

(pop-buffer-insert 10
  (etml-pixel-keep-left "happy hacking emacs" 50) "\n"
  (etml-pixel-keep-right "happy hacking emacs" 50) "\n"
  (etml-pixel-chop-left "happy hacking emacs" 50) "\n"
  (etml-pixel-chop-right "happy hacking emacs" 50))

(pop-buffer-insert 10
  (etml-pixel-wrap
   "Ni-ka Ford has always known that she wanted to be an artist. But she wasn't sure how to channel that passion until her final year as a studio art major in college. She remembers one day in an art studio when she was looking out the window at a tree. “And I was like, 'Wow, the branches really look like veins in the body,'” she says. This inspiration led her to notice “a lot of similarities between our bodies and nature” and drew her to the field of medical illustration. Today her work distills medical complexity into illustrations and graphics that appear in journal articles, teaching materials and popular publications." 600))
