;;; etml-grid-example.el --- Example of ETML grid layout -*- lexical-binding: t; -*-

;;; Commentary:
;; This file provides examples of using the etml-grid layout functionality.

;;; Code:

(require 'etml)
(require 'etml-grid)

(defun etml-grid-demo ()
  "Create a demo of the grid layout functionality."
  (interactive)
  (switch-to-buffer "*ETML Grid Demo*")
  (erase-buffer)
  (let ((inhibit-read-only t))
    ;; Create a grid container
    (let* ((grid (etml-node :width 60
                            :text "Grid Container"))
           ;; Create some child blocks
           (header (etml-block :text "Header"))
           (sidebar (etml-block :text "Sidebar\nNavigation\n- Home\n- About\n- Contact"))
           (content (etml-block :text "Main Content Area\n\nThis is a demonstration of the ETML grid layout system.\nIt allows you to create complex layouts similar to CSS grid."))
           (footer (etml-block :text "Footer © 2025")))
      
      ;; Define grid template
      (etml-node-set-template grid 
                              :columns "1fr 3fr"
                              :rows "auto 1fr auto"
                              :areas '(("header" "header")
                                       ("sidebar" "content")
                                       ("footer" "footer")))
      
      ;; Add children to specific grid areas
      (etml-node-add-child grid header "header")
      (etml-node-add-child grid sidebar "sidebar")
      (etml-node-add-child grid content "content")
      (etml-node-add-child grid footer "footer")
      
      ;; Render the grid
      (etml-render grid)))
  
  (etml-mode))

(defun etml-grid-complex-demo ()
  "Create a more complex demo of the grid layout functionality."
  (interactive)
  (switch-to-buffer "*ETML Complex Grid Demo*")
  (erase-buffer)
  (let ((inhibit-read-only t))
    (let* ((grid (etml-node :width 75
                            :text "Complex Grid Layout"))
           (header (etml-block :text "ETML Grid Layout Demo"))
           (nav1 (etml-block :text "Home"))
           (nav2 (etml-block :text "Products"))
           (nav3 (etml-block :text "Services"))
           (nav4 (etml-block :text "About"))
           (sidebar (etml-block :text "Categories\n\n• Category 1\n• Category 2\n• Category 3\n\nRecent Posts\n\n• Post 1\n• Post 2"))
           (main (etml-block :text "Main Content Area\n\nThis is a more complex demonstration of grid layout capabilities.\n\nYou can create responsive-like designs with different column and row templates."))
           (widget1 (etml-block :text "Widget 1\nUseful links"))
           (widget2 (etml-block :text "Widget 2\nSubscribe form"))
           (footer (etml-block :text "Footer © 2025 - All rights reserved")))
      
      ;; Define grid template with 12-column layout
      (etml-node-set-template grid 
                              :columns "1fr 1fr 1fr 1fr 1fr 1fr 1fr 1fr 1fr 1fr 1fr 1fr"
                              :rows "auto auto 1fr auto auto"
                              :areas '(("head" "head" "head" "head" "head" "head" "head" "head" "head" "head" "head" "head")
                                       ("nav1" "nav2" "nav3" "nav4" "nav4" "nav4" "nav4" "nav4" "nav4" "nav4" "nav4" "nav4")
                                       ("side" "side" "side" "main" "main" "main" "main" "main" "main" "main" "main" "main")
                                       ("side" "side" "side" "wid1" "wid1" "wid1" "wid1" "wid2" "wid2" "wid2" "wid2" "wid2")
                                       ("foot" "foot" "foot" "foot" "foot" "foot" "foot" "foot" "foot" "foot" "foot" "foot")))
      
      ;; Add children to specific grid areas
      (etml-node-add-child grid header "head")
      (etml-node-add-child grid nav1 "nav1")
      (etml-node-add-child grid nav2 "nav2")
      (etml-node-add-child grid nav3 "nav3")
      (etml-node-add-child grid nav4 "nav4")
      (etml-node-add-child grid sidebar "side")
      (etml-node-add-child grid main "main")
      (etml-node-add-child grid widget1 "wid1")
      (etml-node-add-child grid widget2 "wid2")
      (etml-node-add-child grid footer "foot")
      
      ;; Render the grid
      (etml-render grid)))
  
  (etml-mode))

(provide 'etml-grid-example)
;;; etml-grid-example.el ends here
