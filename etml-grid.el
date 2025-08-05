;;; etml-grid.el --- Grid layout for ETML -*- lexical-binding: t; -*-

;; Author: Kinneyzhang
;; Keywords: grid, layout
;; Package-Requires: ((emacs "27.1") (etml "0.1"))

;; This file is not part of GNU Emacs.

;;; Commentary:

;; This file implements CSS grid layout support for ETML (Emacs Text Markup Language).
;; It defines an etml-node class that inherits from etml-block and provides
;; grid layout capabilities similar to CSS Grid.

;;; Code:

(require 'etml)
(require 'cl-lib)

(defclass etml-node (etml-block)
  ((grid-template-columns
    :initarg :grid-template-columns
    :initform nil
    :type (or null string)
    :documentation "CSS grid-template-columns property.")
   
   (grid-template-rows
    :initarg :grid-template-rows
    :initform nil
    :type (or null string)
    :documentation "CSS grid-template-rows property.")
   
   (grid-template-areas
    :initarg :grid-template-areas
    :initform nil
    :type (or null list)
    :documentation "CSS grid-template-areas property.")
   
   (grid-column-gap
    :initarg :grid-column-gap
    :initform 1
    :type number
    :documentation "Gap between columns.")
   
   (grid-row-gap
    :initarg :grid-row-gap
    :initform 0
    :type number
    :documentation "Gap between rows.")
   
   (grid-auto-flow
    :initarg :grid-auto-flow
    :initform "row"
    :type string
    :documentation "Direction for auto-placement algorithm.")
   
   (grid-children
    :initarg :grid-children
    :initform nil
    :type list
    :documentation "Child blocks within this grid.")
   
   (grid-layout
    :initform nil
    :type (or null list)
    :documentation "Internal storage for calculated grid layout."))
  
  "A class for creating grid-based layouts of blocks in ETML."
  :documentation "Grid layout container for ETML blocks.")

(cl-defmethod etml-node-parse-template-value ((node etml-node) template-string)
  "Parse TEMPLATE-STRING into a list of grid track sizes.
Example: '1fr 2fr auto minmax(100px, 1fr)' -> ((fr 1) (fr 2) auto (minmax 100 (fr 1)))"
  (when template-string
    (let ((parts (split-string template-string " " t))
          result)
      (dolist (part parts)
        (cond
         ;; fr units
         ((string-match "\\([0-9.]+\\)fr" part)
          (push (list 'fr (string-to-number (match-string 1 part))) result))
         ;; auto
         ((string= "auto" part)
          (push 'auto result))
         ;; pixel values
         ((string-match "\\([0-9.]+\\)px" part)
          (push (list 'px (string-to-number (match-string 1 part))) result))
         ;; minmax function
         ((string-match "minmax(\\(.*?\\),\\(.*?\\))" part)
          (let ((min (match-string 1 part))
                (max (match-string 2 part)))
            (push (list 'minmax
                        (if (string-match "\\([0-9.]+\\)px" min)
                            (list 'px (string-to-number (match-string 1 min)))
                          (if (string-match "\\([0-9.]+\\)fr" min)
                              (list 'fr (string-to-number (match-string 1 min)))
                            min))
                        (if (string-match "\\([0-9.]+\\)px" max)
                            (list 'px (string-to-number (match-string 1 max)))
                          (if (string-match "\\([0-9.]+\\)fr" max)
                              (list 'fr (string-to-number (match-string 1 max)))
                            max)))
                  result)))))
      (nreverse result))))

(cl-defmethod etml-node-calculate-grid-dimensions ((node etml-node))
  "Calculate the grid dimensions based on templates."
  (let* ((cols (or (slot-value node 'grid-template-columns) "1fr"))
         (rows (or (slot-value node 'grid-template-rows) "auto"))
         (col-sizes (etml-node-parse-template-value node cols))
         (row-sizes (etml-node-parse-template-value node rows))
         (num-cols (length col-sizes))
         (num-rows (if (slot-value node 'grid-template-areas)
                      (length (slot-value node 'grid-template-areas))
                    (ceiling (/ (length (slot-value node 'grid-children)) num-cols)))))
    (cons num-cols (max num-rows (length row-sizes)))))

(cl-defmethod etml-node-place-children ((node etml-node))
  "Place children into grid cells based on grid layout properties."
  (let* ((dims (etml-node-calculate-grid-dimensions node))
         (cols (car dims))
         (rows (cdr dims))
         (areas (slot-value node 'grid-template-areas))
         (children (slot-value node 'grid-children))
         (grid (make-vector (* cols rows) nil))
         (flow (slot-value node 'grid-auto-flow)))
    
    ;; First, place children with explicit grid-area
    (dolist (child children)
      (when (and (slot-exists-p child 'grid-area) 
                 (slot-boundp child 'grid-area)
                 (slot-value child 'grid-area))
        (let* ((area (slot-value child 'grid-area))
               (area-pos (etml-node-find-area-position node area areas cols rows)))
          (when area-pos
            (let ((row (car area-pos))
                  (col (cdr area-pos))
                  (span-rows (car (cdr (cdr area-pos))))
                  (span-cols (cdr (cdr (cdr area-pos)))))
              (dotimes (r span-rows)
                (dotimes (c span-cols)
                  (aset grid (+ (* (+ row r) cols) (+ col c)) child))))))))
    
    ;; Then auto-place remaining children
    (let ((row 0)
          (col 0))
      (dolist (child children)
        (unless (and (slot-exists-p child 'grid-area) 
                     (slot-boundp child 'grid-area)
                     (slot-value child 'grid-area))
          (while (and (< row rows) (< col cols)
                      (aref grid (+ (* row cols) col)))
            (if (string= flow "row")
                (setq col (1+ col)
                      row (if (>= col cols) (1+ row) row)
                      col (if (>= col cols) 0 col))
              (setq row (1+ row)
                    col (if (>= row rows) (1+ col) col)
                    row (if (>= row rows) 0 row))))
          
          (when (and (< row rows) (< col cols))
            (aset grid (+ (* row cols) col) child)
            (if (string= flow "row")
                (setq col (1+ col)
                      row (if (>= col cols) (1+ row) row)
                      col (if (>= col cols) 0 col))
              (setq row (1+ row)
                    col (if (>= row rows) (1+ col) col)
                    row (if (>= row rows) 0 row)))))))
    
    ;; Store layout for rendering
    (setf (slot-value node 'grid-layout) grid)))

(cl-defmethod etml-node-find-area-position ((node etml-node) area areas cols rows)
  "Find position of AREA in AREAS template for a grid of COLS x ROWS."
  (when (and area areas)
    (let ((start-row nil)
          (start-col nil)
          (end-row nil)
          (end-col nil))
      (dotimes (r (length areas))
        (let ((row-areas (nth r areas)))
          (dotimes (c (length row-areas))
            (let ((current-area (nth c row-areas)))
              (when (equal current-area area)
                (setq start-row (or start-row r)
                      start-col (or start-col c)
                      end-row r
                      end-col (max (or end-col c) c)))))))
      (when (and start-row start-col end-row end-col)
        (cons start-row (cons start-col (cons (1+ (- end-row start-row)) 
                                              (1+ (- end-col start-col)))))))))

(cl-defmethod etml-node-get-cell-content ((node etml-node))
  "Generate content for grid cells, including text properties."
  (etml-node-place-children node)
  (let* ((dims (etml-node-calculate-grid-dimensions node))
         (cols (car dims))
         (rows (cdr dims))
         (grid (slot-value node 'grid-layout))
         (cell-contents (make-vector (* cols rows) nil)))
    
    ;; First pass: Convert each unique child block to content string
    (let ((processed-children nil))
      (dotimes (r rows)
        (dotimes (c cols)
          (let* ((index (+ (* r cols) c))
                 (child (and (< index (length grid)) (aref grid index))))
            (when (and child (not (memq child processed-children)))
              (push child processed-children)
              
              ;; Calculate span of this cell
              (let ((span-rows 1)
                    (span-cols 1))
                (while (and (< (+ r span-rows) rows)
                            (< (+ (* (+ r span-rows) cols) c) (length grid))
                            (eq child (aref grid (+ (* (+ r span-rows) cols) c))))
                  (setq span-rows (1+ span-rows)))
                (while (and (< (+ c span-cols) cols)
                            (< (+ (* r cols) (+ c span-cols)) (length grid))
                            (eq child (aref grid (+ (* r cols) (+ c span-cols)))))
                  (setq span-cols (1+ span-cols)))
                
                ;; Get child's text content
                (let ((content (etml-get-text child)))
                  ;; Store in first cell of span area
                  (aset cell-contents (+ (* r cols) c) 
                        (cons (cons span-rows span-cols) content)))))))))
    
    cell-contents))

(cl-defmethod etml-node-generate-grid-string ((node etml-node))
  "Generate string representation of grid layout."
  (let* ((cell-contents (etml-node-get-cell-content node))
         (dims (etml-node-calculate-grid-dimensions node))
         (cols (car dims))
         (rows (cdr dims))
         (col-gap (slot-value node 'grid-column-gap))
         (row-gap (slot-value node 'grid-row-gap))
         (width (or (and (slot-exists-p node 'width) 
                        (slot-boundp node 'width)
                        (slot-value node 'width))
                   80))
         (col-widths (make-vector cols 0))
         (row-heights (make-vector rows 0))
         result)

    ;; First calculate column widths based on content
    (let ((total-fr 0)
          (reserved-width 0))
      
      ;; Distribute available width to columns based on content and fr units
      (dotimes (c cols)
        (aset col-widths c (max 5 (/ width cols))))  ; Simple distribution
      
      ;; Calculate row heights based on content
      (dotimes (r rows)
        (aset row-heights r 1))  ; Default minimum height
      
      ;; Adjust based on cell content
      (dotimes (r rows)
        (dotimes (c cols)
          (let* ((index (+ (* r cols) c))
                 (cell-data (and (< index (length cell-contents)) 
                                (aref cell-contents index))))
            (when cell-data
              (let* ((spans (car cell-data))
                     (span-rows (car spans))
                     (span-cols (cdr spans))
                     (content (cdr cell-data))
                     (content-lines (split-string content "\n"))
                     (content-height (length content-lines))
                     (content-width (apply #'max (mapcar #'length content-lines))))
                
                ;; Adjust row height if needed
                (aset row-heights r (max (aref row-heights r) 
                                        (ceiling (/ content-height span-rows))))
                
                ;; Adjust column width if needed
                (aset col-widths c (max (aref col-widths c)
                                       (ceiling (/ content-width span-cols))))))))))
    
    ;; Generate grid string with appropriate spacing
    (let ((buffer (generate-new-buffer " *etml-grid-temp*")))
      (with-current-buffer buffer
        ;; Draw rows
        (dotimes (r rows)
          (let ((row-height (aref row-heights r)))
            ;; For each row of text in this grid row
            (dotimes (line-num row-height)
              ;; Draw each cell in this row
              (dotimes (c cols)
                (let* ((index (+ (* r cols) c))
                       (cell-data (and (< index (length cell-contents))
                                      (aref cell-contents index)))
                       (col-width (aref col-widths c))
                       cell-text)
                  
                  ;; Only process primary cell (not part of a span)
                  (when (and cell-data
                             (or (= c 0) 
                                 (not (eq (aref cell-contents (1- index))
                                         (aref cell-contents index)))))
                    (let* ((spans (car cell-data))
                           (content (cdr cell-data))
                           (content-lines (split-string content "\n"))
                           (effective-width 
                            (+ col-width 
                               (* (1- (cdr spans)) (+ col-width col-gap)))))
                      
                      ;; Get the line of text for this row
                      (setq cell-text
                            (if (< line-num (length content-lines))
                                (let ((text (nth line-num content-lines)))
                                  (if (> (length text) effective-width)
                                      (concat (substring text 0 (- effective-width 3)) "...")
                                    text))
                              ""))))
                  
                  ;; Insert cell content or padding
                  (if (and cell-data
                           (or (= c 0)
                               (not (eq (aref cell-contents (1- index))
                                       (aref cell-contents index)))))
                      (let ((total-width (+ col-width 
                                           (* (1- (cdr (car cell-data))) 
                                              (+ col-width col-gap)))))
                        (insert (or cell-text ""))
                        (when (< (length (or cell-text "")) total-width)
                          (insert (make-string (- total-width (length (or cell-text ""))) 32))))
                    
                    ;; Skip if this cell is part of a span
                    (unless (and (> c 0)
                                (eq (aref cell-contents (1- index))
                                    (aref cell-contents index)))
                      (insert (make-string col-width 32))))
                  
                  ;; Add column gap
                  (when (< c (1- cols))
                    (insert (make-string col-gap 32)))))
              
              ;; End of row
              (insert "\n")))
          
          ;; Add row gap
          (when (and row-gap (< r (1- rows)))
            (dotimes (_ row-gap)
              (insert (make-string width 32))
              (insert "\n"))))
        
        ;; Get the resulting string
        (setq result (buffer-string)))
      
      ;; Clean up temporary buffer
      (kill-buffer buffer))
    
    result))

(cl-defmethod etml-get-text ((node etml-block))
  "Get the text content of the block.
This helper ensures compatibility with different ETML versions."
  (if (slot-exists-p node 'text)
      (if (slot-boundp node 'text)
          (slot-value node 'text)
        "")
    (if (slot-exists-p node 'content)
        (if (slot-boundp node 'content)
            (slot-value node 'content)
          "")
      "")))

(cl-defmethod etml-get-text :around ((node etml-node))
  "Override the text getter for grid nodes."
  (etml-node-generate-grid-string node))

(cl-defmethod etml-render ((node etml-node))
  "Render the grid layout with all its children as a string."
  (let ((grid-string (etml-node-generate-grid-string node)))
    (insert grid-string))
  nil)

(cl-defmethod etml-node-add-child ((node etml-node) child &optional grid-area)
  "Add CHILD to NODE at optional GRID-AREA."
  (when grid-area
    (unless (slot-exists-p child 'grid-area)
      (object-add-to-list child 'grid-area nil t))
    (setf (slot-value child 'grid-area) grid-area))
  (object-add-to-list node 'grid-children child t))

(cl-defmethod etml-node-set-template ((node etml-node) &key columns rows areas)
  "Set grid template properties on NODE.
COLUMNS and ROWS are strings defining grid-template-columns and grid-template-rows.
AREAS is a list of lists defining named template areas."
  (when columns (setf (slot-value node 'grid-template-columns) columns))
  (when rows (setf (slot-value node 'grid-template-rows) rows))
  (when areas (setf (slot-value node 'grid-template-areas) areas)))

(provide 'etml-grid)
;;; etml-grid.el ends here
