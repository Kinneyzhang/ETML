(items-flex
 (let ((flexs (oref flex items-flex)))
   (cond
    ((symbolp flexs)
     (pcase flexs
       ('none '(0 0 auto))
       ('auto '(1 1 auto))
       (_ (error "Invalid format of items-flex!"))))
    ((vectorp flexs) flexs)
    ((consp flexs) (make-vector num flexs)))))

(blocks-widths
 ;; if flex-basis is number or cons-cell
 ;; use it to override block total width
 (seq-map-indexed
  (lambda (item-flex idx)
    (let ((basis (nth 2 item-flex))
          (block (aref blocks idx)))
      (cond
       ((eq 'auto basis)
        ;; 保持原始宽度
        (etml-block-total-pixel block))
       ((integerp basis)
        ;; 指定个数的字符宽度
        (oset block width basis)
        (etml-block-total-pixel block))
       ((consp basis)
        ;; 指定像素宽度
        (oset block width (list basis))
        basis))))
  items-flex))
