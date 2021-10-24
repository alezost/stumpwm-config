(defun al/ml-windows ()
  ;; Original version of `al/ml-windows'
  (when-let ((cur-win (current-window)))
    (let* ((win-class (window-class cur-win))
           (win-list (remove-if-not
                      (lambda (win)
                        (equal win-class (window-class win)))
                      (sort-windows (current-group))))
           (num (length win-list)))
      (al/ml-separate
       (format nil "^[^B^4*~A^]~A ~{~A~^ ~}"
               win-class
               (if (> num 1)
                   (format nil " (^[^7*~A^])" num)
                   "")
               (mapcar (lambda (w)
                         (let* ((title (window-title w))
                                (title (subseq title 0 (min 20 (length title)))))
                           (if (eq w cur-win)
                               (concat "^[^04" title "^]")
                               (concat "^[^R" title "^]"))))
                       win-list))))))
