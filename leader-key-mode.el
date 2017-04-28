(defvar leader-key-mode-keymap
  (make-sparse-keymap))
(define-minor-mode leader-key-mode
  "Minor mode to support <leader> support." t nil leader-key-mode-keymap)
(defvar leader-key "\\"
  "the default leader key")
(defvar leader-key-mode-mark-active-keymap
  (make-sparse-keymap))
(defconst leader-key-mode--emulation-mode-map-alist
  `((mark-active ,@leader-key-mode-mark-active-keymap)
    (leader-key-mode ,@(let ((m (make-sparse-keymap)))
                         (define-key m (kbd leader-key) leader-key-mode-keymap)
                         m))))
(add-to-list 'emulation-mode-map-alists 'leader-key-mode--emulation-mode-map-alist)
(provide 'leader-key-mode)
