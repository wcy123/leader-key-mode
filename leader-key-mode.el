(define-minor-mode leader-key-mode
  "Minor mode to support <leader> support." t)
(defvar leader-key "\\"
  "the default leader key")
(defvar leader-key-mode-mark-active-keymap
  (make-sparse-keymap))
(defvar leader-key-mode-keymap (make-sparse-keymap))
(defun leader-key-mode-create-entry-keymap (key)
  (let ((m (make-sparse-keymap)))
    (define-key m (kbd key) leader-key-mode-keymap)
    m))
(defconst leader-key-mode--emulation-mode-map-alist
  `((mark-active ,@leader-key-mode-mark-active-keymap)
    (leader-key-mode keymap (,,@(leader-key-mode-create-entry-keymap leader-key))))
(add-to-list 'emulation-mode-map-alists 'leader-key-mode--emulation-mode-map-alist)
(provide 'leader-key-mode)
