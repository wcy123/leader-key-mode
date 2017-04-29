;;; leader-key-mode.el --- A minor mode to mimic leader key in VIM

;;; Commentary:
;;

;;; Code:

(define-minor-mode leader-key-mode
  "Minor mode to support <leader> support." t)
(defvar leader-key "\\"
  "The default leader key.")
(defvar leader-key-mode-mark-active-keymap
  "the keymap for active mark mode. when mark is active, this
  keymap is activated and when mark is inactive, the keymap is
  deactivated.
"
  (let ((m (make-sparse-keymap)))
    (define-key m (kbd "a") (kbd "C-a"))
    m))
(defvar leader-key-mode-keymap
  (let ((m (make-sparse-keymap)))

    m))
(defun leader-key-mode-create-entry-keymap (key)
  "It is a helper function.
it is used to create a keymap which bound to the leader key.

KEY is default to \"\\\" which is the leader key."

  (let ((m (make-sparse-keymap)))
    (define-key m (kbd key) leader-key-mode-keymap)
    m))
(defconst leader-key-mode--emulation-mode-map-alist
  "an alist which will be added into `emulation-mode-map-alists`."
  `((mark-active ,@leader-key-mode-mark-active-keymap)
    (leader-key-mode ,@(leader-key-mode-create-entry-keymap leader-key))))
(add-to-list 'emulation-mode-map-alists
             'leader-key-mode--emulation-mode-map-alist)


(defun leader-key-mode--delete-region (b e)
  "Delete the current region.
B is the beginning of the region.  E is the end of the region."
  (interactive "r")
  (delete-region b e))
(defun leader-key-mode--duplicate-line()
  "duplicate a line, and keep cursor's posistion in the line."
  (interactive)
  (let* ((b (line-beginning-position))
         (e (line-end-position))
         (c (point))
         (l (- c b))
         (txt (buffer-substring b e)))
    (forward-line)
    (save-excursion
      (insert txt "\n"))
    (forward-char l)))
(defun leader-key-mode--delete-and-yank (b e)
  "Delete the current region, and then yank(paste).
This is common convention for many editors.  B is the beginnin of
  the region and E is the end of the region."
  (interactive "r")
  (delete-region b e)
  (call-interactively 'yank))
(defun leader-key-mode--display-buffer-name ()
  "Display the full path of the current buffer-file."
  (interactive)
  (message (or (buffer-file-name (current-buffer))
               (format "%s[%s]" default-directory (buffer-name)))))

(defmacro leader-key-mode--replay(str)
  "this macro is used to bind one key sequence to another key sequence."
  `#'(lambda () (interactive)
       (call-interactively (key-binding (kbd ,str)))))

(define-key leader-key-mode-mark-active-keymap (kbd "c") 'kill-ring-save)
(define-key leader-key-mode-mark-active-keymap (kbd "d")  'leader-key-mode--delete-region)
(define-key leader-key-mode-mark-active-keymap (kbd "x")  'kill-ring-save)
(define-key leader-key-mode-mark-active-keymap (kbd "y")  'leader-key-mode--delete-and-yank)

(define-key leader-key-mode-keymap (kbd "a") 'beginning-of-line)

(define-key leader-key-mode-keymap (kbd "b") (leader-key-mode--replay "C-x b"))
(define-key leader-key-mode-keymap (kbd "d") nil)
(define-key leader-key-mode-keymap (kbd "d l") 'leader-key-mode--duplicate-line)
(define-key leader-key-mode-keymap (kbd "y") 'yank)
;;(define-key leader-key-mode-keymap (kbd "u") 'undo)
(define-key leader-key-mode-keymap (kbd "SPC") 'set-mark-command)
;;(define-key leader-key-mode-keymap (kbd ":") pp-eval-expression)
(define-key leader-key-mode-keymap (kbd "/") 'dabbrev-expand)
(define-key leader-key-mode-keymap (kbd "?") 'hippie-expand)
(define-key leader-key-mode-keymap (kbd "5") 'leader-key-mode--display-buffer-name)
(define-key leader-key-mode-keymap (kbd "7") 'compile)
;;(define-key leader-key-mode-keymap (kbd "`") next-error)
;;(define-key leader-key-mode-keymap (kbd "!") shell-command)
;;(define-key leader-key-mode-keymap (kbd "RET") execute-extended-command)
(define-key leader-key-mode-keymap (kbd "1") 'delete-other-windows)
(define-key leader-key-mode-keymap (kbd "2") 'mark-sexp)
(define-key leader-key-mode-keymap (kbd "\\") #'(lambda ()
                                                  (interactive)
                                                  (switch-to-buffer
                                                   (other-buffer))))

(define-key leader-key-mode-keymap (kbd "f") nil)
(define-key leader-key-mode-keymap (kbd "f s") 'save-buffer)
(define-key leader-key-mode-keymap (kbd "f o") 'find-file-at-point)
(define-key leader-key-mode-keymap (kbd "f S") 'save-some-buffers)
(define-key leader-key-mode-keymap (kbd "(") 'insert-parentheses)
(define-key leader-key-mode-keymap (kbd "\"") #'(lambda (arg) (interactive "P") (insert-pair arg 34 34)))
(define-key leader-key-mode-keymap (kbd "[") #'(lambda (arg) (interactive "P") (insert-pair arg 91 93)))
(define-key leader-key-mode-keymap (kbd "{") #'(lambda (arg) (interactive "P") (insert-pair arg 123 125)))


(provide 'leader-key-mode)

(provide 'leader-key-mode)

;;; leader-key-mode.el ends here
