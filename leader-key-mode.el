;;; leader-key-mode.el --- a minor mode to mimic leader key in VIM  -*- lexical-binding: t; -*-

;; Copyright (C) 2017  wangchunye

;; Author: wangchunye <wangchunye@dg17>
;; Keywords: extensions, emulations
;; Version: 0.0
;; Package-Version: 0.0
;;
;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; usage
;; (require 'leader-key-mode)
;;
;; to add your own bindings
;;
;; (define-key leader-key-mode-keymap (kbd "O") 'your-faverite-command)
;;
;;; Code:



(define-minor-mode leader-key-mode
  "Minor mode to support <leader> support." t)
(defvar leader-key "\\"
  "the default leader key")
(defvar leader-key-mode-mark-active-keymap (make-sparse-keymap))
(defvar leader-key-mode-keymap (make-sparse-keymap))
(defun leader-key-mode-create-entry-keymap (key)
  "It is a helper function.
it is used to create a keymap which bound to the leader key.

KEY is default to \"\\\" which is the leader key."

  (let ((m (make-sparse-keymap)))
    (define-key m (kbd key) leader-key-mode-keymap)
    m))
(defconst leader-key-mode--emulation-mode-map-alist
  `((mark-active ,@leader-key-mode-mark-active-keymap)
    (leader-key-mode ,@(leader-key-mode-create-entry-keymap leader-key)))
  "an alist which will be added into `emulation-mode-map-alists`.")
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
       (let* ((leader-key-mode)
              (seq (kbd ,str))
              (cmd (key-binding seq)))
         (cond
          ((eq cmd 'self-insert-command) (insert seq))
          (t (call-interactively cmd))))))

(define-key leader-key-mode-mark-active-keymap (kbd "c") 'kill-ring-save)
(define-key leader-key-mode-mark-active-keymap (kbd "d")  'leader-key-mode--delete-region)
(define-key leader-key-mode-mark-active-keymap (kbd "n") 'next-line)
(define-key leader-key-mode-mark-active-keymap (kbd "o") 'exchange-point-and-mark)
(define-key leader-key-mode-mark-active-keymap (kbd "x")  'kill-region)
(define-key leader-key-mode-mark-active-keymap (kbd "y")  'leader-key-mode--delete-and-yank)

(define-key leader-key-mode-keymap (kbd "a") 'beginning-of-line)

(define-key leader-key-mode-keymap (kbd "b") (leader-key-mode--replay "C-x b"))
(define-key leader-key-mode-keymap (kbd "d") nil)
(define-key leader-key-mode-keymap (kbd "d l") 'leader-key-mode--duplicate-line)
(define-key leader-key-mode-keymap (kbd "n") 'next-line)
(defun leader-key-mode-select-symbol-at-point()
  (interactive "")
  (let* ((bounds
          (bounds-of-thing-at-point 'sexp))
         (b (car bounds))
         (e (cdr bounds)))
    (when bounds
      (goto-char b)
      (set-mark-command nil)
      (goto-char e))
    ))
(define-key leader-key-mode-keymap (kbd "s") 'leader-key-mode-select-symbol-at-point)
(define-key leader-key-mode-keymap (kbd "y") 'yank)
;;(define-key leader-key-mode-keymap (kbd "u") 'undo)
(define-key leader-key-mode-keymap (kbd "SPC") 'set-mark-command)
;;(define-key leader-key-mode-keymap (kbd ":") pp-eval-expression)
(define-key leader-key-mode-keymap (kbd "/") 'dabbrev-expand)
(define-key leader-key-mode-keymap (kbd "?") 'hippie-expand)
(define-key leader-key-mode-keymap (kbd "5") 'leader-key-mode--display-buffer-name)
(define-key leader-key-mode-keymap (kbd "7") 'compile)
(define-key leader-key-mode-keymap (kbd "=") '
(define-key leader-key-mode-keymap (kbd "`") 'next-error)
;;(define-key leader-key-mode-keymap (kbd "!") shell-command)
;;(define-key leader-key-mode-keymap (kbd "RET") execute-extended-command)
(define-key leader-key-mode-keymap (kbd "1") 'delete-other-windows)
(define-key leader-key-mode-keymap (kbd "2") 'mark-sexp)
(define-key leader-key-mode-keymap (kbd "3") #'(lambda()
                                                 (interactive)
                                                 (point-to-register 'a)))
(define-key leader-key-mode-keymap (kbd "4") 'kill-this-buffer)
(define-key leader-key-mode-keymap (kbd "`") #'(lambda()
                                                 (interactive)
                                                 (jump-to-register 'a)))
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
(define-key leader-key-mode-keymap (kbd "}") (leader-key-mode--replay "\\"))
(define-key leader-key-mode-keymap (kbd "o") (leader-key-mode--replay "C-x C-x"))
(define-key leader-key-mode-keymap (kbd "[") #'(lambda (arg) (interactive "P")
                                                 (insert-pair arg 91 93)))
(define-key leader-key-mode-keymap (kbd "]") (leader-key-mode--replay "\\"))
(define-key leader-key-mode-keymap (kbd "{") #'(lambda (arg) (interactive "P") (insert-pair arg 123 125)))

(define-key leader-key-mode-keymap (kbd "{") #'(lambda (arg) (interactive "P") (insert-pair arg 123 125)))

(define-key leader-key-mode-keymap (kbd "}") 'wcy-complete)
(define-key leader-key-mode-keymap (kbd "g") nil)
(define-key leader-key-mode-keymap (kbd "g c") 'avy-goto-char)
(define-key leader-key-mode-keymap (kbd "g C") 'avy-goto-char-2)
(define-key leader-key-mode-keymap (kbd "g l") 'avy-goto-line)
(define-key leader-key-mode-keymap (kbd "g w") 'avy-goto-word-1)
(define-key leader-key-mode-keymap (kbd "g v") 'avy-copy-region)
(define-key leader-key-mode-keymap (kbd "g x") 'avy-kill-region)
(define-key leader-key-mode-keymap (kbd ".") 'find-tag)
(define-key leader-key-mode-keymap (kbd "x") 'execute-extended-command)


(provide 'leader-key-mode)
;;; leader-key-mode.el ends here
