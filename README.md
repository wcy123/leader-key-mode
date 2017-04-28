leader-key-mode
===============


This is a very simple mode to mimic the concept of leader key in VIM. `evil` is great, but I am not so used to mode switching, i.e. 'normal mode', 'insert mode', 'visual mode' etc. To prevent from [RSI][], I just need more single key stroke bindings.

[RSI]: https://www.emacswiki.org/emacs/RepeatedStrainInjury


# How to use it

`(require 'leader-key-mode)`

Then select a text, `c` for copy, `y` for yank, `x` for cut, `d` for delete. When mark is active, `c`, `y`, `x`, and `d` are bound to commands other than `self-insert`.


We can bind `\ b` to `C-x b`。 Then we cannot use `\` to insert a literal "\\". Fortunately, I don't use "\\" often. I can use `C-q \\` to insert a "\\".

Please read the source code to find more. Do be shy to read the source code, it is simple. The core part is as below.

```elisp
(define-minor-mode leader-key-mode
  "Minor mode to support <leader> support." t)
(defvar leader-key-mode-mark-active-keyma (make-sparse-keymap))
(defvar leader-key-mode-keymap (make-sparse-keymap))  
(defconst leader-key-mode--emulation-mode-map-alist
  `((mark-active ,@leader-key-mode-mark-active-keymap)
    (leader-key-mode ,@(leader-key-mode-create-entry-keymap leader-key))))
(add-to-list 'emulation-mode-map-alists
             'leader-key-mode--emulation-mode-map-alist)
```

Read document about `emulation-mode-map-alists` for more detail.
