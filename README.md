# Horn

This is a simple package for you to define and to call your favourite functions
and modes.
It's good to be able to rapidly call a mode instead of defining even
more keybindings, keeping the complexity of your configuration as low
as possible.
Of course a simple `M-x` can do that for you, specially using nice tools such as
`ivy`, `ido`, and many others.

The only advantage on top of `M-x` is to be succinct, a `M-x magit` will show me
480 magit functions to be called, hopefully the desired function will be
highlighted thanks to `swiper`'s (or some other package) "results by history",
but still a lot of information.
Once you've defined your horn list, you can easily call a mode.
It's like bookmarking a function or mode.

Apart from calling defined modes and functions, Horn have the objective of also
calling buffers.
We already have good options such as `ibuffer` and `ivy-switch-buffer`.
`ibuffer` can be very organized but it will put you in another buffer,
and despite `ivy-switch-buffer` offers more power like showing
"last oppened files" and "bookmarks", it's not very much organized.

Horn aims to offer ibuffer organization for a ivy-switch-buffer -like interface.

## Instalation

Soon on Melpa.

For now, clone to your `.emacs.d/` and `require` the package in your `init.el`.

```elisp
;;; Horn
(add-to-list 'load-path "~/.emacs.d/emacs-horn")
(require 'horn)
```

## Configuration

For easy of use you should create a shortcut so you can call your favourite
modes or functions right away.

```elisp
(global-set-key (kbd "C-x C-x") 'horn-call-mode)
```

## TODO
Define the customizable variable and document how to customize it in README.md.
Work on parsing the buffer list.
