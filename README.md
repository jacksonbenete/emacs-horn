# Horn

## Screenshot
![full list of defined functions][1]

![list after completing][2]

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

This is a very much simple package, more like a toy for learning Elisp.
It will not be on Melpa.

Clone to your `.emacs.d/` and `require` the package in your `init.el`.

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

To customize your list, you can either `customize` or set the values directly:

```elisp
(setq horn-default-list
      '(("Deft" . deft)
      ("Magit" . magit)
      ("Shell" . shell)
      ("Eshell" . eshell)))
```

The first parameter is a identifier for the function or module to be shown on
the list.

The second parameter, followed by a `dot` is the function or mode name.

## TODO
- Work on parsing the buffer list without third party functions.

[1]: ./screenshots/emacs-horn-full-list.png
[2]: ./screenshots/emacs-horn-completing.png
