# Modifications for Org-Mode

I had to do some seriously annoying legwork to get the org-mode publish working.

- Go into `xscheme` and erase these two lines:

```emacs-lisp
(xscheme-evaluation-commands scheme-mode-map)
(xscheme-interrupt-commands scheme-mode-map)
```

- Go into `cmuscheme` and change `run-scheme` to `run-cmu-scheme`.

### Custom Publishing

To get the custom publishing going, you'll need to install my [custom org mode
evaluator](https://github.com/sritchie/spacemacs.d/blob/sritchie/google/ob-mit-scheme.el).
Place that in your `~/.spacemacs.d`, or `~/.emacs.d` folder if you're not using
spacemacs.

Once it's there, you can add these two lines to your emacs init.

```emacs-lisp
(load "~/.spacemacs.d/ob-mit-scheme.el")
(require 'ob-mit-scheme)
```

This, in combination with the modifications to xscheme and cmuscheme above, will let you:

- run `mechanics` to start up an inferior scheme process with cmuscheme
- let `xscheme` handle actually executing code for org-mode in its own buffer.

### Nice Rendering

To get nice equation rendering, make sure you have `dvisvgm` installed, and add
this to your init file:

```emacs-lisp
(setq org-latex-create-formula-image-program 'dvisvgm)
```

If you're on a Mac, you'll notice that the previews don't actually display. To get that

### Init Modifications

The new functions in your init block will look like this:

```emacs-lisp
  (require 'cmuscheme)

  (defun mechanics-local ()
    (interactive)
    (run-cmu-scheme "mechanics"))

  ;; And finally, the goods for SICM.
  (defun mechanics ()
    (interactive)
    (let ((default-directory (or (projectile-project-root)
                                 default-directory)))
      (call-interactively #'mechanics-local)))

  ;; Here's an older version that does NOT use my docker stuff.
  (defun mechanics-osx ()
    (interactive)
    (run-cmu-scheme "mechanics-osx"))
```

## Seeing Equations

Install this Mathjax plugin: https://chrome.google.com/webstore/detail/mathjax-plugin-for-github/ioemnmodlmafdkllaclgeombjnmnbima?hl=en

Here's how to get the default installed locally:
https://developer.chrome.com/extensions/getstarted#manifest
