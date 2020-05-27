# Modifications for Org-Mode

The repository has exercises in an org-mode file at the repo root. That file
gets published regularly as a PDF and a Markdown file that lives in the git
root.

Any of the exercises that require code are tangled out of that org-file.

## Modifying Emacs for Publishing

I had to do some seriously annoying legwork to get the org-mode publish working.
I'm going to log it here, and tidy it up if anyone's interested in making this
happen.

We want to use `xscheme` for org-mode code evaluation, but `cmuscheme` for the
actual interaction with the repl. This is annoying to make happen.

- Go into `xscheme` and erase these two lines:

```emacs-lisp
(xscheme-evaluation-commands scheme-mode-map)
(xscheme-interrupt-commands scheme-mode-map)
```

That will prevent xscheme from stomping all the repl and scheme keyboard
bindings.

Next, go into `cmuscheme` and change `run-scheme` to `run-cmu-scheme`. This will
give us a NEW name

These changes will not follow you around as you move machines, and don't live in
your init file. So don't forget that you'll have to do this again if you
upgrade!

### Custom Publishing

To get the custom publishing going, you'll need to install my [custom org mode
evaluator](https://github.com/sritchie/spacemacs.d/blob/sritchie/google/ob-mit-scheme.el).
Place that in your `~/.spacemacs.d`, or `~/.emacs.d` folder if you're not using
spacemacs.

Go get it downloaded at [this
location](https://github.com/sritchie/spacemacs.d/blob/sritchie/google/ob-mit-scheme.el),
then add these two lines to your emacs init:

```emacs-lisp
(load "~/.spacemacs.d/ob-mit-scheme.el")
(require 'ob-mit-scheme)
```

This, in combination with the modifications to xscheme and cmuscheme above, will let you:

- run `mechanics` to start up an inferior scheme process with cmuscheme
- let `xscheme` handle actually executing code for org-mode in its own buffer.

I tried making it work with Geiser, but it was a total nightmare. Geiser tries
to `read` the results of the execution block, which apparently blows up
`mit-scheme`.

### Init Modifications

You'll have to modify your `mechanics` function to call the new `cmuscheme`
start function. The new functions in your init block will look like this:

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

## Nice Equation Rendering

You can render LaTeX inline in org mode!

To get nice equation rendering on OS X, make sure you have `dvisvgm` installed,
and add this to your init file:

```emacs-lisp
(setq org-latex-create-formula-image-program 'dvisvgm)
```

If you're on a Mac, you'll notice that the previews don't actually display. To get that

## How to Export

To generate the markdown file, go to `sicm.org` and run `C-c C-e g g` to
regenerate the markdown file.

[ox-gfm](https://github.com/larstvei/ox-gfm) is what makes it possible to export
in github-flavored markdown. For this to work in Spacemacs, you'll need to make
sure that your `org` setup looks like this:

```emacs-lisp
  (org :variables
        org-enable-github-support t
        org-enable-hugo-support t
        org-enable-org-journal-support t
        org-want-todo-bindings t)
```

Run `C-c C-e l p` to generate PDF.

## Tangling Code

Once I get some exercises transferred over, I'll update this page with
instructions on how to tangle the exercise code out of the org-mode file.

## Seeing Equations

I've forked this [nice Mathjax plugin for
Github](https://chrome.google.com/webstore/detail/mathjax-plugin-for-github/ioemnmodlmafdkllaclgeombjnmnbima?hl=en)
to make it work with a more modern MathJax. I've also modified the config to
work with the LaTeX syntax that org-mode emits.

I'm working on getting this published as a public Chrome extension. In the
meantime, you can install the plugin locally with these instructions.

1. Click [this
   link](https://github.com/sritchie/sicm/raw/master/bin/github-mathjax.crx) to
   download the extension from https://github.com/sritchie/sicm/tree/master/bin
2. Go to chrome://extensions and click the "Developer Mode" toggle in the top
   right of the page
3. Drag the file you just downloaded (github-mathjax.crx) onto the Chrome
   window, and follow the prompts to install the plugin.
4. Scroll through https://github.com/sritchie/sicm/blob/master/sicm.md and see
   if you see any equations rendering!

I'll update this with a link once I get the plugin up on the Chrome Web Store.

## Creating Github Markdown with Equations

The Github-flavored markdown that `ox-gfm` emits turns all LaTeX fragments into
the standard `\( equation surrounded by parens \)` or `\[ square braces \]`.
Github renders markdown by treating those as escapes for the parens or braces...
and because the backslash is missing, Mathjax won't render equations.

We can add some customization to org-mode to get it to add an extra backslash
escape, which will force Github's renderer to show the equations properly.

Here's what I had to do to get this working:

```emacs-lisp
  (defmacro ->> (&rest body)
    (let ((result (pop body)))
      (dolist (form body result)
        (setq result (append form (list result))))))

  (defun replace-in-string (what with in)
    (replace-regexp-in-string (regexp-quote what) with in nil 'literal))


  (defun org-gfm-latex-fragment (latex-fragment contents info)
    "Transcode SRC-BLOCK element into Github Flavored Markdown
format. CONTENTS is nil.  INFO is a plist used as a communication
channel."

    (let* ((latex-frag (org-element-property :value latex-fragment)))
      (message "Got it!")
      (message latex-frag)
      "face"))

  (defun my-latex-filter-gfm-macro (text backend info)
    "Replace \\R with \\mathbb{R}"
    (when (org-export-derived-backend-p backend 'gfm)
      (->> text
           (replace-in-string "\\(" "\\\\(")
           (replace-in-string "\\)" "\\\\)")
           (replace-in-string "\\[" "\\\\[")
           (replace-in-string "\\]" "\\\\]"))))

  (add-to-list 'org-export-filter-latex-fragment-functions
               'my-latex-filter-gfm-macro)
```

If you add that to your emacs initialization code, you'll find that `ox-gfm`
Does the Right Thing when you try to export to Github-flavored markdown, and
equations show up looking great.

## Custom LaTeX Processing

I modified `exdisplay.scm` to use proper LaTeX environment style, like
`\begin{pmatrix}\end{pmatrix}` instead of `\matrix{}`.

This led to a bug discovery! I've included the modified file in
`ch1/exdisplay.scm`. This has the bugfix, and the upgraded `matrix` printing for
up and down tuples. (Without this change, MathJax and XDVI will be able to
handle rendering no problem, but LaTeX itself will choke.)

## Debugging

If you use `org-journal`, you may have to modify `org-journal-is-journal` to be
this:

```emacs-lisp
(defun org-journal-is-journal ()
  "Determine if file is a journal file."
  (when (buffer-file-name)
    (string-match (org-journal-dir-and-file-format->pattern)
                  (buffer-file-name))))
```

You'll know if you see that error. So annoying!
