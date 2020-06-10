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

The plugin lives
[here](https://chrome.google.com/webstore/detail/mathjax-3-plugin-for-gith/peoghobgdhejhcmgoppjpjcidngdfkod).
Install it in Chrome and you should see nicely LaTeX for all math on the
markdown files in this repository.

## Creating Github Markdown with LaTeX

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

(defun escape-gfm-latex-characters (text)
    (->> text
         (replace-in-string "\\(" "\\\\(")
         (replace-in-string "\\)" "\\\\)")
         (replace-in-string "\\[" "\\\\[")
         (replace-in-string "\\]" "\\\\]")
         (replace-in-string "_" "\\_")))

  (defun org-gfm-latex-filter (text backend info)
    "Properly escape code so it gets rendered."
    (when (org-export-derived-backend-p backend 'gfm)
      (escape-gfm-latex-characters text)))

  (require 'ox)

  ;; These are required to get proper escaping in github-flavored markdown for
  ;; latex snippets and embedded equations and environments.
  (add-to-list 'org-export-filter-latex-fragment-functions 'gfm-latex-filter)
  (add-to-list 'org-export-filter-latex-environment-functions 'gfm-latex-filter)

  ;; Override the built-in stuff in org-md-export-block, since I don't want to
  ;; declare my own backend and
  (defun org-md-export-block (export-block contents info)
    "Transcode a EXPORT-BLOCK element from Org to Markdown.
CONTENTS is nil. INFO is a plist holding contextual information."
    (let ((prop-type (org-element-property :type export-block)))
      (message contents)
      (cond ((string= prop-type "LATEX")
             (escape-gfm-latex-characters
              (org-remove-indentation (org-element-property :value export-block))))

            ;; this is the default markdown behavior.
            ((member prop-type '("MARKDOWN" "MD"))
             (org-remove-indentation (org-element-property :value export-block)))

            ;; Also include the default for HTML export blocks.
            (t (org-export-with-backend 'html export-block contents info)))))
```

If you add that to your emacs initialization code, you'll find that `ox-gfm`
Does the Right Thing when you try to export to Github-flavored markdown, and
equations show up looking great.

I also had to override the default markdown processing, so that it would emit latex blocks:



## Custom LaTeX Processing

I modified `exdisplay.scm` to use proper LaTeX environment style, like
`\begin{pmatrix}\end{pmatrix}` instead of `\matrix{}`.

This led to a bug discovery! I've included the modified file in
`ch1/exdisplay.scm`. This has the bugfix, and the upgraded `matrix` printing for
up and down tuples. (Without this change, MathJax and XDVI will be able to
handle rendering no problem, but LaTeX itself will choke.)

## Embedding Gifs

### Giphy

This should be super easy. Upload, then drop in the image. Make sure to use
`.gif`, not `.gifv`.

### Dropbox

This does NOT work now, since Dropbox seems to grab the gif and rewrite it. But
here's what I've learned. If you do this on Dropbox, you need to:

- Get the link!
- add `?dl=1` to the end of the URL

Then the image will embed properly. You'll also have to add this to your emacs config:

```emacs-lisp
;; This adds support for embedding dropbox images
(add-to-list 'org-html-inline-image-rules
               `("https" . ,(format "\\.%s\\'"
                                    (regexp-opt
                                     '("gif?dl=1")
                                     t))))
```

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
