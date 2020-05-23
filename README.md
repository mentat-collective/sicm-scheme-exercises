## Structure and Interpretation of Classical Mechanics

Welcome to my solution and notes repository for Sussman and Wisdom's [The
Structure and Interpretation of Classical Mechanics](https://amzn.to/2LUx62M).
You can get a copy of the book on [Amazon](https://amzn.to/2LUx62M), or follow
along using this beautiful [HTML version](https://tgvaughan.github.io/sicm/).

<p align="center">
  <img src="https://user-images.githubusercontent.com/69635/82737469-6478a880-9cee-11ea-9559-aa0e85a60a5c.png" alt="SICM Cover"/>
</p>


I'm working through the exercises for each chapter using [MIT
Scheme](https://www.gnu.org/software/mit-scheme/), and the `scmutils` library
(also called `mechanics` by the authors and me) that Sussman and Wisdom
developed for the textbook.

Each chapter gets its own folder, named `ch1`, `ch2`, etc. Inside each folder
I've included a file for each exercise that required code or benefited from
exploration in Scheme.

I've also worked hard to make sure that it's easy for *you* to run any of this
code, and recreate my work in any of the exercises. I've provided a working copy
of the `mechanics` library in this repository in `bin/mechanics`. There's a
small amount of work you'll have to do to make the script run, but I've covered
that below under ["Running the Code"](#running-the-code).

## The Road to Reality

If you like this sort of thing, you might also consider subscribing to ["The
Road to Reality"](https://roadtoreality.substack.com/), a newsletter where I
post primers and lessons on interesting topics in math, physics, machine
learning or artificial intelligence, with a heavy emphasis on locking down the
intuition behind the ideas over mere symbol shuffling.

Check it out here: https://roadtoreality.substack.com

## Other SICM Resources

- The [course website](https://groups.csail.mit.edu/mac/users/gjs/6946/) for
  MIT's 6.946, ["Classical Mechanics: A Computational
  Approach"](https://groups.csail.mit.edu/mac/users/gjs/6946/). This is a
  graduate level course in classical mechanics that uses this textbook. The
  course page has excellent materials, links to errata, all the goods.
- [Errata for the 2nd edition of
  SICM](http://groups.csail.mit.edu/mac/users/gjs/6946/errata.pdf). I've found
  some more, which I'll add to this page as I go.
- [The SICM Textbook on Amazon](https://amzn.to/2LUx62M)
- [Beautiful HTML version of the SICM
  Textbook](https://tgvaughan.github.io/sicm/). You have to read this!
- [MIT OpenCourseware page for the Fall 2008 version of the
  course](https://ocw.mit.edu/courses/earth-atmospheric-and-planetary-sciences/12-620j-classical-mechanics-a-computational-approach-fall-2008/index.htm).
  This uses the first edition of the textbook. I haven't gone through this page
  in any detail.
- [Documentation for `scmutils`, aka
  `mechanics`](https://github.com/Tipoca/scmutils/blob/master/manual/refman.txt)
- [SICMUtils](https://github.com/littleredcomputer/sicmutils) a Clojure
  implementation of the `scmutils` / `mechanics` library. Clojure is brilliant.
  I may include some examples of code using this library as I go.
- My Dockerhub pages for the
  [`mit-scheme`](https://hub.docker.com/repository/docker/sritchie/mit-scheme)
  and [`mechanics`](https://hub.docker.com/repository/docker/sritchie/mechanics)
  Docker images.

## Running Mechanics

You can interact with any of the utilities or solutions I've written using the
`bin/mechanics` program in this repository. `bin/mechanics` holds a full,
working installation of `mechanics` and MIT Scheme, packaged up inside of a
Docker container. To make this work, you'll need, at minimum, a working
installation of [Docker](https://www.docker.com/).

If you want to see graphics (which of course you do!), you'll need an X11 window
system installed. For LaTeX rendering, you'll need the `xdvi` program, which
comes with installations of [LaTeX](https://www.latex-project.org/get/).

### OS X Prerequisites

If you're on a Mac, install [Docker Desktop for
Mac](https://hub.docker.com/editions/community/docker-ce-desktop-mac/) by
clicking the "Get Docker" button on the right side of that page.

[XQuartz](https://www.xquartz.org/) will cover the X11 requirement. Download the
file [here](https://www.xquartz.org/) and install it in the usual way.

Finally, install [MacTeX](https://tug.org/mactex/mactex-download.html) and
afterward make sure that typing `xdvi` at the terminal makes something happen.

Once you have XQuartz and MacTeX installed, you'll need to configure it to let
`bin/mechanics` make graphics on your computer from inside the Docker container
where it runs. To set this up,

- launch XQuartz from `/Applications/Utilities/Xquartz`,
- go into the Preferences menu and navigate to the Security tab
- make sure that both "Authenticate Connections" and "Allow connections from
  network clients** is checked

### Linux Prerequisites

[This page](https://docs.docker.com/engine/install/ubuntu/) has a nice guide on
how to get Docker installed on Linux. I think the X11 requirement might just
work? If you try this, please let me know, or send a PR updating this README.

The [LaTeX Project](https://www.latex-project.org/get/) main page describes how
to get LaTeX on Linux. They recommend installing the [TeX
Live](https://www.tug.org/texlive/) distribution.

### Starting a REPL

Once you have Docker installed, clone this repository onto your machine,
navigate into the folder and run `bin/mechanics`:

```
git clone https://github.com/sritchie/sicm.git && cd sicm
bin/mechanics
```

The first time you run this it should take a while, as you'll have to download
all of the Docker image requirements. Eventually you'll see an MIT Scheme REPL
with the whole `mechanics` library loaded:

```
127.0.0.1 being added to access control list
MIT/GNU Scheme running under GNU/Linux
Type `^C' (control-C) followed by `H' to obtain information about interrupts.

Copyright (C) 2019 Massachusetts Institute of Technology
This is free software; see the source for copying conditions. There is NO warranty; not even for MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

Image saved on Friday August 30, 2019 at 11:20:36 PM
  Release 10.1.10 || Microcode 15.3 || Runtime 15.7 || SF 4.41 || LIAR/x86-64 4.118 || SOS 1.8 || XML 1.0 || Edwin 3.117 || X11 1.3 || X11-Screen 1.0 || ScmUtils Mechanics.Summer 2019

1 ]=>
```

Go ahead and try evaluating some Lisp, using functions from the `mechanics` library:

```scheme
1 ]=> (square (up 'x 'y 'z))
#|
(+ (expt x 2) (expt y 2) (expt z 2))
|#
```

You're all set!

### Testing Graphics via X11

If you set up XQuartz / X11, you should be able to get LaTeX expressions printing.

To test this out, run the `bin/mechanics` script and try entering an expression
like `(show-expression (+ 'alpha 't))` at the REPL.

```
$ bin/mechanics
127.0.0.1 being added to access control list
MIT/GNU Scheme running under GNU/Linux
Type `^C' (control-C) followed by `H' to obtain information about interrupts.

Copyright (C) 2019 Massachusetts Institute of Technology
This is free software; see the source for copying conditions. There is NO warranty; not even for MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

Image saved on Friday August 30, 2019 at 11:20:36 PM
  Release 10.1.10 || Microcode 15.3 || Runtime 15.7 || SF 4.41 || LIAR/x86-64 4.118 || SOS 1.8 || XML 1.0 || Edwin 3.117 || X11 1.3 || X11-Screen 1.0 || ScmUtils Mechanics.Summer 2019

1 ]=> (show-expression (+ 'alpha 't))
(+ alpha t)
```

You should see a window pop up in XQuartz with a nicely rendered LaTeX
expression:

![](https://user-images.githubusercontent.com/69635/82736715-05646500-9ce9-11ea-9227-4db5fee32eb6.png)


The REPL will hang until you close the XQuartz window, so go ahead and do that
now.

## Running the Exercises

The exercises for each chapter live in their own folder, named `ch1`, `ch2`,
etc. Inside each folder I've included a file for each exercise that required
code or benefited from exploration in Scheme.

You can run any code in these folders by starting the REPL `bin/mechanics` from
the repository root folder, and calling, for example:

```scheme
(load "ch1/ex1-29.scm")
```

This will execute all code in the file located at `ch1/ex1-29.scm`, including
all side effects and graphics that the file generated. If you see LaTeX
expressions pop up, you'll have to close each one to move to the next one.

## Emacs Integration

If you want to interact more heavily with the code, add more exercises and work
on it yourself, I suggest you switch to a proper text editor with nice support
for Scheme. I use [Emacs](https://emacsformacosx.com/), specifically
[Spacemacs](https://samritchie.io/moving-to-spacemacs-for-scala-and-python/),
these days. (Here's my [Spacemacs
configuration](https://github.com/sritchie/spacemacs.d/tree/sritchie/google), if
that phrase means anything to you.)

I won't go into a whole spiel about how to set up Emacs, but I will note a
configuration options that I've found very helpful when working on these
exercises. (If you have guides you like, let me know and I'll link them here.)

### Run Mechanics Inside Emacs

Emacs has amazing built-in support for working with Scheme. I've added a
`mechanics` function to my Emacs config:

```elisp
(defun mechanics ()
    (interactive)
    (run-scheme "mechanics"))
```

(This works because I've added the `sicm/bin` directory to my path. You'll need
to do that too, or give this command the full path to the `bin/mechanics`
program.)

When I'm in a file in the project, I can type `M-x mechanics`, and Emacs will
start up a REPL running the `mechanics` code, with X11 support and everything.
Then, when I'm working on a file, `C-c C-z` will switch to the repl.

MAKE SURE you run this from the root of the project!! Go to `README.md` or
something and run it there. All of the `load` statements in all of the code
assume your main directory is the project root, so you'll run into problems if
you don't do this.

### Mechanics and Projectile

I actually use a slightly better version of the function above. If you use the
incredible [Projectile](https://docs.projectile.mx/en/latest/) library for Emacs
project navigation (and why wouldn't you??) you can make `mechanics` start
Scheme from the project root every time.

Here's the better version:

```elisp
(defun mechanics ()
  (interactive)
  (let ((default-directory
          (if (string= (file-name-extension buffer-file-name) "ml")
              (concat default-directory "..")
            default-directory))))
  (call-interactively #'run-scheme (vector "mechanics")))
```

This will attempt to use Projectile to launch Scheme in the root of your
project, and fall back to the current folder if you're not in a project when you
run `M-x mechanics`. Total gold!

## Other Emacs Customizations

There are a few other customizations that I've found helpful working with
MIT-Scheme and mechanics. I'll detail them here.

### Prettify Symbols / Lambdas

This block of configuration code that, in Scheme, changes "lambda" appearances
into nice, pretty greek symbols. This is called
[prettify-symbols-mode](https://emacsredux.com/blog/2014/08/25/a-peek-at-emacs-24-dot-4-prettify-symbols-mode/),
and looks great.

```elisp
;; This enables nice greek-letter lambdas whenever you type "lambda"
;; anywhere in a Scheme file.
(global-prettify-symbols-mode 1)

;; Some bonus code to get this working in other language modes:
(defun enable-pretty-lambdas ()
  "Make them beautiful!"
  (setq prettify-symbols-alist '(("lambda" . 955))))

;; I'm using Racket for a different project, so I wanted the good there too.
(add-hook 'racket-mode-hook 'enable-pretty-lambdas)
(add-hook 'racket-repl-mode-hook 'enable-pretty-lambdas)
```

### Smartparens

I use [smartparens](https://github.com/Fuco1/smartparens) to make sure that my
parentheses never get out of balance. This is absolutely essential when working
with Lisp.

I'm used to the
[Paredit](http://danmidwood.com/content/2014/11/21/animated-paredit.html)
keybindings from years back, so I run this setting to make sure that all of the
chords burned into my fingers don't have to change:

```elisp
(sp-use-paredit-bindings)
```

To learn how to use these bindings, and what this library can do for you, check
out Dan Midwood's amazing [Animated Guide to
Paredit](http://danmidwood.com/content/2014/11/21/animated-paredit.html).

Here's a [guide comparing Paredit and
Smartparens](https://github.com/Fuco1/smartparens/wiki/Paredit-and-smartparens),
so you don't get caught out by the differences.


## Native SCMUtils Installation on OS X

This method of installation is a little more involved, but if you decide that
you want to skip the Docker route and install `mechanics` and MIT Scheme
natively on OS X, these are the steps.

The instructions are
[here](http://groups.csail.mit.edu/mac/users/gjs/6946/installation.html), but
let's get through them more carefully.

- Install MIT Scheme: https://www.gnu.org/software/mit-scheme/
- Rename the ``MIT-Scheme` application in the Applications folder to
 `MIT-Scheme`, so we don't have to worry about escaping spaces in the terminal.
- Create a `~/bin` folder and get it on your `$PATH`.

Run the following command to make the `mit-scheme` binary available:

```bash
ln -s /Applications/MIT-Scheme.app/Contents/Resources/mit-scheme ~/bin/mit-scheme
```

Add this line to your `~/.bashrc` or `~/.bash_profile`, so that Scheme can see its libraries:

```bash
export MITSCHEME_LIBRARY_PATH=/Applications/MIT-Scheme.app/Contents/Resources
```

Test that this all worked by running `mit-scheme` at a fresh terminal. You should see the following:

```
$ mit-scheme
MIT/GNU Scheme running under OS X
Type `^C' (control-C) followed by `H' to obtain information about interrupts.

Copyright (C) 2019 Massachusetts Institute of Technology
This is free software; see the source for copying conditions. There is NO warranty; not even for MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

Image saved on Saturday August 10, 2019 at 6:28:48 PM
  Release 10.1.10 || Microcode 15.3 || Runtime 15.7 || SF 4.41 || LIAR/x86-64 4.118

1 ]=>
```

Once that's working, it's time to install the SCMUtils / Mechanics library.

### Manual SCMUtils Installation

The download link for the library lives at the [SICM course
website](http://groups.csail.mit.edu/mac/users/gjs/6946/installation.html), at
step 4. Here's the [direct download
link](http://groups.csail.mit.edu/mac/users/gjs/6946/scmutils-20190830.tar.gz)
if you prefer that.

The next few steps come from the instructions at the course site:

- Expand this gzipped tar archive by executing `tar xzf
  scmutils-20190830.tar.gz`. This will make a directory named
  `scmutils-20190830`.
- Run `cd scmutils-20190830 && ./install.sh` to install `scmutils` into your
  local Scheme installation's resource directory.
- Copy the starter script into `~/bin` by running `cp mechanics.sh ~/bin/mechanics`.

Test it all out by running `mechanics`. You should see the following:

```
$ mechanics
MIT/GNU Scheme running under OS X
Type `^C' (control-C) followed by `H' to obtain information about interrupts.

Copyright (C) 2019 Massachusetts Institute of Technology
This is free software; see the source for copying conditions. There is NO warranty; not even for MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

Image saved on Friday August 30, 2019 at 11:20:36 PM
  Release 10.1.10 || Microcode 15.3 || Runtime 15.7 || SF 4.41 || LIAR/x86-64 4.118 || SOS 1.8 || XML 1.0 || Edwin 3.117 || X11 1.3 || X11-Screen 1.0 || ScmUtils Mechanics.Summer 2019

1 ]=>
```

You're all set now, and can refer back to previous sections for instructions on
how to install X11 and LaTeX, work with the exercises, or configure Emacs.

## Thank you!

Thanks for following along! If you run into any issues, please file a ticket via
[Github Issues](https://github.com/sritchie/sicm/issues).

If you have any questions, or want to send me a note, you can do that at
sritchie09 at gmail.com.

## License

Copyright 2020, Sam Ritchie.

Licensed under the [Apache License, Version
2.0](http://www.apache.org/licenses/LICENSE-2.0).
