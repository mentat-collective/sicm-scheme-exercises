# My Workflow

I'm writing this whole thing in org-mode. Here are some tips about how the project is structured.

## Header Args

It takes a while to evaluate each code block, so I've added this to each section:

```
  :PROPERTIES:
  :header-args: :eval no-export
  :END:
```

Then, on each exercise, to add to the default instead of wiping them out, I have a block like this:

```
**** Exercise 9.2: Computing Derivatives
     :PROPERTIES:
     :header-args+: :tangle ch9/ex9-2.scm
     :END:
```

## Publishing Text with Exercises

If you include `:comments org`, you'll get all of the surrounding context
spliced in around the exercises.

## Noweb

Say you want to show off some block of code, but splice it in later.
