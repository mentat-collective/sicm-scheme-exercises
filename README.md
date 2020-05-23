## SICM

This joins the other half-finished repositories for this tough book that you can find on GitHub. We'll see how I do with getting it working.

## Resources

[assignments](http://ocw.mit.edu/courses/earth-atmospheric-and-planetary-sciences/12-620j-classical-mechanics-a-computational-approach-fall-2008/assignments/) and [readings](http://ocw.mit.edu/courses/earth-atmospheric-and-planetary-sciences/12-620j-classical-mechanics-a-computational-approach-fall-2008/readings/) are listed on the [MIT open courseware site](http://ocw.mit.edu/courses/earth-atmospheric-and-planetary-sciences/12-620j-classical-mechanics-a-computational-approach-fall-2008/index.htm).

Here's the book in HTML form: http://mitpress.mit.edu/sites/default/files/titles/content/sicm/book.html

## Installations for scmutils on OS X

TODO!

## Docker Builds

### To Interact

Use this to get in and debug during installation.

```
docker build .
docker run --ipc host -it --entrypoint /bin/bash $CONTAINER_ID # that gets printed
```

Run the notebook with:
```
docker run --ipc host -it --rm -p 8888:8888 649c9549c1ec
```

## Mac Notes on how to get everything going

- Install XQuartz
- Activate the option ‘Allow connections from network clients’ in XQuartz settings
