# org-inline-image

This package adds functionality to inline images into an `org-mode`
buffer.  The images can be present locally on the filesystem (not
implemented yet) or downloaded from the internet automatically.

In addition to a simple direct-to-image links, this package
supports a resolving mechanism to inline images from popular
websites such as imgur, deviantart, tumblr and others.  There is
also planned support for galleries.

An example use-case is a bookmarks file, where the user can easily
display the images/galleries in emacs without switching to the
browser, which is often not necessary.

# Use

Call `org-inline-image` when the point is on the link to inline it
there.  The link text will be overlayed with the image.  To hide
the image, hit `h` (or call `org-inline-image-hide`) while the
point is on the image.  Gif images are animated automatically when
inlined.  To animate it again, hit `a` (or call
`org-inline-image-animate`).


# Supported websites

See the file `examples.org` for all supported links.  If you want
your preferred site to be supported, please write a resolver and
submit a patch, or at least start an issue where details could be
discussed.

The built in `org-inline-image--regexp-resolver` uses an alist
`org-inline-image-regexp-resolver-alist` to feed the URL to a
function which will return the URL of the image by matching the
supplied URL to a regexp.  For a more sophisticated resolver, you
can write a custom function and add it to
`org-inline-image-resolvers`.  See the documentation of the
mentioned functions for more informations.
