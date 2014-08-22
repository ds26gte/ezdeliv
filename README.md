# Hassle-free delivery of a multi-file Lisp program

Assume the end-user has an ANSI-compliant Common Lisp, and downloads your
program, which you distribute as a tarball named, say,
`coffee.tar.bz2`, that unpacks to a directory called `coffee`.  What is
the easiest way for them to use your program?

If your distribution contains the single Lisp file `coffee.lisp`,
the user can
load it into their CL, thus:

```
(load "coffee")
```
and use whatever functions, macros, and variables you provided as the
external interface.  They can even do

```
(compile-file "coffee")
```
and then

```
(load "coffee")
```

CL
is now smart enough to pick up the faster compiled file -- which has the same
basename as the source file, but with the file extension changed from
`.lisp` to something like `.fasl` (or `.fas`, `.dx64fsl`, or
`.sse2f`, depending on the user's CL implementation).

Typically, though, your distribution contains several Lisp files spread
across
several directories, to ease your development process, i.e., to
keep things neat and maintainable.  `coffee.lisp` may load `caf.lisp` and
`decaf.lisp`;
`caf.lisp` may then load `cappuccino.lisp`, `espresso.lisp`, and `mocha.lisp`;
and `decaf.lisp` may load a completely different
`cappuccino.lisp` and `mocha.lisp`
(in some other directory of course).  The various
calls to `load` use relative pathnames, viz.,

```
(load (merge-pathnames ... *load-pathname*))
```

So the user can still load
the "main" file `coffee.lisp`, as before:

```
(load "coffee")
```
and expect things to work.  Just to consolidate our example situation,
let's say the directory structure of the files in the distribution is as
follows:

```
coffee
├── coffee.lisp
├── type
│   ├── caf.lisp
│   └── decaf.lisp
├── caf
│   ├── cappuccino.lisp
│   ├── espresso.lisp
│   └── mocha.lisp
└── decaf
    ├── cappuccino.lisp
    └── mocha.lisp
```

Since all these several coffee-related files must be introducing symbols
galore, you of course want to define a package `coffee`, so that the
`coffee` symbols
don't clash with any symbols the user has, either of their own, or from
other packages.  So you put

```
(defpackage :coffee
  (:use :cl)
  (:export :WHATEVER ...))

(in-package :coffee)
```
at the head of `coffee.lisp`.  This should be enough, because when
`coffee.lisp` loads other files, and these other files load still other
files, the `load` function fluidly sets the special variable
`*package*` to the prevailing value of `*package*`, so
all the files will inherit `coffee` as their `*package*`.  However, for
the purpose of documentation, or maybe even to play nice with any slimy
text-editor setup you may have, you may still want to put

```
(in-package :coffee)
```
at the head of each constituent file.  It doesn't hurt.

Let's say the user likes your program, but finds it somewhat on
the slow side, and wants to speed things
up with
```
(compile-file "coffee")
```
What can you, as program developer, do to facilitate this?

## A solution

This is when most program deliverers reach for
[ASDF](http://common-lisp.net/project/asdf).  ASDF, however requires
some non-trivial effort on the
part of the user (and of course, much more on the part of the developer too).  The user has to download ASDF, they have to create a
registry directory, and for every program they download, they have to
put the `.asd` file in the registry directory, and they have to follow a
new syntax to load the program.  I wondered if it was possible to have
something really simple instead, where the user approach remains the
same as for a program consisting of a single Lisp file.  I.e.,
the user calls `load` on the main file, and they can optionally
`compile-file` that main file.

Turns out there is a protocol -- and quite an easy one -- for securing this.  Again, as
with adding `defpackage` and `in-package`, the little bit of
extra code goes just in
the main `coffee.lisp` file.  We place one requirement: all the
names of the
Lisp files and subdirectories they are in can be identified with Lisp
symbols without any escaping mechanism.  Thus, the file and directory
names are lowercase and are allowed to use the same characters that a
Lisp symbol can, provided they don't interfere with the OS's own
convention for unadorned file naming.  (E.g., the name can't be a number
even though the OS doesn't mind, and it can't contain a slash even though Lisp
doesn't mind.)

Recall that `coffee.lisp` loads `caf.lisp` and
`decaf.lisp`.  First, make sure that you don't specify the extension
`.lisp` in the calls to `load`, as we want the compiled versions
of the files to be picked up whenever possible.

Now add an expression to `coffee.lisp` that compiles these subfiles whenever
`coffee.lisp` itself is compiled.  We could try

```
(eval-when (:compile-toplevel)
  (compile-file (merge-pathnames "type/caf" *compile-file-pathname*))
  (compile-file (merge-pathnames "type/decaf" *compile-file-pathname*)))
```

But we don't just want to
compile
`type/caf.lisp` and `type/decaf.lisp`;
we also want to compile the other files
that these files load, viz., `caf/espresso.lisp`,
`caf/mocha.lisp`,
and
`caf/cappuccino.lisp`; and `decaf/mocha.lisp` and
`decaf/cappuccino.lisp`.  And we want them compiled in the right
order, because these files depend on each other in a certain order.
Instead of explicitly writing the various `compile-file`s in the
right order, we can simply specify the dependencies in a succinct
makefile-like way,
and have a Lisp macro take care of calling `compile-file`
on the files in the correct topological order.  For this we use the macro
`do-in-topological-order`, defined in the file
[dotopo.lisp](./dotopo.lisp):

```
(defmacro do-in-topological-order (fn &rest deps)
  (let ((all-items (remove-duplicates (apply #'append deps)))
        (f (gensym))
        (callf (gensym)))
    `(let ((,f ,fn)
           ,@(mapcar (lambda (x)
                       `(,x (vector ',x nil nil)))
                     all-items))
      ,@(mapcar
         (lambda (dep)
           (destructuring-bind (x &rest befores) dep
             `(setf (svref ,x 1) (list ,@befores))))
         deps)
      (labels ((,callf (x)
                (unless (svref x 2)
                  (setf (svref x 2) t)
                  (mapc #',callf (svref x 1))
                  (funcall ,f (svref x 0)))))
        ,@(mapcar
           (lambda (x) `(,callf ,x))
           all-items)))))
```

`do-in-topological-order`'s first argument is a function, and its
second argument is an expression of the dependencies among the symbols
representing the various files.  It topologically arranges the symbols, and
then calls the function on them, ensuring that a file is
treated before any file depending on it.  Here's a more maintainable way
to add the `compile-file`s to `coffee.lisp`,
assuming `dotopo.lisp` is in the same directory as
`coffee.lisp`:

```
(eval-when (:compile-toplevel)
  (load (merge-pathnames "dotopo" *compile-file-pathname*)))

(eval-when (:compile-toplevel)
  (do-in-topological-order
      (lambda (f)
        (compile-file (merge-pathnames (string-downcase (symbol-name f))
                                       *compile-file-pathname*)))
    (coffee type/caf type/decaf)
    (type/caf caf/cappuccino caf/espresso caf/mocha)
    (type/decaf decaf/cappuccino decaf/mocha)))
```

This is almost right, except for two issues, which we will now
address.

## Preventing repeated compile of main file

First, one of the files that
`do-in-topological-order`'s first argument will attempt to
compile is `coffee.lisp`, the file that we are already in the
process of compiling
when this expression is encountered! We could add a conditional
disallowing the loop in
`do-in-topological-order`'s first argument:

```
(eval-when (:compile-toplevel)
  (do-in-topological-order
      (lambda (f)
        (unless (eq f 'coffee) ;***
          (compile-file (merge-pathnames (string-downcase (symbol-name f))
                                         *compile-file-pathname*))))
    ...))
```

But there is an easier way: We could simply leave out (or comment
out) the
dependency line for `coffee.lisp`. Thus:

```
(eval-when (:compile-toplevel)
  (do-in-topological-order
      (lambda (f)
        (compile-file (merge-pathnames (string-downcase (symbol-name f))
                                       *compile-file-pathname*)))
   ;(coffee type/caf type/decaf) ;***
    (type/caf caf/cappuccino caf/espresso caf/mocha)
    (type/decaf decaf/cappuccino decaf/mocha)))
```

This works, but only because the files that `coffee.lisp` depends
on, viz., `type/caf.lisp` and `type/decaf.lisp`, are mentioned
elsewhere in the dependency list. If `coffee.lisp` depended on a
file `type/water.lisp` that nothing else depended on and that
didn't depend on anything else, then the
compiler wouldn't recognize that `type/water.lisp` needed to be
compiled at all. To accommodate this, we can add a dependency line for
`type/water.lisp` that listed no dependencies:

```
   ;(coffee type/caf type/decaf)
    (type/water ) ;***
    (type/caf caf/cappuccino caf/espresso caf/mocha)
    (type/decaf decaf/cappuccino decaf/mocha)
```

We of course need only do this for files that the main file
depends on. A file like `type/espresso.lisp`, which also doesn't
depend on anything else, doesn't need a line of its own because
it's already mentioned in the dependency line for
`type/caf.lisp`. But it doesn't hurt to add such a line anyway.

## Making special variables visible to dependent files

The second issue has to do with a file B using a special variable
introduced in another file A that B depends on.  In some
implementations, `compile-file` may
issue a warning that file B has an "undeclared free variable".
To avoid this annoyance, make the introduction of the special
variable in file A visible to `compile-file`, e.g.,

```
(eval-when (:compile-toplevel :load-toplevel :execute) ;***
  (defvar *bean-type*))
```

N.B.
[Clozure](http://ccl.clozure.com) and [ECL](http://ecls.sourceforge.net) issue
this warning, but it seems benign. [ABCL](http://abcl.org) and
[SBCL](http://sbcl.org) are properly
silent.

## Summary

That's all there is to it.

To summarize:

1. you include
`dotopo.lisp` in your distribution alongside the main file;

2. include the above changes to the content of that main file; and

3. make sure
that all special variables used outside their files are made visible to
the compiler.

The user who unpacks your tarball then simply `load`s your main
file, either as source, or after `compile-file`-ing it. They do
not have to worry about dealing with the other files at all, so
long as the latter's relative path to the main file is not
altered.
