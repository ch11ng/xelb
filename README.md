# X protocol Emacs Lisp Binding

XELB (X protocol Emacs Lisp Binding) is a pure Elisp implementation of X11
protocol based on the XML description files from XCB project.
It features an object-oriented API and permits a certain degree of concurrency.
It should enable you to implement some low-level X11 applications.
Please refer to [xelb.el](https://github.com/ch11ng/xelb/blob/master/xelb.el)
for more details.

**Note to Emacs 24 users**:
If you install XELB from source (rather than GNU ELPA), be sure to install
`cl-generic` package from GNU ELPA first.
