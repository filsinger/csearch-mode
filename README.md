# csearch-mode

An emacs minor mode for [codesearch](http://code.google.com/p/codesearch/).

## Usage

```lisp
(require 'csearch-mode)
```

## Notes

This is still in early development.

### OS X Users

If you are running the GUI version of emacs and not executing from the terminal
OS X will not provide emacs with the correct search path for executable files if
you have custom paths set.  This can cause csearch to fail to execute if you have
csearch located in a non-standard search path.

If this is the case you can set the csearch executable path manually in your `.emacs` file.

```lisp
(setq csearch/csearch-program "~/bin/csearch")
(setq csearch/cindex-program "~/bin/cindex")
```
