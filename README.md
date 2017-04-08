Wandbox for Emacser
===================

[![Build Status](https://travis-ci.org/kosh04/emacs-wandbox.svg?branch=master)](https://travis-ci.org/kosh04/emacs-wandbox)
[![MELPA](https://melpa.org/packages/wandbox-badge.svg)](https://melpa.org/#/wandbox)
[![MELPA Stable](https://stable.melpa.org/packages/wandbox-badge.svg)](https://stable.melpa.org/#/wandbox)

`wandbox.el` is Wandbox client tool.

README : [Japanese](README.ja.md) / English


What is Wandbox ?
-----------------

Wandbox is online compilation service.
Developed by @melponn and @kikairoya.

You can try various programming language such as C, C++, D, Haskell,
Perl, Python, Ruby, PHP, Erlang, Java, JavaScript, CoffeeScript Rust,
Common Lisp, and more.

* [Wandbox Home](https://wandbox.org)
* [API Reference](https://github.com/melpon/wandbox/blob/master/kennel2/API.rst)


Installation
------------

1. Install wandbox.el into your `load-path` directory.
   or `M-x package-install wandbox` if Emacs 24+.
2. and put `(require 'wandbox)` into your `.emacs`.

```elisp
;; .emacs sample
(require 'wandbox)
(global-set-key (kbd "C-c w w") 'wandbox)
(global-set-key (kbd "C-c w e") 'wandbox-eval-last-sexp)
(global-set-key (kbd "C-c w i") 'wandbox-insert-template)
(global-set-key (kbd "C-c w l") 'wandbox-list-compilers)
```

Usage
-----

1. Open some program source
2. Type `M-x wandbox` to copmile this buffer
3. The result are display to "Wandbox Output" buffer

for more details, see following reference.


References
----------

### Command

* `wandbox`                          - Alias of `wandbox-compile-buffer`
* `wandbox-compile-file (filename)`  - Compile with file contents
* `wandbox-compile-region (from to)` - Compile marked region
* `wandbox-compile-buffer ()`        - Compile with current buffer
* `wandbox-list-compilers ()`        - Display available compilers
* `wandbox-insert-template (name)`   - Insert template snippet
* `wandbox-select-server (name)`     - Switch to selected NAME wandbox server

### Function

* `wandbox` - Alias of `wandbox-compile`
* `wandbox-compile (&rest profile &key compiler options code stdin compiler-option runtime-option lang name file save)`

  Run wandbox compile with detailed options.
  arguments `compiler`, `options`, `code`, `compiler-option`, `runtime-option`,
  `save` is based on wandbox API.

* `wandbox-add-server (name location)` - Register wandbox server the name as `name`

### Variable

* `wandbox-profiles` - List of wandbox compiler options.
* `wandbox-precompiled-hook` - Hook run before post wandbox. Return value will be merged into the old profile.
* `wandbox-default-server` - Wandbox server name to use by default. (default "melpon")

Example
-------

```elisp
;; Compile code with compiler name and options
(wandbox :compiler "gcc-head" :options "warning" :code "main(){}")
```

```elisp
;; Compile as C source code (use wandbox-profiles)
(wandbox :lang "C" :code "main(){}")
```

```elisp
;; Compile local file, and generate permalink
(wandbox :lang "C" :file "/path/to/prog.c" :save t)
```

```elisp
;; Use standard-input
(wandbox :lang "Perl" :code "while (<>) { print uc($_); }" :stdin "hello")
```

```elisp
;; Multiple compile
(wandbox :profiles [(:name "php HEAD") (:name "php")] :code "<? echo phpversion();")
```

```elisp
;; Add user-profile
(add-to-list 'wandbox-profiles '(:name "ANSI C" :compiler "clang-head" :options "warning,c89"))
(wandbox :name "ANSI C" :file "/path/to/prog.c")
```

```elisp
;; Compile gist snippet
;; sample: https://gist.github.com/219882
(wandbox :name "CLISP" :gist 219882 :stdin "Uryyb Jbeyq!")
```

```elisp
;; Compile any url file (add :url keyword)
(defun* wandbox-option-url (&key url &allow-other-keys)
  (if url (plist-put nil :code (wandbox-fetch url))))
(add-to-list 'wandbox-precompiled-hook #'wandbox-option-url)

(wandbox :lang "C" :url "http://localhost/prog.c")
```

```elisp
;; Use other server
(wandbox-add-server "fetus" "https://wandbox.fetus.jp")

(wandbox :code "<?php echo phpversion();"
         :profiles [(:name "HHVM") (:name "HippyVM") (:name "PHP")]
         :server-name "fetus")

;; switch default server
;; or M-x wandbox-select-server fetus
(setq wandbox-default-server-name "fetus")
```


Tips
----

### Use new/latest compilers

The compiler list information available in Wandbox is saved
as a cache at the time of byte-compile.

If you want to use latest compilers, recompile `wandbox.el` and
restart Emacs.


License
-------

This software is licensed under the MIT-License.
