Wandbox for Emacser
===================

wandbox.el is a Emacs Lisp Library for Wandbox API.

README : [Japanese](README.ja.md) / English


What is Wandbox ?
-----------------

Wandbox is online compiler/interpreter.
Developed by @melponn and @kikairoya.

You can try various programming language sush as C, C++, D, Haskell,
Perl, Python, Ruby, PHP, Erlang, Java, JavaScript, CoffeeScript Rust,
Common Lisp, and more.

* [Wandbox Home](http://melpon.org/wandbox/)
* [API Reference](https://github.com/melpon/wandbox/blob/master/kennel2/API.rst)


Installation
------------

Install wandbox.el into your `load-path` directory.
and put `(require 'wandbox)` into your `.emacs`.


References
----------

### Command

* `wandbox-compile-file (filename)`  - Compile with file contents
* `wandbox-compile-region (from to)` - Compile marked region
* `wandbox-compile-buffer ()`        - Compile with current buffer

### Function

* `wandbox-compile (&rest profile &key compiler options code stdin compiler-option runtime-option lang name file save)`

  Note: `wandbox` is an alias definition.
  Run wandbox compile with detailed options.
  arguments `compiler`, `options`, `code`, `compiler-option`, `runtime-option`,
  `save` is based on wandbox API.

### Variable

* `wandbox-profiles` - List of wandbox compiler options.
* `wandbox-precompiled-hook` - Hook run before post wandbox. Return value will be merged into the old profile.


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
;; use standard-input
(wandbox :lang "Perl" :code "while (<>) { print uc($_); }" :stdin "hello")
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


License
-------

This software is licensed under the MIT-License.
