Wandbox for Emacser
===================

[![MELPA](http://melpa.org/packages/wandbox-badge.svg)](http://melpa.org/#/wandbox)

wandbox.el is Wandbox API Library for Emacs.

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

1. Install wandbox.el into your `load-path` directory.
   or `M-x package-install wandbox` if Emacs 24+.
2. and put `(require 'wandbox)` into your `.emacs`.


References
----------

### Command

* `wandbox`                          - Alias of `wandbox-compile-buffer`
* `wandbox-compile-file (filename)`  - Compile with file contents
* `wandbox-compile-region (from to)` - Compile marked region
* `wandbox-compile-buffer ()`        - Compile with current buffer

### Function

* `wandbox` - Alias of `wandbox-compile`
* `wandbox-compile (&rest profile &key compiler options code stdin compiler-option runtime-option lang name file save)`

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


TODO
----

- [x] add test
- [x] add merge-plist
- [ ] configure response-data
- [x] handle gist snippet as code
- [ ] multiple profiles (e.g. compare gcc/clang)
- [ ] easy setting for compiler-options
- [ ] require request.el
- [x] auto generated profile (#2)


License
-------

This software is licensed under the MIT-License.
