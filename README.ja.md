Wandbox for Emacser
===================

[![MELPA](http://melpa.org/packages/wandbox-badge.svg)](http://melpa.org/#/wandbox)
[![Build Status](https://travis-ci.org/kosh04/emacs-wandbox.svg?branch=master)](https://travis-ci.org/kosh04/emacs-wandbox)

Emacs 用 Wandbox クライアント

README : 日本語 / [英語](README.md)


Wandox とは
-----------

Wandbox は @melponn と @kikairoya が開発したオンラインコンパイラです。
主に C++ に特化している他、C, Perl, Python, Ruby, PHP, Common Lisp など
様々な言語に対応しています。

* [Wandbox Home](http://wandbox.org)
* [API Reference](https://github.com/melpon/wandbox/blob/master/kennel2/API.rst)


インストール
------------

wandbox.el を拾ってきてロードして下さい。
Emacs24 以上ならば `M-x package-install wandbox` でもインストール出来ます。

使い方
------

適当なプログラムファイルを開いて M-x `wandbox` を実行すると
実行結果が Wandbox Output バッファに表示されます。

詳細なオプションを指定して実行したい場合は `wandbox-compile` を利用してください。


リファレンス
------------

### コマンド

* `wandbox ()`

  `wandbox-compile-buffer` を呼び出します。

* `wandbox-compile-file (filename)`

  指定したファイルをコンパイルします。
  呼び出すコンパイラは拡張子から自動判別されます。

* `wandbox-compile-region (from to)`

  指定したリージョンをコンパイルします。

* `wandbox-compile-buffer ()`

  現在のバッファをコンパイルします。

* `wandbox-list-compilers ()`

  利用可能なコンパイラ一覧を表示します。

### 関数

* `wandbox`

  `wandbox-compile` を呼び出します。

* `wandbox-compile (&rest profile &key compiler options code stdin compiler-option runtime-option lang name file save)`

  コンパイラやオプションなどを直接指定してコンパイルします。
  引数 `compiler`, `options`, `code`, `stdin`, `compiler-option`, `runtime-option` は
  Wandbox API に渡すための文字列パラメータを指定します。
  `compiler-option`, `runtime-option` はリスト形式で指定することもできます。

  また追加機能としてファイル名やプロファイル名の指定ができます。
  プロファイルについては `wandbox-profiles` を参照してください。

* `wandbox-add-server (name location)` - Wandbox サーバーを登録します

### 変数

* `wandbox-profiles`

  Wandbox API の呼び出しパラメータを予め設定しておくためのプロファイル (という名のテンプレート) 群です。
  ユーザ独自のプロファイルを追加することもできます。

* `wandbox-precompiled-hook`

  ここに関数を追加しておくことで、Wandbox を呼び出す前のプロファイルを編集できます。
  呼び出された関数の返り値はプロファイルにマージされます。


Example
-------

```elisp
;; コンパイラとオプションを指定してコンパイルする
(wandbox :compiler "gcc-head" :options "warning" :code "main(){}")
```

```elisp
コードを C 言語としてコンパイルする (プロファイルを利用)
(wandbox :lang "C" :code "main(){}")
```

```elisp
;; 指定したファイルをコンパイルして、パーマリンクを生成する
(wandbox :lang "C" :file "/path/to/prog.c" :save t)
```

```elisp
;; 標準入力を利用する
(wandbox :lang "Perl" :code "while (<>) { print uc($_); }" :stdin "hello")
```

```elisp
;; 複数プロファイルを利用する
(wandbox :profiles [(:name "php HEAD") (:name "php")] :code "<? echo phpversion();")
```

```elisp
;; 独自プロファイルを追加する
(add-to-list 'wandbox-profiles '(:name "ANSI C" :compiler "clang-head" :options "warning,c89"))
(wandbox :name "ANSI C" :file "/path/to/prog.c")
```

```elisp
;; gist のコード片をコンパイルする
;; sample: https://gist.github.com/219882
(wandbox :name "CLISP" :gist 219882 :stdin "Uryyb Jbeyq!")
```

```elisp
;; ファイルの代わりにURLを指定してコンパイルする (:url キーワードの追加)
(defun* wandbox-option-url (&key url &allow-other-keys)
  (if url (plist-put nil :code (wandbox-fetch url))))
(add-to-list 'wandbox-precompiled-hook #'wandbox-option-url)

(wandbox :lang "C" :url "http://localhost/prog.c")
```
```elisp
;; 別のサーバを利用する
(wandbox-add-server "fetus" "https://wandbox.fetus.jp")

(wandbox :code "<?php echo phpversion();"
         :profiles [(:name "HHVM") (:name "HippyVM") (:name "PHP")]
         :server-name "fetus")

;; デフォルトで利用するサーバを切り替える
;; もしくは M-x wandbox-select-server fetus
(setq wandbox-default-server-name "fetus")
```

Tips
----

### 最新のコンパイラ一覧を利用したい

このパッケージでは Wandbox で利用可能なコンパイラ一覧の情報を
バイトコンパイル時にキャッシュとして保存しています。

より最新のコンパイラ情報を利用したい場合は、`wandbox.el` を再度バイトコンパイルして
Emacs を再起動してください。


ライセンス
----------

このソフトウェアは MIT ライセンスのもとで公開されています。
