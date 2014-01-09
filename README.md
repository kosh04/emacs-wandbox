Wandbox for Emacser
===================

Wandbox API を利用した Emacs 拡張ライブラリ.


## Wandox とは

Wandbox は @melpon と @kikairoya が開発したオンラインコンパイラです.
主に C++ に特化している他、C, Perl, Python, Ruby, PHP, Common Lisp など
様々な言語に対応しています.

Homepage
    [Wandbox](http://melpon.org/wandbox/)

API Reference
    [Wandbox API](https://github.com/melpon/wandbox/blob/master/kennel/API.rst)


## 使い方

wandbox.el を拾ってきてロードして下さい.

適当なファイルを開いて M-x `wandbox-compile-buffer` を実行すると
実行結果が *Wandbox Output* バッファに表示されます.

より詳細なオプションを指定したい場合は `wandbox-compile` を利用してください.


## リファレンス

M-x `wandbox-compile-file`
    指定したファイルをコンパイルします.

M-x `wandbox-compile-region`
    指定したリージョンをコンパイルします.

M-x `wandbox-compile-buffer`
    現在のバッファをコンパイルします.

Function `wandbox-compile`
    コンパイラやオプションなどを直接指定してコンパイルします.
    引数 compiler, options, code, stdin, compiler-option, runtime-option は
    Wandbox に渡すための API パラメータを指定します (文字列).
    compiler-option, runtime-option はリスト形式で指定することもできます.

    また追加機能として、ファイル名やテンプレートの指定ができます.
    詳しくは `wandbox-profiles` を参照してください.

Variable `wandbox-profiles`
    コンパイル時に必要な API パラメータのプロファイル (という名のテンプレート) 群です.
    例えば `(wandbox-compile :name "C" :code "main(){}")` を実行すると
    コードを C 言語としてコンパイルします.
    ユーザ独自のプロファイルを作成することもできます.

    ```Lisp
    (add-to-list 'wandbox-profiles
                 '(:name "ANSI C" :compiler "clang-head" :options "warning,c89"))
    (wandbox-compile :name "ANSI C" :code "...")
    ```

## TODO

- add merge-plist
- コンパイル結果のデータをユーザが弄れるようにする
- gist などのコード片を扱えるようにする
- 複数プロファイルの指定
- request.el を利用するべき？ (ただし依存関係が増える)


## ライセンス

このソフトウェアは MIT ライセンスのもとで公開されています.
