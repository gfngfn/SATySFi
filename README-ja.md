![logo1](https://raw.githubusercontent.com/wiki/gfngfn/SATySFi/img/satysfi-logo.png)

[![Build Status](https://travis-ci.org/gfngfn/SATySFi.svg?branch=master)](https://travis-ci.org/gfngfn/SATySFi)

## 概要

*SATySFi*（英単語の “satisfy” と同様に発音します）は，新しい組版処理システムとその言語です。構文は主にテキスト部分とプログラム部分からなり，前者はLaTeX風の構文で文書を執筆するために，後者はコマンドを定義するために使われます。いわゆる函数型プログラミングの要領でコマンドが定義でき，かつ静的に型がつけられるため，柔軟な記述とわかりやすいエラー報告が両立されています。

本ソフトウェアは2017年度IPA未踏事業の1プロジェクトとして支援のもと開発されました。

<!--
Macrodown（マクロダウン） は
「マークアップの記述においてはマークアップ方法自体もユーザが設計できるべきである」
という思想のもとに制作された，TeX/LaTeXやHTML/CSSなどのマークアップ言語をラップするための軽量［要出典］マークアップ言語です。
主にテキスト階層とプログラム階層という2つの階層から成っており，
前者は文書を記述するためのLaTeX風の構文を，
後者は函数・マクロを定義するためのML風の構文を持っています。
函数・マクロ定義は強い静的型つけで，Hindley-Milner型システム（いわゆるlet多相）での型推論が可能です。
Macrodownなら，意味マークアップの機能を持つ柔軟なマクロを駆使して文書を記述することができるでしょう。

![logo2](https://raw.githubusercontent.com/wiki/gfngfn/Macrodown/img/macrodown-logo2.png)
-->

## インストール

### OPAMによる場合
* インストール：
  1. `git submodule update -i` を実行してサブモジュールをフェッチ
  2. `opam pin add satysfi .` を実行
  3. `opam install satysfi` を実行
* 再インストール： `opam reinstall satysfi` を実行
* アンインストール： `opam uninstall satysfi` を実行

<!--
###手動ビルド
1. ocamlbuild，ocamlfind，Menhirをインストール
2. このリポジトリをクローンし，`make` を実行
3. バイナリ `macrodown` が `Macrodown/` 直下に生成される
4. `make install` を実行し `/bin/macrodown` としてインストール
-->

## 用法

    satysfi <input files> -o <output file>

で `<input files>` から `<output file>` を出力します。例えばソースファイル `doc.saty` から `output.pdf` を出力したい場合，次のようにします：

    satysfi doc.saty -o output.pdf

## コマンドラインオプション

* `-v`, `--version`: ヴァージョンを表示します。
* `-o`, `--output`: 出力ファイル名を指定します。省略された場合，入力ファイル名の拡張子を `.pdf` に変えた名前を出力ファイル名とします。
* `--full-path`: 標準出力に書き込むログに於いて，ファイル名をすべて絶対パスで表示します。
