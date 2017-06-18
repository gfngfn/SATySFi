![logo1](https://raw.githubusercontent.com/wiki/gfngfn/Macrodown/img/macrodown-logo1.png)

[![Build Status](https://travis-ci.org/gfngfn/Macrodown.svg?branch=master)](https://travis-ci.org/gfngfn/Macrodown)

## 概要

Macrodown（マクロダウン） は
「マークアップの記述においてはマークアップ方法自体もユーザが設計できるべきである」
という思想のもとに制作された，TeX/LaTeXやHTML/CSSなどのマークアップ言語をラップするための軽量［要出典］マークアップ言語です。
主にテキスト階層とプログラム階層という2つの階層から成っており，
前者は文書を記述するためのLaTeX風の構文を，
後者は函数・マクロを定義するためのML風の構文を持っています。
函数・マクロ定義は強い静的型つけで，Hindley-Milner型システム（いわゆるlet多相）での型推論が可能です。
Macrodownなら，意味マークアップの機能を持つ柔軟なマクロを駆使して文書を記述することができるでしょう。

![logo2](https://raw.githubusercontent.com/wiki/gfngfn/Macrodown/img/macrodown-logo2.png)

## インストール

### OPAMによる場合
* インストール： `opam pin add macrodown` を実行
* 再インストール： `opam reinstall macrodown` を実行
* アンインストール： `opam uninstall macrodown` を実行

###手動ビルド
1. ocamlbuild，ocamlfind，Menhirをインストール
2. このリポジトリをクローンし，`make` を実行
3. バイナリ `macrodown` が `Macrodown/` 直下に生成される
4. `make install` を実行し `/bin/macrodown` としてインストール

## 用法

    macrodown <input files> -o <output file>

で`<input files>`（入力ファイルをスペースで区切ったもの）から`<output file>`を出力します。例えばソースファイル `doc.mcrd` からマクロ集 `macros.mcrdh` を用いて `output.tex` を出力したい場合，次のようにします：

    macrodown macros.mcrdh doc.mcrd -o output.tex

注意として，`macros.mcrdh` は `doc.mcrd` よりも先に記述されていなければなりません。

言語仕様は`doc/introduction.mcrd`に日本語で記述されています。
