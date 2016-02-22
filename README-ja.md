![logo1](https://raw.githubusercontent.com/wiki/gfngfn/Macrodown/img/macrodown-logo1.png)

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

## 用法

まずMacrodownのGitHubリポジトリのReleaseから実行ファイルをダウンロードします。
もし自身でソースコードからコンパイルして実行ファイルを生成したいのであれば，
`make`と`ocamlc`/`ocamlopt`をあらかじめインストールしておき，
このリポジトリをクローンして`make`を実行してください。
すると`macrodown.exe`または`macrodown`が`bin/`に生成されます。


Windowsならば`macrodown.exe`にパスを通し，

    macrodown.exe <input files> -o <output file>

と，UNIX系の環境ならば

    ./macrodown <input files> -o <output file>

とそれぞれ打つことで`<input files>`（入力ファイルをスペースで区切ったもの）から`<output file>`を出力します。

言語仕様は`doc/introduction.mcrd`に日本語で記述されています。
