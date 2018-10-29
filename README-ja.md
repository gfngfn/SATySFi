<!-- -*- coding: utf-8 -*- -->
![logo1](https://raw.githubusercontent.com/wiki/gfngfn/SATySFi/img/satysfi-logo.png)

[![Build Status](https://travis-ci.org/gfngfn/SATySFi.svg?branch=master)](https://travis-ci.org/gfngfn/SATySFi)

[English README is here](https://github.com/gfngfn/SATySFi/blob/master/README.md)

## 概要

*SATySFi*（英単語の “satisfy” と同様に発音します）は，新しい組版処理システムとその言語です。構文は主にテキスト部分とプログラム部分からなり，前者はLaTeX風の構文で文書を執筆するために，後者はコマンドを定義するために使われます。いわゆる函数型プログラミングの要領でコマンドが定義でき，かつ静的に型がつけられるため，柔軟な記述とわかりやすいエラー報告が実現されています。

本ソフトウェアは2017年度IPA未踏事業の1プロジェクトとして支援のもと開発されました。（概要は[こちら](https://www.ipa.go.jp/jinzai/mitou/2017/gaiyou_t-4.html)）

## Homebrew を使ったインストール方法 (Mac ユーザ向け)

Homebrew のフォーミュラが用意されています。

```sh
$ brew install --HEAD nyuichi/satysfi/satysfi
```

## OPAM を使ったインストール方法

### 事前に必要なもの

ビルド前に最低限，以下のソフトウェアが必要です。

* bzip2
* cc
* git
* m4
* make
* unzip
* wget or curl
* ruby
* [opam](https://opam.ocaml.org/) 2.0 （インストール手順は[こちら](https://opam.ocaml.org/doc/Install.html)。）
    * opam 2 をインストールするのに必要なツールである bubblewrap は，いくつかの環境において未だ簡単にはインストールできません。たとえば Windows Subsystem for Linux（WSL）や Ubuntu 16.04 が該当します。さしあたりの回避法として，`opam init` をする際に `--disable-sandboxing` オプションを渡すことで opam 2 を bubblewrap 無しにインストールすることができます。**詳細を [opam の FAQ](https://opam.ocaml.org/doc/FAQ.html#Why-does-opam-require-bwrap) で必ずご確認ください。**
* ocaml 4.06.0 （OPAM からインストールします）

また，ビルドには外部 OPAM リポジトリの追加が必要です。これは以下のコマンドでできます。

```sh
opam repository add satysfi-external https://github.com/gfngfn/satysfi-external-repo.git
opam update
```

#### 準備例（Ubuntu）

```sh
sudo apt-get update
sudo apt-get install build-essential git m4 unzip curl

sh <(curl -sL https://raw.githubusercontent.com/ocaml/opam/master/shell/install.sh)

# 以下のコマンドは OPAM がファイルに追記してもよいか聞いてきます。
# 必ず説明を読み，環境変数を適切に設定してください。
opam init --comp 4.06.0

eval $(opam env)

opam repository add satysfi-external https://github.com/gfngfn/satysfi-external-repo.git
opam update
```

#### 準備例（OS X Mavericks 以降）

```sh
# このスクリプトを実行する前に，GCC や Make などの基本的なソフトウェアをインストールしておいてください。これらは Xcode Command Line Tools からインストールできます。
# また，Homebrew もインストールしてください。

brew update
brew install opam

# 以下のコマンドは OPAM が（~/.bash_profile などの）ファイルに環境変数に関する設定を追記してもよいか聞いてきます。
# 必ず説明を読み，環境変数を適切に設定してください。
opam init --comp 4.06.0

eval $(opam env)

opam repository add satysfi-external https://github.com/gfngfn/satysfi-external-repo.git
opam update
```

### ビルド

まず，このリポジトリとサブモジュールを clone します。その後 OPAM を使って SATySFi をビルドします。

```sh
# clone
git clone https://github.com/gfngfn/SATySFi.git
cd SATySFi

# build
opam pin add satysfi .
opam install satysfi
```

* 再インストール： `opam reinstall satysfi` を実行
* アンインストール： `opam uninstall satysfi` を実行

## 用法

```sh
satysfi <input file> -o <output file>
```

で `<input file>` から `<output file>` を出力します。例えばソースファイル `doc.saty` から `output.pdf` を出力したい場合，次のようにします：

```sh
satysfi doc.saty -o output.pdf
```

## コマンドラインオプション

* `-v`, `--version`: ヴァージョンを表示します。
* `-o`, `--output`: 出力ファイル名を指定します。省略された場合，入力ファイル名の拡張子を `.pdf` に変えた名前を出力ファイル名とします。
* `-b`, `--bytecomp`: 評価前にバイトコンパイルを行ないます（複雑な計算に対して高速化が期待できます）。
* `--full-path`: 標準出力に書き込むログに於いて，ファイル名をすべて絶対パスで表示します。
* `--type-check-only`: 型検査だけをして終了します。
* `--debug-show-bbox`: （デバッグ目的で）各グリフにバウンディングボックスをつけて出力します。
* `--debug-show-space`: （デバッグ目的で）スペース部分に目印をつけて出力します。
