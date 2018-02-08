# 使用方法

## 環境変数の設定

SATySFiが設定ファイル等を読み込むために唯一用いる環境変数 `SATYSFI_LIB_ROOT` を設定します。この変数にはSATySFiが依存するファイル一式が格納されたディレクトリの絶対パスが記録されている必要があります。このディレクトリをSATySFiの *ライブラリルート* と呼びます。UNIX系OSの環境下では，典型的には `/usr/local/lib-satysfi` です。`~/.bash_profile` に

```
    export SATYSFI_LIB_ROOT=/usr/local/lib-satysfi
```

などと記載しておくとOSの起動時に自動的に環境変数を設定してくれます。


## ライブラリルート以下の構造

以下のようなディレクトリ構造になっている必要があります：

```
    $SATYSFI_LIB_ROOT/
    |
    +-- dist/
        |
        +-- fonts/
        |
        +-- hash/
        |   |
        |   +-- default-font.satysfi-hash
        |   +-- fonts.satysfi-hash
        |   +-- mathfonts.satysfi-hash
        |
        +-- hyph/
        |   |
        |   +-- english.satysfi-hyph
        |
        +-- packages/
        |
        +-- unidata/
            |
            +-- EastAsianWidth.txt
            +-- LineBreak.txt
            +-- Scripts.txt
```

まず `$SATYSFI_LIB_ROOT/` の直下にはディレクトリ `dist/` があります。これは内容物が将来的にパッケージマネージャやディストリビューションによって操作される（すなわち，ユーザが手で内容物を変更するべきでない）ことを意識して “distribution” から命名されていますが，現在はこのディレクトリ以下の内容をユーザが操作することは非推奨ではなく，また（カレントディレクトリが `$SATYSFI_LIB_ROOT/` 以下の状態でSATySFiを起動しない限り）`$SATYSFI_LIB_ROOT/` が内容を変更することもありません。

`dist/hash/` と `dist/unidata/` は設定ファイルの格納場所であり，どのような名前のファイルが直下に置かれている必要があるかも決められています。他のディレクトリは直下にデータを格納します。

`dist/hash/` にはSATySFiコードからデータにアクセスするためのいくつかの設定ファイルが格納されており，それぞれ *YOJSON形式* （`yojson` パッケージにより拡張されたJSON形式をこう呼ぶことにします）で記述されています。

`dist/unidata/` はUnicodeコードポイントを組版処理中にどのように扱うかを規定したファイルを格納しており，これはUnicode Databaseとして公開されているテキストファイル群の一部です。

`dist/fonts/` はSATySFiで使用するOpenTypeフォントファイルを直下に格納します。

`dist/hyph/` はハイフネーション辞書を直下に格納します。2018年2月5日現在は英語のハイフネーションのみに対応しており，`dist/hyph/english.satysfi-hyph` というデータのみを利用しますが，将来的に多言語へと拡張し，`dist/hash/` 以下でハイフネーション辞書の設定を扱う想定です。

`dist/packages/` はパッケージ類を格納します。ソースファイル `dist/packages/path/to/foo.satyh` を置くと，パッケージとして `@require: path/to/foo` で読み込めるようになります。2018年2月5日現在では `dist/packages/` からの相対パスをすべて指定しなければなりませんが，今後この仕組みをより洗煉することを検討しています。

## フォント設定

SATySFiで使用できるフォントは *フォントハッシュファイル* `$SATYSFI_LIB_ROOT/dist/hash/fonts.satysfi-hash` に設定の記載のあるフォントです。ユーザが新しいフォントをSATySFiで使えるように設定したい場合は，`$SATYSFI_LIB_ROOT/dist/fonts/` 直下にフォントファイル `bar.otf` を置き，フォントハッシュファイル内のディクショナリに

```
    "bar": <Single {"src-dist": "bar.otf"}>
```

というエントリを追記します。この設定を加えることでフォント名 `bar` がSATySFiで使えるようになります。フォントの（拡張子を除いた）ファイル名とフォント名は一致していなくても構いません。2018年2月5日現在，フォントファイルの形式はTrueType collectionではないOpenTypeフォントに限ります。

## 数式フォント設定

前節と同様に，SATySFiで使用する数式フォントについても *数式フォントハッシュファイル* `$SATYSFI_LIB_ROOT/dist/hash/mathfonts.satysfi-hash` に設定を記載します。記述する形式はフォントハッシュファイルと同様です。

## デフォルトのフォント設定

組版処理中で，特にフォント指定がない場合に使われるフォントは *デフォルトフォントハッシュファイル* `$SATYSFI_LIB_ROOT/dist/hash/default-font.satysfi-hash` で設定されているものです。デフォルトフォントハッシュファイルの内容は文字体系ごとに初期値とするフォントを指定したディクショナリです。2018年2月5日現在は文字体系として漢字，かな，ラテン文字，その他，の4つが定められています（勿論今後より多くの文字体系をサポートする想定ですが，現在のところ手前3つにあてはまらないコードポイントはその他扱いになります）。デフォルトフォントハッシュファイルは，例えば以下のようにして記述します：

```
    {
      "han-ideographic": {
        "font-name": "ipaexm",
        "ratio"    : 0.88,
        "rising"   : 0.0
      },
      "kana": {
        "font-name": "ipaexm",
        "ratio"    : 0.88,
        "rising"   : 0.0
      },
      "latin": {
        "font-name": "Arno",
        "ratio"    : 1.0,
        "rising"   : 0.0
      },
      "other-script": {
        "font-name": "Arno",
        "ratio"    : 1.0,
        "rising"   : 0.0
      }
    }
```

`ratio` エントリは拡大率，`rising` エントリは行方向に垂直な位置調整の指定です。上の設定では漢字と仮名を 0.88 倍の大きさにしています。

## デフォルトの数式フォント設定

2018年2月5日現在，デフォルトの数式フォントは `lmodern` で固定されています。近い将来に変更可能なように拡張します。
