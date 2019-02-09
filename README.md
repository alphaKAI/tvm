# TVM

## 概要
  TVMとは，高階関数を持つ最低限のプログラミング言語をToyScriptと定義し，それに対する，パーサー，コンパイラ，VM(スタックベース)からなるシステムである．  
筑波大学の情報学郡情報科学類開設の[情報特別演習Ⅰ](http://www.coins.tsukuba.ac.jp/syllabus/GB13312_GB13322.html)にて開発した．  
TVMの詳細な仕様や実装については以下のレポートに詳細に記したので，そちらを参照されたい．また，TVMの概要についてはスライドを見ると短時間で把握しやすい．

- レポート : [高階関数を持つ言語に対するスタックベースのVM型処理系の実装](https://alpha-kai-net.info/info_sp_2018/report.pdf)  
- スライド : [2018年度 情報特別演習 採集発表 - 高階関数を持つ言語に対するスタックベースのVM型処理系の実装](https://nc.alpha-kai-net.info/s/YxWkRtbFMn5pFcP#pdfviewer)

## プログラム例
以下のようなプログラムを記述することが可能である．

### Hello, world

```js
println("Hello, world");
```

### ループの例

```js
for (var i = 0; i < 10; i = i + 1) {
  println(i);
}
```

### 簡単な高階関数の例

```js
///関数fを受け取り，fに値x,yを適用する
function exec(f, x, y) { return f(x, y); }
///掛け算を行う関数
function mul(x, y) { return x * y; }
///足し算を行う関数を返す関数
function retAdd() {
  function add(x, y) { return x + y; }
  return add;
}

println(exec(mul, 3, 4));// -> 12
println(exec(retAdd(), 3, 4));// -> 7
```

### クロージャの例

```js
/// カウンターとして動作するクロージャを返す関数
function counter() {
  var count = 0;
  function countup() {
    count = count + 1;
    return count;
  }
  return countup;
}

var c = counter();
println(c()); // -> 1
println(c()); // -> 2
println(c()); // -> 3
```

### フィボナッチ数列の例

```js
function fib(n) {
  if (n < 2) {
    return n;
  } else {
    return fib(n - 2) + fib(n - 1);
  }
}

var n = 10;

println(fib(n));
```

### foldの例(少し高度な高階関数の例)

```js
function fold(f, v, lst, len, idx) {
  if (len == idx) {
    return v;
  } else {
    return f(lst[idx], fold(f, v, lst, len, idx + 1));
  }
}

function add(a, b) {
  return a + b;
}

var lst = [0, 1, 2, 3, 4, 5];
var len = 6;

println(fold(add, 0, lst, len, 0));
```

### mapの例(少し高度な高階関数の例)

```js
function map(f, lst, len, idx) {
  if (len == idx) {
    return lst;
  } else {
    lst[idx] = f(lst[idx]);
    return map(f, lst, len, idx + 1);
  }
}

function square(x) {
  return x * x;
}

var lst = [0, 1, 2, 3, 4, 5];
var len = 6;

println(map(square, lst, len, 0));
```

## インストール方法

### Prerequirements

事前に最新バージョンのいかのプログラムをインストールしてください．

- Dコンパイラ: DMD or LDC2
- ビルドツール兼パッケージマネージャ: dub

### Build

```zsh
$ git clone https://github.com/alphaKAI/tvm
$ cd tvm
$ dub build --build=release --compiler=ldc or --compiler=dmd #コンパイラを指定する
```

## 使い方

- ./tvm -r でREPLが起動(ただし，複数行を書くことができないので，ワンライナーで記述する必要がある．)
- ./tvm で Hello, worldが出力される
- ./tvm hoge.toy や hoge.tvm でファイルを実行する
- ./tvm -c hoge.tvm でファイルを事前コンパイルする
- ./tvm -e hoge.compiled で事前コンパイルされたファイルを実行できる．

## 免責事項
TVMは"正しく記述されたコードを正しく実行する"事ができます．つまり，エラー報告が適当です．
パースエラーが発生してもわかりにくいエラーしか出ません．これは今後修正したいと考えていますが，現時点ではそういう仕様です．

## ライセンス
TVMはMITライセンスのもとで公開しています．
Copyright (C) 2018-2019, Akihiro Shoji