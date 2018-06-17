# 文字認識でオート寿司打

[寿司打](http://neutral.x0.com/home/sushida/play2.html)を文字認識を利用して自動的に行う．

使用するライブラリやコードの詳細については[doc/autosushi.md](doc/autosushi.md)を参照．

## 準備

### Tesseract，PyOCR，PyAutoGUIのインストール

MacでHomebrewとAnacondaを使っていることを前提とする．

```
brew install tesseract
```

```
pip install pyocr
```

```
conda install -c conda-forge pyautogui
```

### 醤油の画像の用意

![doc/murasaki.png](doc/murasaki.png)

[寿司打](http://neutral.x0.com/home/sushida/play2.html)を一度スタートし，[上記の画像](doc/murasaki.png)を参考に醤油の画像のスクリーンショットを撮影する．

`src` とう名前のフォルダを作り，撮影した自身の醤油の画像をその中に `murasaki.png` という名前で保存する．

## 実行

寿司打を開始したあと， `auto.py` を実行し，その後寿司打の画面をクリックする．
