# 画像認識でオート寿司打に挑戦する

## TesseractとPyOCRのインストール

[Tesseract OCR](https://github.com/tesseract-ocr/tesseract)はGoogleによって開発されたオープンソースのOCRエンジンである．

Macでは以下のコマンドでインストールできる．

```
brew install tesseract
```

コマンドラインでは以下のようなコマンドで文字認識ができる．

```
tesseract input.png out
```

[PyOCR](https://gitlab.gnome.org/World/OpenPaperwork/pyocr)は，Pythonで文字認識を行うためのラッパーツールである．

インストールはAnacondaの仮想環境の中で以下のコマンドを実行する．

```
pip install pyocr
```

## OpenCVのインストール

OpenCVは画像処理などを行うことができるライブラリである．

Pythonで使用するためには，Anacondaの仮想環境の中で以下のコマンドを実行．

```
conda install -c conda-forge opencv
```

## PyAutoGUIのインストール

[PyAutoGUI](https://github.com/asweigart/pyautogui)は，キーボードやマウスの操作，画面キャプチャなどが可能なライブラリである．
インストールはAnacondaの仮想環境内で以下のコマンド．

```
conda install -c conda-forge pyautogui
```

## 文字認識のテスト

```Python
import cv2
from PIL import Image
import pyocr

tools = pyocr.get_available_tools()
tool = tools[0]

img = cv2.imread('./sample/ss1.png')

txt = tool.image_to_string(
    Image.fromarray(img),
    lang="eng",
    builder=pyocr.builders.TextBuilder()
)

print(txt)
```

`sample/ss1.png` を文字認識してその結果を出力します．

## 寿司打のローマ字表示認識に挑戦

まずは寿司打のローマ字表示を正しく認識できるかどうかテストしてみる．

```python
x, y, _, _ = pyautogui.locateOnScreen('src/murasaki.png', grayscale=True)
img = pyautogui.screenshot(region=(x, y - 88, 450, 28))
txt = tool.image_to_string(
    img,
    lang="eng",
    builder=pyocr.builders.TextBuilder()
)
```

寿司打の画面の醤油の位置を基準にして，ローマ字表示される部分を横長に取得し，画像解析にかける．

![img1](autosushi_1.png)

解析結果： `puroguramlngugengo H`

問題は二つある．

- 文字認識の精度が低いこと
- 枠やお茶などを文字と誤認する可能性があること

## 応急処置

文字認識の精度に関しては，画面を拡大したりディスプレイの設定を変えたりして，寿司打の画面の解像度を上げることで改善が可能である．

枠やお茶などを文字と誤認する問題に関しては，打つべきローマ字と枠の間に広めの隙間があることから， `I amerikarenpousousakyoku R` のように，誤認した文字と打つべきローマ字の間にスペースが入る．

このことを利用して，Pythonの文字列やリストの処理を駆使して打つべきローマ字だけを取り出せば良い．

```Python
types = txt.split(" ")
typing = types[np.argmax(list(map(lambda x: len(x), types)))]
pyautogui.typewrite(typing)
```

これで文字認識を使って正確にタイピングすることはできるようになった．

## 実践

```Python
import numpy as np
import time
import cv2
import pyautogui
from PIL import Image
import pyocr

tools = pyocr.get_available_tools()
tool = tools[0]

x, y, w, h = pyautogui.locateOnScreen('src/murasaki2.png', grayscale=True)
regx, regy, regw, regh = x, y - int(h*1.95), int(w*4.5), int(h*0.65)

while(True):
    img = pyautogui.screenshot(region=(regx, regy, regw, regh))
    txt = tool.image_to_string(
        img,
        lang="eng",
        builder=pyocr.builders.TextBuilder()
    )
    types = txt.split(" ")
    typing = types[np.argmax(list(map(lambda x: len(x), types)))]
    print(typing)
    pyautogui.typewrite(typing)
    time.sleep(0.1)
```

![img2](autosushi_2.png)

一応機能はしているが，はっきり言って人力より遅い．
文字認識の遅さと正確性の低さが問題である．
