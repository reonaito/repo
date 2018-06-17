# 文字認識でオート寿司打に挑戦する

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

x, y, w, h = pyautogui.locateOnScreen('src/murasaki.png', grayscale=True)
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

処理速度に関して具体的に言うと，スクリーンショットの取得と文字認識にそれぞれ0.5秒ほどかかっている．

スクリーンショットに関して言うと，PyAutoGUIのスクリーンショットは，一度ファイルに保存してそれを読み込んでいる模様．

## 試行錯誤

スクリーンショット取得の遅さを解消するため，他のスクリーンショットの取得法を試す．

---

[PyGObject](https://pygobject.readthedocs.io/en/latest/index.html)

```
brew install gtk+3
brew install gobject-introspection
```

```
Package libffi was not found in the pkg-config search path.
Perhaps you should add the directory containing `libffi.pc'
to the PKG_CONFIG_PATH environment variable
Package 'libffi', required by 'gobject-introspection-1.0', not found
```

```
export PKG_CONFIG_PATH=`brew --prefix libffi`/lib/pkgconfig
```

```
pip install pygobject
```

```Python
import gi
gi.require_version("Gdk", "3.0")
from gi.repository import Gdk
from gi.repository import GdkPixbuf

win = Gdk.get_default_root_window()
pixbuf = Gdk.pixbuf_get_from_window(win, 0, 0, 200, 150)
pixbuf.savev("screenshot.png","png", (), ())
```

これはスクリーンショットを取得することができなかった．

---

[Pillow](https://pillow.readthedocs.io/en/5.1.x/)

```
pip install pyscreenshot
pip install pillow
```

```
from PIL import ImageGrab
ImageGrab.grab().save("picture.png")
```

これはPyAutoGUIと変わらない．というより，PyAutoGUIのスクリーンショット機能が内部でPillowを呼び出しているようだ．

---

[Selenium](http://selenium-python.readthedocs.io/)

```Python
from selenium import webdriver
from selenium.webdriver.chrome.options import Options

chrome_options = Options()
chrome_options.add_argument("window-size=1200,1000")

driver = webdriver.Chrome(chrome_options=chrome_options)

URL = 'http://neutral.x0.com/home/sushida/play2.html'
driver.get(URL);

driver.save_screenshot("picture.png")
```

スクレイピングでも用いたSeleniumを使ってみる．

結果は，スクリーンショットを取るのにかかる時間はPyAutoGUIとそれほど変わらなかった．

## Ubuntu仮想マシンの導入

PyGObjectを使用するため，[VirtualBox](https://www.virtualbox.org/)を用いてUbuntuのGUI版の仮想マシンを導入する．

Ubuntuはaptを用いることでいろいろなパッケージを容易にインストールできる上，VirtualBoxのスナップショット機能を使えば簡単に環境を巻き戻すことができるので，Anacondaは使わずに必要なパッケージはaptで導入する．

### PyGObjectのインストール

UbuntuにPyGObjectをインストールするのは以下のコマンドでできる．

```
sudo apt install python-gi python-gi-cairo python3-gi python3-gi-cairo gir1.2-gtk-3.0
```

スクリーンショットの所要時間を測定してみる．

```Python
import gi
gi.require_version("Gdk", "3.0")
from gi.repository import Gdk
import time

start = time.time()
win = Gdk.get_default_root_window()
pixbuf = Gdk.pixbuf_get_from_window(win, *win.get_geometry())
print(time.time() - start)

start = time.time()
pixbuf.savev("screenshot.png","png", (), ())
print(time.time() - start)
```

スクリーンショットを取得するのにかかった時間: 0.048秒  
ファイルに保存するのにかかった時間: 0.42秒

ファイルを介さなければ早い．

### PyAutoGUIのインストール

```
sudo apt install python3-pip
pip3 install python3-xlib
sudo apt-get install scrot
sudo apt-get install python3-tk
sudo apt-get install python3-dev
pip3 install pyautogui
```

### Tesseract，PyOCRのインストール

```
sudo apt install python3-pyocr
```

Tesseractは依存パッケージとして自動的にインストールされる．

## PyGObjectを用いた改善

GdkPixbufからPILイメージへの変換は以下の通り．

```Python
import gi
gi.require_version(Gdk", "3.0")
from gi.repository import Gdk
from PIL import Image
import time

start = time.time()
win = Gdk.get_default_root_window()
pix = Gdk.pixbuf_get_from_window(win, *win.get_geometry())

data = pix.get_pixels()
w = pix.props.width
h = pix.props.height
stride = pix.props.rowstride
mode = "RGB"
im = Image.frombytes(mode, (w, h), data, "raw", mode, stride)
print(time.time() - start)
```
