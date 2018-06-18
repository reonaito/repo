import numpy as np
import time
import gi
gi.require_version("Gdk", "3.0")
from gi.repository import Gdk
import pyautogui
from PIL import Image
import pyocr

tools = pyocr.get_available_tools()
tool = tools[0]

win = Gdk.get_default_root_window()

x, y, w, h = pyautogui.locateOnScreen('src/murasaki.png', grayscale=True)
regx, regy, regw, regh = x, y - int(h*1.95), int(w*4.5), int(h*0.65)

time.sleep(0.5)

pyautogui.press("escape")
pyautogui.press("space")

while(True):
    pix = Gdk.pixbuf_get_from_window(win, regx, regy, regw, regh)

    data = pix.get_pixels()
    w = pix.props.width
    h = pix.props.height
    stride = pix.props.rowstride
    mode = "RGB"
    img = Image.frombytes(mode, (w, h), data, "raw", mode, stride)

    txt = tool.image_to_string(
        img,
        lang="eng",
        builder=pyocr.builders.TextBuilder()
    )
    types = txt.split(" ")
    typing = types[np.argmax(list(map(lambda x: len(x), types)))]
    print(typing)
    pyautogui.typewrite(typing)
    time.sleep(0.4)

