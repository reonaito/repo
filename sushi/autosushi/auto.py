import numpy as np
import time
import gi
gi.require_version("Gdk", "3.0")
from gi.repository import Gdk
import pyautogui
from PIL import Image
import pyocr

def cut_with_frame(img):
    try:
        # PIL.Image.Image to np.array
        pix = np.array(img)

        # extract the white frame by looking at the pix.red of the top row
        row_top = pix[0, :, 0]
        thresholds = np.where(row_top > 240)[0]
        th_left, th_right = thresholds[0], thresholds[-1]

        # convert the img
        cut_pix = pix[:, th_left:th_right, :]
        cut_img = Image.fromarray(cut_pix)

        return cut_img

    except:
        return img

tools = pyocr.get_available_tools()
tool = tools[0]

win = Gdk.get_default_root_window()

x, y, w, h = pyautogui.locateOnScreen('src/murasaki.png', grayscale=True)
regx, regy, regw, regh = x, int(y-h*1.8), int(w*4.5), int(h*0.5)

time.sleep(0.5)

pyautogui.press("escape")
pyautogui.press("space")

time.sleep(2.5)

index = 0

while(True):
    pix = Gdk.pixbuf_get_from_window(win, regx, regy, regw, regh)

    data = pix.get_pixels()
    w = pix.props.width
    h = pix.props.height
    stride = pix.props.rowstride
    mode = "RGB"
    img = Image.frombytes(mode, (w, h), data, "raw", mode, stride)
    cut_img = cut_with_frame(img)

    txt = tool.image_to_string(
        cut_img,
        builder=pyocr.builders.TextBuilder(tesseract_layout=8)
    )
    types = txt.split(" ")
    typing = types[np.argmax(list(map(lambda x: len(x), types)))]
    if len(typing) == 0:
        typing = "-"
    print(typing)

    # saving
    cut_img.save("sample/cut_" + str(index) + ".png")
    img.save("sample/original_" + str(index) + ".png")
    with open("sample/typing_" + str(index) + ".txt", mode='w') as f:
        f.write(txt)
    index = index + 1

    pyautogui.typewrite(typing)
    time.sleep(0.4)

