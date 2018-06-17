import numpy as np
import time
import pyautogui
from PIL import Image
import pyocr

tools = pyocr.get_available_tools()
tool = tools[0]

x, y, w, h = pyautogui.locateOnScreen('src/murasaki.png', grayscale=True)
regx, regy, regw, regh = x, y - int(h*1.95), int(w*4.5), int(h*0.65)

while(True):
    img = pyautogui.screenshot(region=(regx, regy, regw, regh))
    # img.save("sample/sample.png")
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
