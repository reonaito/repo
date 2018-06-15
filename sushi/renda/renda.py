import sys
import pyautogui as pgui

misstype = sys.argv[1] * 4000
pgui.typewrite(misstype)
