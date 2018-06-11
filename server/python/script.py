#!/usr/local/bin/python3
# -*- coding: utf-8 -*-

import time
from datetime import datetime

while(True):
    s = datetime.now().strftime('%y%m%d%H%M%S')
    with open("./src/sample.html", "w") as file:
        file.write(s)
    time.sleep(10)
