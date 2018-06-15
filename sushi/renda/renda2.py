import subprocess

misstypes = "- - - - - - - - - - - - - - - -".split(" ")

for i in misstypes:
    subprocess.Popen(["python", "renda.py", i])
