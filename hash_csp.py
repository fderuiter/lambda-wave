import re
import hashlib
import base64

with open('app/Control/WebUI/assets/index.html', 'rb') as f:
    content = f.read()

styles = re.findall(b'<style>(.*?)</style>', content, re.DOTALL)
for s in styles:
    h = hashlib.sha256(s).digest()
    print("style:", base64.b64encode(h).decode())

scripts = re.findall(b'<script>(.*?)</script>', content, re.DOTALL)
for s in scripts:
    h = hashlib.sha256(s).digest()
    print("script:", base64.b64encode(h).decode())
