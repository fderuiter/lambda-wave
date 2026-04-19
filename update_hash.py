import hashlib
import base64
import re

with open('app/Control/WebUI/assets/index.html', 'rb') as f:
    content = f.read()

style_match = re.search(b'<style>(.*?)</style>', content, flags=re.DOTALL)
if style_match:
    style_content = style_match.group(1)
    sha = base64.b64encode(hashlib.sha256(style_content).digest()).decode('utf-8')
    print(f"Style hash: 'sha256-{sha}'")
else:
    print("Style tag not found")

script_match = re.search(b'<script>(.*?)</script>', content, flags=re.DOTALL)
if script_match:
    script_content = script_match.group(1)
    sha = base64.b64encode(hashlib.sha256(script_content).digest()).decode('utf-8')
    print(f"Script hash: 'sha256-{sha}'")
else:
    print("Script tag not found")
