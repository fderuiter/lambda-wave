import hashlib
import base64
import re

with open('app/Control/WebUI/assets/index.html', 'rb') as f:
    content = f.read()

# Extract <style>
style_match = re.search(b'<style>(.*?)</style>', content, re.DOTALL)
if style_match:
    style_content = style_match.group(1)
    style_hash = base64.b64encode(hashlib.sha256(style_content).digest()).decode('utf-8')
    print(f"style-src: 'sha256-{style_hash}'")

# Extract <script>
script_match = re.search(b'<script>(.*?)</script>', content, re.DOTALL)
if script_match:
    script_content = script_match.group(1)
    script_hash = base64.b64encode(hashlib.sha256(script_content).digest()).decode('utf-8')
    print(f"script-src: 'sha256-{script_hash}'")
