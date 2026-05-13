import hashlib
import base64
import re

# Get new hashes
with open('app/Control/WebUI/assets/index.html', 'rb') as f:
    content = f.read()

style_match = re.search(b'<style>(.*?)</style>', content, re.DOTALL)
style_hash = base64.b64encode(hashlib.sha256(style_match.group(1)).digest()).decode('utf-8')

script_match = re.search(b'<script>(.*?)</script>', content, re.DOTALL)
script_hash = base64.b64encode(hashlib.sha256(script_match.group(1)).digest()).decode('utf-8')

# Update WebUI.hs
with open('app/Control/WebUI.hs', 'r') as f:
    webui = f.read()

new_csp = f"default-src 'self'; connect-src 'self' ws: wss:; script-src 'self' 'sha256-{script_hash}'; style-src 'self' 'sha256-{style_hash}'"

# Use regex to replace the CSP string
webui_updated = re.sub(
    r'\(\"Content-Security-Policy\", \".*?\"\)',
    f'("Content-Security-Policy", "{new_csp}")',
    webui
)

with open('app/Control/WebUI.hs', 'w') as f:
    f.write(webui_updated)

print(f"Updated CSP with hashes: script={script_hash}, style={style_hash}")
