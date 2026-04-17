import re
with open("sgrt-radar-system.cabal", "r") as f:
    content = f.read()

content = re.sub(r'      Control\.UIRendererSpec\n', '', content)
content = re.sub(r'      Control\.RendererSpec\n', '', content)

with open("sgrt-radar-system.cabal", "w") as f:
    f.write(content)
