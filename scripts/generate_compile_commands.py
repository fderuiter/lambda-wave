#!/usr/bin/env python3
import re
import json
import os
import sys

def main():
    cabal_file = "sgrt-radar-system.cabal"
    if not os.path.exists(cabal_file):
        print(f"Error: {cabal_file} not found.")
        sys.exit(1)

    with open(cabal_file) as f:
        content = f.read()

    sources = []
    in_cxx = False
    for line in content.splitlines():
        strip_line = line.strip()
        if strip_line.startswith("cxx-sources:"):
            in_cxx = True
            continue
        if in_cxx:
            if strip_line == "" or not line.startswith(" "):
                in_cxx = False
            elif strip_line.endswith(".cpp") or strip_line.endswith(".c"):
                sources.append(strip_line)
            else:
                pass # skip cc-options etc.

    m = re.search(r"cxx-options:\s*(.*)", content)
    cxx_options = m.group(1).strip() if m else ""

    # Provide includes for dependencies
    cxx_options += " -I cbits/include"

    commands = []
    for src in sources:
        commands.append({
            "directory": os.getcwd(),
            "command": f"clang++ {cxx_options} -c {src}",
            "file": src
        })

    with open("compile_commands.json", "w") as f:
        json.dump(commands, f, indent=2)
    print("Generated compile_commands.json")

if __name__ == "__main__":
    main()
