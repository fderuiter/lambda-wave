import os
import re

def get_freeze_deps():
    deps = {}
    with open("cabal.project.freeze", "r") as f:
        for line in f:
            m = re.search(r'any\.([a-zA-Z0-9\-]+)\s*==\s*([0-9\.]+)', line)
            if m:
                deps[m.group(1)] = m.group(2)
    return deps

def update_cabal_file(deps):
    with open("sgrt-radar-system.cabal", "r") as f:
        content = f.read()

    # Find build-depends blocks and replace package references
    # This is a bit tricky with regex, we can match lines inside build-depends
    
    lines = content.split('\n')
    in_build_depends = False
    new_lines = []
    
    for line in lines:
        stripped = line.strip()
        if stripped.startswith("build-depends:"):
            in_build_depends = True
            new_lines.append(line)
            continue
            
        if in_build_depends:
            # If line starts with a non-whitespace or is empty and not part of list, it might be end of block
            if line and not line.startswith(" ") and not line.startswith("\t"):
                in_build_depends = False
            elif stripped == "" or stripped.startswith("--"):
                pass
            else:
                # We are in build-depends. 
                # Lines usually look like: "base >=4.7 && <5", ", stm", "  , binary >= 0.8"
                # Let's extract the package name.
                m = re.match(r'^(\s*,?\s*)([a-zA-Z0-9\-]+)(.*)$', line)
                if m:
                    prefix = m.group(1)
                    pkg = m.group(2)
                    rest = m.group(3)
                    
                    if pkg in deps:
                        exact_version = f" =={deps[pkg]}"
                        # Replace the rest with the exact version
                        line = f"{prefix}{pkg}{exact_version}"
                    elif pkg == "sgrt-radar-system":
                        # internal package
                        pass
        
        new_lines.append(line)

    with open("sgrt-radar-system.cabal", "w") as f:
        f.write("\n".join(new_lines))

if __name__ == "__main__":
    deps = get_freeze_deps()
    update_cabal_file(deps)
    print("Enforced exact version bounds in sgrt-radar-system.cabal")
