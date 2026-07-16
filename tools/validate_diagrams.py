import re
import os
import sys

def validate_diagrams(repair=False):
    error_found = False
    
    pattern = re.compile(r'([\w_-]+)\s*([\[\{\(]+)\s*([^"\]\}\)]*[<>][^"\]\}\)]*)\s*([\]\}\)]+)')
    
    for root, dirs, files in os.walk("."):
        dirs[:] = [d for d in dirs if not d.startswith('.')]
        for file in files:
            if file.endswith(".md"):
                file_path = os.path.join(root, file)
                with open(file_path, "r", encoding="utf-8") as f:
                    text = f.read()
                
                original_text = text
                
                # Find mermaid blocks
                # We need to correctly parse the block boundaries.
                mermaid_blocks = re.findall(r'(```mermaid\n.*?\n```)', text, re.DOTALL)
                for block in mermaid_blocks:
                    new_block = block
                    lines = block.split('\n')
                    for line in lines:
                        matches = pattern.finditer(line)
                        for m in matches:
                            full_match = m.group(0)
                            node_id = m.group(1)
                            open_b = m.group(2)
                            content = m.group(3)
                            close_b = m.group(4)
                            
                            # It could be that content is already quoted but regex shouldn't match it 
                            # because of [^"] logic. Let's double check.
                            
                            if not repair:
                                print(f"Error: Unquoted comparison operator found in {file_path}")
                                print(f"Line: {line.strip()}")
                                error_found = True
                            else:
                                replacement = f'{node_id}{open_b}"{content}"{close_b}'
                                new_block = new_block.replace(full_match, replacement)
                                
                    if repair and new_block != block:
                        text = text.replace(block, new_block)
                
                if repair and text != original_text:
                    with open(file_path, "w", encoding="utf-8") as f:
                        f.write(text)
                    print(f"Repaired diagrams in {file_path}")

    if error_found:
        sys.exit(1)
    else:
        if not repair:
            print("All diagrams validated successfully.")

if __name__ == "__main__":
    repair_mode = len(sys.argv) > 1 and sys.argv[1] == "--repair"
    validate_diagrams(repair=repair_mode)
