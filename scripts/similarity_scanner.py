import sys
import os
import difflib

def get_all_files(extensions, exclude_files):
    all_files = []
    for root, _, files in os.walk('.'):
        if '.git' in root or 'dist-newstyle' in root:
            continue
        for f in files:
            path = os.path.normpath(os.path.join(root, f))
            if any(path.endswith(ext) for ext in extensions) and path not in exclude_files:
                all_files.append(path)
    return all_files

def extract_tokens(text):
    return set(text.split())

def main():
    if len(sys.argv) < 2:
        print("Usage: similarity_scanner.py <modified_files.txt>")
        sys.exit(1)

    modified_files_path = sys.argv[1]
    if not os.path.exists(modified_files_path):
        print(f"Error: {modified_files_path} not found.")
        sys.exit(1)

    with open(modified_files_path, 'r') as f:
        modified_files = [line.strip() for line in f if line.strip()]

    core_extensions = {'.hs', '.c', '.cpp', '.h'}
    core_dirs = ('src/', 'src-math/', 'cbits/')
    
    modified_core_files = [
        f for f in modified_files 
        if any(f.startswith(d) for d in core_dirs) and any(f.endswith(ext) for ext in core_extensions) and os.path.exists(f)
    ]

    if not modified_core_files:
        print("No modified core files to scan.")
        sys.exit(0)

    # Pre-load existing files to avoid re-walking
    existing_files = get_all_files(core_extensions, set(modified_core_files))
    existing_file_data = {}
    for f in existing_files:
        try:
            with open(f, 'r', encoding='utf-8') as file:
                content = file.read()
                existing_file_data[f] = {
                    'content': content,
                    'tokens': extract_tokens(content),
                    'len': len(content)
                }
        except Exception as e:
            pass # ignore unreadable files

    found_duplicate = False

    for mod_file in modified_core_files:
        ext = os.path.splitext(mod_file)[1]
        try:
            with open(mod_file, 'r', encoding='utf-8') as file:
                mod_content = file.read()
        except Exception:
            continue
            
        mod_tokens = extract_tokens(mod_content)
        mod_len = len(mod_content)
        
        # We only need to compare files if they have meaningful content
        if mod_len < 20: 
            continue

        for existing_file, data in existing_file_data.items():
            if not existing_file.endswith(ext):
                continue
                
            existing_content = data['content']
            existing_tokens = data['tokens']
            existing_len = data['len']
            
            # Fast filter 1: size difference
            size_ratio = min(mod_len, existing_len) / max(mod_len, existing_len) if max(mod_len, existing_len) > 0 else 0
            if size_ratio < 0.6: # If sizes are vastly different, similarity will be low
                continue
                
            # Fast filter 2: Token overlap heuristic (Jaccard similarity approximation)
            intersection = len(mod_tokens.intersection(existing_tokens))
            union = len(mod_tokens.union(existing_tokens))
            token_ratio = intersection / union if union > 0 else 0
            
            if token_ratio < 0.4: # Only do expensive difflib if token overlap is decent
                continue

            # Full SequenceMatcher
            matcher = difflib.SequenceMatcher(None, mod_content, existing_content)
            ratio = matcher.ratio()

            if ratio >= 0.8:
                print(f"ERROR: Duplicate logic detected!")
                print(f"File '{mod_file}' matches '{existing_file}' with similarity ratio {ratio:.2f} (>= 0.80)")
                found_duplicate = True

    if found_duplicate:
        print("Build failed due to duplicate utility logic.")
        sys.exit(1)
        
    print("Similarity scan passed. No duplicates found.")
    sys.exit(0)

if __name__ == "__main__":
    main()
