import sys
import os

def main():
    if len(sys.argv) < 2:
        print("Usage: path_selector.py <modified_files.txt>")
        sys.exit(1)

    modified_files_path = sys.argv[1]
    if not os.path.exists(modified_files_path):
        print(f"Error: {modified_files_path} not found.")
        sys.exit(1)

    with open(modified_files_path, 'r') as f:
        files = [line.strip() for line in f if line.strip()]

    prototype_only = True
    for f in files:
        if not f.startswith("tools/"):
            prototype_only = False
            break

    # Determine GHC options
    ghc_options = "-Wwarn" if prototype_only else "-Werror"
    
    # Configure cabal.project.local
    cabal_local = "cabal.project.local"
    # Read existing content if exists
    content = ""
    if os.path.exists(cabal_local):
        with open(cabal_local, 'r') as f:
            content = f.read()
    
    # Append or create ghc-options
    with open(cabal_local, 'a') as f:
        f.write(f"\npackage sgrt-radar-system\n  ghc-options: {ghc_options}\n")

    print(f"Prototype only run: {prototype_only}")
    print(f"Appended ghc-options: {ghc_options} to {cabal_local}")

    # Set GitHub Actions output
    github_output = os.environ.get('GITHUB_OUTPUT')
    if github_output:
        with open(github_output, 'a') as f:
            f.write(f"prototype_only={'true' if prototype_only else 'false'}\n")

if __name__ == "__main__":
    main()
