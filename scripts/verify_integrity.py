import json
import os
import subprocess
import sys
import urllib.request
import hashlib

def verify_integrity():
    plan_path = "dist-newstyle/cache/plan.json"
    if not os.path.exists(plan_path):
        print("plan.json not found. Run 'cabal build --dry-run' first.")
        sys.exit(1)

    with open(plan_path, "r") as f:
        plan = json.load(f)

    print("Verifying package integrity from plan.json...")
    mismatches = 0
    verified = 0

    for install_plan in plan.get("install-plan", []):
        if install_plan.get("type") == "pre-existing":
            continue

        pkg_name = install_plan.get("pkg-name")
        pkg_version = install_plan.get("pkg-version")
        pkg_src = install_plan.get("pkg-src")
        
        if not pkg_name or not pkg_version or not pkg_src:
            continue

        if pkg_src.get("type") == "repo-tar":
            expected_hash = install_plan.get("pkg-src-sha256")
            if not expected_hash:
                print(f"[{pkg_name}-{pkg_version}] Warning: No hash found in plan")
                continue

            # In some plan.json, the repo is specified
            repo_name = pkg_src.get("repo", {}).get("uri", "https://hackage.haskell.org/")
            url = f"https://hackage.haskell.org/package/{pkg_name}-{pkg_version}/{pkg_name}-{pkg_version}.tar.gz"
            
            print(f"[{pkg_name}-{pkg_version}] Verifying against {url} ... ", end="")
            sys.stdout.flush()
            
            try:
                req = urllib.request.Request(url, headers={'User-Agent': 'Mozilla/5.0'})
                with urllib.request.urlopen(req) as response:
                    data = response.read()
                    actual_hash = hashlib.sha256(data).hexdigest()
                    
                    if actual_hash == expected_hash:
                        print("OK")
                        verified += 1
                    else:
                        print("FAIL")
                        print(f"  Expected: {expected_hash}")
                        print(f"  Actual:   {actual_hash}")
                        mismatches += 1
            except Exception as e:
                print(f"ERROR: {e}")
                mismatches += 1

    print(f"\nVerification complete. {verified} packages verified successfully.")
    if mismatches > 0:
        print(f"Found {mismatches} integrity mismatches!")
        sys.exit(1)

if __name__ == "__main__":
    verify_integrity()
