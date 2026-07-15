#!/usr/bin/env python3
import re
import sys
import glob
from pathlib import Path

def main():
    has_errors = False

    # 1. Extract environment variables and fallbacks from Haskell source files
    haskell_vars = {} # EnvVar -> Fallback
    for hs_file in glob.glob("**/*.hs", recursive=True):
        if ".stack-work" in hs_file or "dist-newstyle" in hs_file:
            continue
        try:
            with open(hs_file, 'r', encoding='utf-8') as f:
                content = f.read()
            matches = re.findall(r'fromMaybe\s+"([^"]+)"\s+<\$>\s+lookupEnv\s+"([^"]+)"', content)
            for fallback, var_name in matches:
                haskell_vars[var_name] = fallback
        except Exception:
            pass

    # 2. Parse docker-compose.yml manually
    compose_env_dict = {}
    compose_devices = []
    try:
        with open("docker-compose.yml", "r") as f:
            lines = f.readlines()
            in_env = False
            in_devices = False
            for line in lines:
                stripped = line.strip()
                if not stripped:
                    continue
                if stripped.startswith("environment:"):
                    in_env = True
                    in_devices = False
                    continue
                elif stripped.startswith("devices:"):
                    in_devices = True
                    in_env = False
                    continue
                elif not stripped.startswith("-"):
                    if in_env or in_devices:
                        # end of block
                        in_env = False
                        in_devices = False
                
                if in_env and stripped.startswith("-"):
                    env_entry = stripped.lstrip("- ").strip()
                    if "=" in env_entry:
                        k, v = env_entry.split("=", 1)
                        compose_env_dict[k.strip()] = v.strip()
                elif in_devices and stripped.startswith("-"):
                    dev_entry = stripped.lstrip("- ").strip()
                    compose_devices.append(dev_entry)
    except Exception as e:
        print(f"Error reading docker-compose.yml: {e}")
        sys.exit(1)

    # 3. Check serial port configurations in docker-compose.yml against Haskell fallbacks
    for port_var in ['SGRT_SENSOR_PORT', 'SGRT_CLI_PORT']:
        if port_var in haskell_vars and port_var in compose_env_dict:
            if compose_env_dict[port_var] != haskell_vars[port_var]:
                print(f"ERROR: {port_var} in docker-compose.yml ({compose_env_dict[port_var]}) mismatches application fallback ({haskell_vars[port_var]}).")
                has_errors = True
        elif port_var in haskell_vars and port_var not in compose_env_dict:
            print(f"ERROR: {port_var} is missing from docker-compose.yml environment.")
            has_errors = True

    # 4. Align Host-Container Pathways in docker-compose.yml devices
    for device in compose_devices:
        if ':' in device:
            host, container = device.split(':', 1)
            if host != container:
                print(f"ERROR: Mismatched host and container paths in docker-compose.yml devices: {device}")
                has_errors = True

    # 5. Verify Environment Completeness in templates
    template_files = list(Path('.').glob('.env.*'))
    if not template_files:
        print("ERROR: No environment templates (.env.example etc) found.")
        has_errors = True
    else:
        for t_file in template_files:
            if t_file.name == '.env.example' or t_file.name == '.env.template':
                with open(t_file, 'r') as f:
                    content = f.read()
                
                template_vars = {}
                for line in content.splitlines():
                    line = line.strip()
                    if line and not line.startswith('#'):
                        if '=' in line:
                            k, v = line.split('=', 1)
                            template_vars[k.strip()] = v.strip()
                
                # Check for missing variables
                for hs_var in haskell_vars.keys():
                    if hs_var not in template_vars:
                        print(f"ERROR: Configurable system environment parameter '{hs_var}' is completely omitted from {t_file.name}.")
                        has_errors = True

    if has_errors:
        sys.exit(1)
    else:
        print("Validation successful!")
        sys.exit(0)

if __name__ == '__main__':
    main()
