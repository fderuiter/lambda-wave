#!/usr/bin/env python3
import os
import sys

def create_module_and_test(module_path):
    # module_path might be "Subsystem.ModuleName" or "Subsystem/ModuleName"
    # We want to support dotted notation or path notation.
    module_name = module_path.replace('/', '.')
    
    parts = module_name.split('.')
    if len(parts) < 1:
        print("Invalid module name.")
        sys.exit(1)
        
    src_dir = os.path.join("src", *parts[:-1])
    test_dir = os.path.join("test", *parts[:-1])
    
    os.makedirs(src_dir, exist_ok=True)
    os.makedirs(test_dir, exist_ok=True)
    
    src_file = os.path.join("src", *parts) + ".hs"
    test_file = os.path.join("test", *parts) + "Spec.hs"
    
    if os.path.exists(src_file):
        print(f"Error: {src_file} already exists.")
        sys.exit(1)
        
    if os.path.exists(test_file):
        print(f"Error: {test_file} already exists.")
        sys.exit(1)
        
    with open(src_file, "w") as f:
        f.write(f"module {module_name} (\n")
        f.write(f"    -- exports\n")
        f.write(f") where\n\n")
        f.write(f"-- Core logic for {module_name}\n")

    with open(test_file, "w") as f:
        f.write("{-# LANGUAGE OverloadedStrings #-}\n")
        f.write(f"module {module_name}Spec (spec) where\n\n")
        f.write(f"import Test.Hspec\n")
        f.write(f"import {module_name}\n\n")
        f.write(f"spec :: Spec\n")
        f.write(f"spec = do\n")
        f.write(f"    describe \"{module_name}\" $ do\n")
        f.write(f"        it \"has a placeholder test\" $ do\n")
        f.write(f"            True `shouldBe` True\n")
        
    print(f"Scaffolded source: {src_file}")
    print(f"Scaffolded test:   {test_file}")

if __name__ == "__main__":
    if len(sys.argv) != 2:
        print("Usage: scaffold_tdd_module.py <ModuleName>")
        print("Example: scaffold_tdd_module.py Subsystem.MyModule")
        sys.exit(1)
        
    create_module_and_test(sys.argv[1])
