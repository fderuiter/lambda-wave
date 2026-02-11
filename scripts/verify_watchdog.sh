#!/bin/bash
set -e

# Compile (use -threaded just in case, though for single core it matters less)
ghc -threaded -isrc test/Safety/WatchdogFault.hs -o test/watchdog_fault

# Run and capture output
# We expect failure (exit code 1), so we temporarily disable set -e
set +e
./test/watchdog_fault > test_output.log 2>&1
EXIT_CODE=$?
set -e

# Analyze
if grep -q "WATCHDOG TRIP" test_output.log; then
    echo "PASS: Watchdog log found."
else
    echo "FAIL: Watchdog log NOT found."
    echo "Output was:"
    cat test_output.log
    exit 1
fi

if [ $EXIT_CODE -ne 0 ]; then
    echo "PASS: Process exited with error (Code: $EXIT_CODE)."
else
    echo "FAIL: Process exited successfully (should have been killed)."
    exit 1
fi

if grep -q "SURVIVED" test_output.log; then
    echo "FAIL: Process survived the timeout."
    exit 1
fi

echo "VERIFICATION SUCCESSFUL"
rm -f test/watchdog_fault test_output.log test/Safety/WatchdogFault.hi test/Safety/WatchdogFault.o
