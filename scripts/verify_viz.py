import subprocess
import time
import os
import sys
from playwright.sync_api import sync_playwright, expect

def verify_viz():
    # 1. Start Server
    env = os.environ.copy()
    env["SGRT_SIMULATION"] = "1"

    print("Starting Server in Simulation Mode...")

    # Run unbuffered to see output?
    server_process = subprocess.Popen(
        ["cabal", "run", "sgrt-radar-system-exe"],
        env=env,
        stdout=subprocess.PIPE,
        stderr=subprocess.PIPE,
        text=True,
        bufsize=1
    )

    try:
        # Wait for server to be ready
        # We can read stdout in a non-blocking way or just sleep.
        # Sleeping is safer/simpler for this script.
        print("Waiting 15 seconds for server startup...")
        time.sleep(15)

        with sync_playwright() as p:
            browser = p.chromium.launch()
            page = browser.new_page()

            print("Navigating to UI...")
            try:
                page.goto("http://localhost:8080")
            except Exception as e:
                print(f"Failed to connect: {e}")
                # Dump server logs
                server_process.terminate()
                outs, errs = server_process.communicate()
                print("Server Stdout:\n", outs)
                print("Server Stderr:\n", errs)
                sys.exit(1)

            print("Checking page title...")
            expect(page).to_have_title("Lambda-Wave SGRT")

            print("Waiting for WebSocket connection...")
            # Wait for status to change from "Connecting..." to "Connected"
            try:
                page.wait_for_selector("#status:has-text('Connected')", timeout=10000)
                print("WebSocket Connected.")
            except:
                print("Timeout waiting for WebSocket connection.")
                # Capture what we have
                page.screenshot(path="verification/viz_failure.png")
                sys.exit(1)


            print("Waiting for Data Stream...")
            # Wait for Beam Status to appear (indicates JSON parsed)
            try:
                page.wait_for_selector("#status:has-text('BEAM STATUS')", timeout=10000)
                print("Data Stream Received.")
            except:
                 print("Timeout waiting for Beam Status.")
                 page.screenshot(path="verification/viz_failure_data.png")
                 sys.exit(1)

            # Wait a bit for points to accumulate/draw on canvas
            time.sleep(3)

            print("Taking Screenshot...")
            os.makedirs("verification", exist_ok=True)
            page.screenshot(path="verification/viz_verification.png")
            print("Screenshot saved to verification/viz_verification.png")

    finally:
        print("Terminating Server...")
        server_process.terminate()
        try:
            server_process.wait(timeout=5)
        except subprocess.TimeoutExpired:
            server_process.kill()

        # Optional: Print server output for debug
        # outs, errs = server_process.communicate()
        # print("Final Server Stdout:\n", outs[-500:] if outs else "")
        # print("Final Server Stderr:\n", errs[-500:] if errs else "")

if __name__ == "__main__":
    verify_viz()
