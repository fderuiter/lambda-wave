import os
from playwright.sync_api import sync_playwright

def verify_dashboard():
    with sync_playwright() as p:
        browser = p.chromium.launch()
        page = browser.new_page()

        # Open the local HTML file directly
        # Since we can't run the Haskell server easily in this env, we test the HTML structure.
        cwd = os.getcwd()
        page.goto(f"file://{cwd}/app/Control/WebUI/assets/index.html")

        # Verify Title
        assert page.title() == "Lambda-Wave SGRT Dashboard"
        print("Title Verified")

        # Verify Canvas exists
        assert page.locator("#radarCanvas").is_visible()
        print("Canvas Verified")

        # Verify Status Indicator
        assert page.locator("#beam-status").is_visible()
        assert page.locator("#beam-status").inner_text() == "BEAM OFF"
        print("Status Indicator Verified")

        # Verify WebSocket Status (Should be disconnected as server isn't running)
        assert page.locator("#ws-status").inner_text() == "Disconnected"
        print("WS Status Verified (Disconnected)")

        # Take Screenshot
        page.screenshot(path="test/WebUI/dashboard_screenshot.png")
        print("Screenshot saved to test/WebUI/dashboard_screenshot.png")

        browser.close()

if __name__ == "__main__":
    verify_dashboard()
