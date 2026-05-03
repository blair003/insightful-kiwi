from playwright.sync_api import sync_playwright

def run_cuj(page):
    page.goto("http://127.0.0.1:3000/")
    page.wait_for_timeout(3000)

    # Click on "Visualisations" menu
    page.get_by_text("Visualisations").click()
    page.wait_for_timeout(1000)

    # Click on "Playback Map"
    page.get_by_text("Playback Map").click()
    page.wait_for_timeout(3000)

    # Wait for the map and sidebar to load
    page.screenshot(path="/home/jules/verification/screenshots/verification_1.png")
    page.wait_for_timeout(1000)

    # Change to single period
    page.get_by_text("Single Period").click()
    page.wait_for_timeout(1000)

    # Click play button
    try:
        page.get_by_role("button", name="Play").click()
        page.wait_for_timeout(5000)
    except:
        pass

    # Take screenshot at the key moment
    page.screenshot(path="/home/jules/verification/screenshots/verification_2.png")
    page.wait_for_timeout(1000)  # Hold final state for the video

if __name__ == "__main__":
    import os
    os.makedirs("/home/jules/verification/videos", exist_ok=True)
    os.makedirs("/home/jules/verification/screenshots", exist_ok=True)
    with sync_playwright() as p:
        browser = p.chromium.launch(headless=True)
        context = browser.new_context(
            record_video_dir="/home/jules/verification/videos",
            viewport={'width': 1280, 'height': 800}
        )
        page = context.new_page()
        try:
            run_cuj(page)
        finally:
            context.close()  # MUST close context to save the video
            browser.close()
