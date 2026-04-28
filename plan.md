1. **Enable Shiny Bookmarking**
   - Update `global.R` to add `enableBookmarking("url")`.
   - Update `ui.R` to wrap the UI definition in `function(request) { ... }`.
   - Ensure the server function signature supports bookmarking (no change needed as `server <- function(input, output, session)` is standard).
2. **Add a Global Share Button UI**
   - In `ui.R`, add a global share button to the navigation bar using `nav_item(actionLink("global_share_btn", label = NULL, icon = icon("share-nodes"), title = "Share this view"))`.
3. **Handle Global Share Button Click**
   - In `server.R`, add `observeEvent(input$global_share_btn, { session$doBookmark() })`.
   - Add `onBookmarked(function(url) { showModal(...) })` in `server.R` to present a modal to the user containing the generated URL and a copy button.
4. **Add Share Button to Observation Modals**
   - In `includes/media_functions.R` (`show_image_modal`) and `includes/server_observation_handlers.R` (`show_review_sequences_modal`), add a share button inside the modal dialog header/title.
   - When clicked, it should copy a URL formatted as `?observation_id=<observation_id>` to share the specific observation page view.
   - This can be done via a Javascript action to construct the URL and copy it, or by calling back to Shiny.
5. **Add JavaScript for Clipboard Copy**
   - Add a Javascript function in `www/custom.js` (`function copyToClipboard(text) { ... }`) to copy the generated URL to the clipboard, along with a user feedback mechanism (like a small toast or changing the button text temporarily).
6. **Verify and Pre-commit Checks**
   - Run the app and ensure state is correctly saved/restored with bookmarking.
   - Run `pre_commit_instructions` tool.
