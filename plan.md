1. **Add `activity_patterns` to Visualisations menu in UI.**
   - In `ui.R`, add a new `nav_panel` for "Activity Patterns" under `nav_menu(title = "Visualisations")`. It should have the value `"activity_patterns"`.
   - The UI should have the same 4-tab layout as Species Dashboards: Overall, Current complete season, Prior season, Last year period.
   - For each tab, use `plotOutput` for the activity pattern plot.
2. **Add sidebar logic for Activity Patterns in UI.**
   - Add conditional panels in the global sidebar for `input.nav === 'activity_patterns'`.
   - It should use `mapping_module_ui(..., view = "select_species")` allowing for a species selection (similar to Density Map) and `mapping_module_ui(..., view = "select_localities")` for locality selection.
3. **Implement server logic in `server.R` or a module.**
   - We need to handle the reactive inputs: `selected_species` and `selected_localities` from the new sidebar panels.
   - We need to implement a new function to generate the multi-species activity pattern plot.
   - The plot should take multiple species, compute their hourly counts, and plot them as separate lines/polygons (using `ggplot2` polar coordinates or a standard line plot). The user asked for "plot multiple species on the same plot for comparison purposes. So each species will show as a separate colour. This could be trickly to show on the graph, I am not sure if we can use opacity? Realistically I am only expecting to have 2-3 species on the plot at a time."
   - Create a module or add server code to render plots for "Overall", "Current", "Prior", and "Last year".
4. **Complete pre commit steps.**
   - Call `pre_commit_instructions` to ensure proper testing, verifications, reviews, and reflections are done.
5. **Submit the change.**
   - Commit and submit.
