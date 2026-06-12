1. **Add Dependency**: Make sure the `unmarked` package is added to the required environment packages in `config/environment.R` (if such a file exists).
2. **Add Config Flag**: Add an `occupancy_enabled` flag (default `TRUE`) to `config/config-wkt_main.R` (and potentially other config files) so the user can toggle this feature.
3. **Implement Occupancy Calculation (`calculate_occupancy`)**:
   - Add a new function in `R/functions/metrics_functions.R` to compute the occupancy matrix and use `unmarked::occu()` to return occupancy (`psi`) and standard error (`SE`).
   - Group the calculation by `rai_group` and network-wide / locality-wide. We need to define "sites" as camera `location` (or deployment). Let's use `deploymentID` or location. "Occasions" will be days within the deployment period.
4. **Integrate with Summary Functions**:
   - Update `calculate_rai()` or `generate_spp_summary()` / `generate_rai_group_summary()` in `R/functions/metrics_functions.R` to include Occupancy and Occupancy_SE calculations alongside RAI.
   - For example, in `generate_spp_summary()`, after calculating RAI by locality, run `calculate_occupancy` per species and join the results.
5. **Update UI / Table Display**:
   - In `R/functions/table_presentation_functions.R`, if `occupancy_enabled` is true, ensure the occupancy metrics are added to the output fields.
   - Update `dashboard_module.R` or `reporting_visualisations_module.R` as needed to display the Occupancy alongside RAI if applicable.
6. **Pre-commit and Test**:
   - Complete pre-commit checks as per `pre_commit_instructions`.
   - Start the Shiny server locally with mock data and confirm `Occupancy` appears and computes without crashing.
