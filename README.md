# InsightfulKiwi

InsightfulKiwi is an R Shiny application designed for analyzing and visualizing wildlife camera monitoring data that adheres to the Camera Trap Data Package (Camtrap DP) format. It provides automated, in-depth insights into data collected from camera networks.

## Features

- **Dashboard**: A high-level overview displaying quick statistics like Kiwi Detections, Animal Detections, Camera Hours, and Species Observations grouped by time period.
- **Species Dashboards**: Dedicated dashboards providing detailed, focused insights and metrics for individual target species.
- **Reporting**: Automated generation of comprehensive reports (in HTML and PDF format) detailing executive summaries, results summaries, species summaries, and visualizations (e.g., daily species counts).
- **Density Maps**: Visual representation of species density across primary and comparative deployment seasons.
- **Observation Maps**: Interactive maps that pinpoint specific species observations across selected locations and time ranges. These maps now also support the custom import of trapping data to display alongside monitoring data.
- **Monitoring vs Trapping**: A dedicated feature to analyze and compare wildlife monitoring data with custom imported trapping records.
- **Raw Data Exploration**: Data tables for browsing and exploring raw observations and deployment data across all projects and seasons.

## Setup and Configuration

1.  **Dependencies**: Before running, ensure all required R packages are installed. You can review the main packages list in `instance/config/environment.R`. Basic packages like `shiny` and `logger` will automatically bootstrap missing components upon running.
2.  **Dataset**:
    - The application requires a Camtrap DP dataset.
    - Place the Camtrap DP monitoring dataset package inside `instance/extdata/monitoring-data/` (or modify the `camtrap_package` path variable in `instance/config/environment.R` to point to its location).
    - Ensure a valid `datapackage.json` is present in that directory.
3.  **Configurations**:
    - Various deployment logic settings such as organization name, species name display types, named species classes (like target vs interesting species), period groupings, and camera thresholds are controlled in `instance/config/*.R`. Adjust these to reflect your project before proceeding.

## Running the Application

1. Open R or RStudio in the root folder of this project.
2. Run the application using the following command:
   ```r
   shiny::runApp()
   ```
3. The app will open in your default browser.

## Contact & Authorship

Developed by Blair George. For more information about InsightfulKiwi, please contact: `blair@aketechnology.co.nz` (as documented in the configuration files and reporting templates).