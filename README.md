# InsightfulKiwi

Welcome to **InsightfulKiwi**, a powerful, interactive analytical platform designed for wildlife monitoring and conservation efforts. Built as an R Shiny web application, InsightfulKiwi ingests standard Camera Trap Data Packages (Camtrap DP) and transforms raw monitoring data into accessible, actionable insights.

While the platform was originally inspired by efforts to protect New Zealand’s native Kiwi from introduced predators like stoats, its generalized, standardized data architecture makes it highly adaptable to almost any wildlife monitoring project across the globe.

---

## 🧭 For Conservationists and Project Managers

InsightfulKiwi empowers community projects, conservationists, and non-technical stakeholders to quickly interpret the health of their monitoring networks. The dashboard removes the complexity of raw data, allowing you to focus on the story the data tells.

### Key Features & What They Mean

- **Species Dashboards:** Dedicated views for specific animals (like target predators or protected native species). Here, you can monitor exactly how populations are trending over specific seasons.
- **The Dashboard Overview:** A quick, at-a-glance summary showing critical network statistics such as Total Detections, Active Camera Hours, and overall system health.
- **Interactive Mapping:**
  - **Density Maps:** Visualize *where* animal activity is concentrated across your project area. Perfect for identifying predator hotspots or native species sanctuaries.
  - **Observation Maps:** Pinpoint specific species observations across selected locations and time ranges.
- **Activity Patterns:** Understand the daily rhythms of your monitored wildlife. See exactly when predators are most active versus when native species are out, helping inform the timing of interventions like trapping.
- **Automated Reporting:** Generate comprehensive executive summaries and species reports in a single click (available in HTML and PDF). Perfect for sharing results with stakeholders, funding bodies, or the wider community.

### Understanding Key Metrics

- **RAI (Relative Abundance Index):** You will see RAI mentioned throughout the app. RAI is a standardized metric used by ecologists to measure the frequency of animal detections relative to the amount of effort put in (i.e., how many days a camera was active). A higher RAI indicates more frequent activity for that species in that area.

---

## 💻 For Developers and Data Scientists

InsightfulKiwi is built using R Shiny, leveraging the robust `bslib` framework for its UI. Data is loaded and structured using the `camtraptor` package to ensure strict adherence to the international Camtrap DP standard.

### Prerequisites & Dependencies

To run InsightfulKiwi locally, you must have R installed along with several packages. Core package dependencies include (but are not limited to):

- `shiny`, `bslib`, `shinyjs` (UI Framework & interactivity)
- `dplyr`, `tidyr`, `stringr`, `lubridate` (Data manipulation)
- `ggplot2`, `plotly`, `leaflet`, `sf` (Visualizations & Mapping)
- `inbo/camtraptor` (GitHub dependency for Camtrap DP parsing)

A full list of required packages can be found in `config/environment.R`. Basic packages like `logger` and `shiny` will attempt to automatically bootstrap missing components when you first launch the app.

### Project Setup and Data Loading

1. **Clone the Repository:** Download the InsightfulKiwi source code to your local machine.
2. **Provide your Data:**
   - InsightfulKiwi requires a valid Camtrap DP dataset.
   - Place your dataset (which must include a valid `datapackage.json` and associated CSVs) inside the `extdata/` directory.
   - Alternatively, you can modify the `camtrap_package` path variable in `config/environment.R` to point to an external directory.
3. **Configuration:**
   - Review `config/config-wkt_main.R`. This file controls deployment logic, including project organization name, species name display types, grouping periods, named species classes (e.g., Target vs Non-Target species), and threshold settings.

### Running the Application

Open an R session (or RStudio) in the root directory of the project and execute:

```R
shiny::runApp()
```

The application will launch in your default web browser.

### Architecture Notes

- **Data Centralization:** The core application data is processed once on startup and stored in a centralized list object named `core_data`, which contains interconnected dataframes (`media`, `obs`, `deps`).
- **Modules:** The application relies heavily on Shiny modules located in `R/modules/` to maintain clean separation of concerns (e.g., mapping, plotting, dashboards).
- **Asynchronous Execution:** Background tasks (like generating heavy PDF reports) utilize the `future` and `promises` packages to prevent blocking the main UI thread.

---

## 📬 Contact & Authorship

Developed by **Blair George**.

For more information about InsightfulKiwi, contributing, or using the tool for your specific conservation project, please contact: `blair@aketechnology.co.nz`.
