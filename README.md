# 🏈 NFL Report Engine (Shiny App)

📍 **Live App**: [Click here to launch](https://achappidspoa.shinyapps.io/NFL_Report_Engine/)

This interactive Shiny dashboard visualizes 2024 NFL play-by-play data to help coaches, analysts, and fans understand key team tendencies and efficiencies. The app was developed as part of the UNC Charlotte NFL Analytics project and leverages the `nflfastR` package and curated play-by-play data.

---

## 📂 Project Files

### `prepare_data.R`
- Downloads and processes 2024 play-by-play data using `nflfastR`
- Selects only relevant columns for performance and efficiency analysis
- Saves a slimmed `.rds` file (`pbp_app_data.rds`) for use in the Shiny app

### `app.R`
- Builds an interactive Shiny app UI using `shiny`, `gt`, and `ggplot2`
- Loads the trimmed dataset (`pbp_app_data.rds`)
- Displays EPA-based team performance and down-by-down success rate tables
- Includes customized fonts, themes, and color palettes for presentation-ready visuals

---

## 🧪 Features

- Filterable 2024 regular season play-by-play data
- Success rate tables by down, team, or play type
- GT tables with embedded visuals and conditional formatting
- Clean, mobile-friendly design using `shinythemes` and Google Fonts

---

## 🛠️ How to Run Locally

1. Clone the repository
   ```bash
   git clone https://github.com/akhimass/NFLShinyReportApp.git
   cd NFLShinyReportApp
