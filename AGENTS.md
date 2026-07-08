# downr — Project Memory

## Package Overview

`downr` is an R package providing reusable Shiny download modules for plots and tables. It wraps the Shiny `downloadHandler` toolset with a customized UI/UX developed for research team workflows.

- **GitHub**: https://github.com/byandell/downr
- **License**: GPL-3
- **Dependencies**: `bslib`, `DT`, `ggplot2`, `shiny`

## Key R Files

| File | Purpose |
|------|---------|
| `R/downloadApp.R` | Full download app (plot + table) with optional date in filename and filename display |
| `R/downloadPlotApp.R` | Standalone plot-only download app |
| `R/downloadTableApp.R` | Standalone table-only download app |
| `R/plot_null.R` | Helper to produce a null/placeholder plot |

## Core Concepts

### `download_list`

The central data structure passed to download servers. It is a `shiny::reactiveValues` object (or a reactive that evaluates to one) with three named reactives:

```
download_list
├── Plot      # reactive returning a plot object
├── Table     # reactive returning a data frame / matrix
└── Filename  # reactive returning a named character vector c(Plot = "...", Table = "...")
```

### Exported Functions

- `downloadInput(id)` — UI input controls (type selector, optional width/height via `downloadUI()`)
- `downloadServer(id, download_list, addDate)` — server-side module
- `downloadUI(id)` — optional UI for plot dimension controls
- `downloadOutput(id)` — UI for previewing download app output
- `downloadApp()` — standalone demo/test app (plot + table)
- `downloadPlotApp()` — standalone demo app (plot only)
- `downloadTableApp()` — standalone demo app (table only)
- `plot_null(label)` — returns a blank ggplot with a label

## Usage Pattern

```r
xxxApp <- function() {
  ui <- bslib::page(
    downr::downloadInput("download")
  )
  server <- function(input, output, session) {
    download_list <- xxxServer("xxx")
    downr::downloadServer("download", download_list, addDate = TRUE)
  }
  shiny::shinyApp(ui, server)
}
```

## Roxygen Notes

- Uses roxygen2 v8+: each `@importFrom` tag **must be a single line** (no continuation-indented multi-line form).
- `RoxygenNote` in DESCRIPTION should be updated after running `devtools::document()`.
