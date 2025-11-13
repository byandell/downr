# downr

The package [downr](https://github.com/byandell/downr)
has a Shiny Download App that builds on the shiny
[download](https://mastering-shiny.org/action-transfer.html#download)
toolset, which provides generic download features.

This package provides a customized look and use
based on interaction with a team of researchers.
It hopefully will have broader utility.
It relies on a `download_list`, which is a
[reactiveValues](https://mastering-shiny.org/reactivity-objects.html)
object, to supply `Plot()` and `Table()` reactives
as well as a reactive `Filename()` that contains
a named vector for the base of the filename for these
when asked to be saved.

## Download List

The `download_list` is a list of reactives

```
download_list
├── Plot      # reactive with plot to be saved
├── Table     # reactive with table to be saved
└── Filename  # reactive with character 2-vector of file names
```

## Use in shiny app

Typically, user would return a result from their server function
that provides the currently viewed plot and table information.

```
xxxApp <- function() {
  ui <- bslib::page(
    downr::downloadInput("download") # inputs for Plot or Table
  )
  server <- function(input, output, session) { 
    download_list <- xxxServer("xxx")
    downr::downloadServer("download", download_list, addDate = TRUE)
  }
  shiny::shinyApp(ui, server)
}
```

Here is a simplified form with simple plot and table provided.
A more complicated version is in the
[downloadApp.R](https://github.com/byandell/downr/blob/main/R/downloadApp.R).

```
xxxServer <- function(id) {
  shiny::moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
    # Download.
    download_Plot <- shiny::reactive(downr::plot_null("none"))
    download_Table <- shiny::reactive(matrix(1:12,nrow=3))
    download_Filename <- shiny::reactive(c(Plot = "none", Table = "twelve"))
    download_list <- shiny::reactiveValues(
      Filename = download_Filename,
      Plot = download_Plot,
      Table = download_Table)
      
    # Return.
    download_list
  })
}
```

## Package use

Install package:

```
# Install devtools from CRAN
install.packages("devtools")

# Install downr from GitHub
library(devtools)
install_github("byandell/downr")
```

## Options to Consider

The `downloadServer()` has the option `addDate` (used above)
to postpend the date as `YYYYMMDD` to the base filename.
The `downloadUI()` user interface, if included in the `ui()` function,
allows the user to change the dimensions of the plot if desired.
The `downloadUI()` can be skipped, as illustrated with
[downloadHideApp()](https://github.com/byandell/downr/blob/main/R/downloadHideApp.R).
There is also a `downloadOutput()` UI function which is used with the
`downloadApp()` to preview the download app functionality,
but is not meant for more general use.

The apps
[downloadPlotApp()](https://github.com/byandell/downr/blob/main/R/downloadPlotApp.R)
and
[downloadTableApp()](https://github.com/byandell/downr/blob/main/R/downloadTableApp.R)
can be used on their own if only one or the other is needed.
In a sense,
[downloadApp()](https://github.com/byandell/downr/blob/main/R/downloadApp.R)
is a wrapper for these two apps.
