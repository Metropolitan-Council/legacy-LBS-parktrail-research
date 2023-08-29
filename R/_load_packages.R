##### council packages #####
suppressMessages(library(councilR, quietly = TRUE))
suppressMessages(library(streetlightR, quietly = TRUE))

##### workflow #####
suppressMessages(library(keyring, quietly = TRUE))
suppressMessages(library(here, quietly = TRUE))
suppressMessages(library(docstring, quietly = TRUE))

##### general cleaning/manipulation/etc #####
suppressMessages(library(data.table, quietly = TRUE))
suppressMessages(library(openxlsx, quietly = TRUE))
suppressMessages(library(readxl, quietly = TRUE))
suppressMessages(library(dplyr, quietly = TRUE))
suppressMessages(library(tidyr, quietly = TRUE))
suppressMessages(library(purrr, quietly = TRUE))
suppressMessages(library(stringr, quietly = TRUE))
suppressMessages(library(janitor, quietly = TRUE))
suppressMessages(library(imputeTS, quietly = TRUE))
suppressMessages(library(lubridate, quietly = TRUE))
suppressMessages(library(units, quietly = TRUE))
suppressMessages(library(scales, quietly = TRUE))

##### visualization #####
suppressMessages(library(ggplot2, quietly = TRUE))
suppressMessages(library(RColorBrewer, quietly = TRUE))
suppressMessages(library(ggrepel, quietly = TRUE))
suppressMessages(library(cowplot, quietly = TRUE))
suppressMessages(library(htmlwidgets, quietly = TRUE))
suppressMessages(library(grid, quietly = TRUE))
suppressMessages(library(gridtext, quietly = TRUE))
suppressMessages(library(ggbeeswarm, quietly = TRUE))

##### fonts #####
suppressMessages(library(showtext, quietly = TRUE))
suppressMessages(library(sysfonts, quietly = TRUE))

##### spatial/mapping #####
suppressMessages(library(sf, quietly = TRUE))
suppressMessages(library(smoothr, quietly = TRUE))
suppressMessages(library(leaflet, quietly = TRUE))
suppressMessages(library(mapview, quietly = TRUE))
suppressMessages(library(osmdata, quietly = TRUE))

##### census #####
suppressMessages(library(tidycensus, quietly = TRUE))
suppressMessages(library(tigris, quietly = TRUE))

##### tables #####
suppressMessages(library(knitr, quietly = TRUE))
suppressMessages(library(kableExtra, quietly = TRUE))
suppressMessages(library(flextable, quietly = TRUE))


load_packages <- TRUE
cli::cli_inform(
  c("v" = "Packages\n"),
  .frequency = "once",
  .frequency_id = "load_packages"
)
