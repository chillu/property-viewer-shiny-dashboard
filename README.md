# Property Trends Analysis Dashboard

Make data-driven decisions, particularly for the likely largest purchase of your life!
This dashboard is built on [Shiny](http://shiny.rstudio.com/) and the
[R](https://www.r-project.org/) programming language (a proud New Zealand invention!).

*Note: Uses anonymised and outdated sample data, please don't base any decisions on this
without getting your own data set.*

## Features

 * Interactive map with dot size mapped to sales price, and dot colour mapped to "% over RV"
 * Filter by neighbourhood
 * Filter by property attributes like floor space or RV
 * Browse tabular data for max. detail
 * Plot sales trends over time (e.g. shifts in "% over RV")

## Installation

```
install.packages("shiny")
install.packages("tidyverse")
install.packages("leaflet")
install.packages("scales")
install.packages("geosphere")
install.packages("ggmap")
```

## Run

```
R -e "shiny::runApp('.')"
```

## Screenshots

![Main](_img/screenshot_main.png)
![Analysis](_img/screenshot_analysis.png)
![Map](_img/screenshot_map.png)
![Data](_img/screenshot_data.png)