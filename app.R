library(shiny)
library(tidyverse)
library(lubridate)
library(dplyr)
library(leaflet)
library(htmltools)
library(scales)
library(geosphere)

props <- read_csv("./sample-data.csv",
  na = c("", "NULL")
)
props <- mutate(
  props,
  # TODO Use tags$a (currently only applies for the first record)
  href = paste('<a href="', url, '">', address, "</a>"),
  over_cv = round(price / capital_value, digits = 2),
  price_by_floor_area = round(price / floor_area),
  price_by_land_area = round(price / land_area)
)

ui <- function(request) {
  fluidPage(
    titlePanel("Property Trends Analysis Dashboard"),
    
    sidebarLayout(
      sidebarPanel(
        radioButtons(
          "location_type",
          "Location",
          choices = list("By Surburb" = "suburb", "By Coordinates" = "coordinates"), 
          selected = "suburb"
        ),
        
        conditionalPanel(
          condition = "input.location_type == 'suburb'",
          selectInput(
            "suburb_name",
            "Suburb",
            choices = c(c("All"), sort(
              unique(props$suburb_name)
            ))
          )
        ),
        conditionalPanel(
          condition = "input.location_type == 'coordinates'",
          textInput(
            "location_lat",
            "Latitude"
          ),
          textInput(
            "location_lng",
            "Longitude"
          ),
          sliderInput(
            inputId = "location_radius",
            label = "Radius in km",
            min = 0.5,
            max = 5,
            post = ' km',
            step = 0.5,
            value = 1
          )
        ),
        
        sliderInput(
          inputId = "floor_area",
          label = "Floor Area",
          min = 10,
          max = 300,
          post = ' sqm',
          step = 10,
          value = c(60, 140)
        ),
        
        sliderInput(
          inputId = "land_area",
          label = "Land Area",
          min = 100,
          max = 2000,
          post = ' sqm',
          step = 10,
          value = c(100, 1000)
        ),
        
        sliderInput(
          inputId = "capital_value",
          label = "Rateable Value",
          min = 100000,
          max = 1000000,
          step = 50000,
          pre = '$',
          value = c(200000, 500000)
        ),
        
        sliderInput(
          inputId = "months_ago",
          label = "Sold date (months ago)",
          min = 1,
          max = 12,
          value = 6
        ),
        
        bookmarkButton()
      ),
      
      mainPanel(tabsetPanel(
        type = "tabs",
        tabPanel("Plot",
          plotOutput("plot"),
          plotOutput("plot_price_by_floor_area_by_decade")
        ),
        tabPanel("Map",
          leafletOutput("map"),
          selectInput(
            "dot_colour",
            "Dot Colour",
            choices = c(
              "% over RV" = 'over_cv', 
              "Price by floor area" = 'price_by_floor_area',
              "Commute by bike" = 'duration_bicycling',
              "Commute by car" = 'duration_driving',
              "Commute by bus" = 'duration_transit'
            )
          )
        ),
        tabPanel("Table",
          dataTableOutput('table')
        )
      ))
    )
  )
}
server <- function(input, output) {
  props_filtered = function() {
    is_using_coords <- function() {
      input$location_type == "coordinates" && !is.na(as.numeric(input$location_lat))  && !is.na(as.numeric(input$location_lng))
    }
    filter(
      props,
      price_on > (now() - months(input$months_ago)),
      between(
        capital_value,
        input$capital_value[1],
        input$capital_value[2]
      ),
      between(floor_area, input$floor_area[1], input$floor_area[2]),
      between(land_area, input$land_area[1], input$land_area[2]),
      (
        # Filter by radius
        is_using_coords() 
        & distHaversine(c(as.numeric(input$location_lng), as.numeric(input$location_lat)), cbind(lng, lat)) < (input$location_radius * 1000)
      ) | (
        # Filter by suburb, if one is chosen
        !is_using_coords()
        & (input$suburb_name == "All" | suburb_name == input$suburb_name)
      )
    )
  }
  
  output$plot <- renderPlot({
    ggplot(props_filtered(), aes(price_on, over_cv)) +
      geom_point(aes(size = price), alpha = 1/3) +
      geom_smooth()
  })
  
  output$plot_price_by_floor_area_by_decade <- renderPlot({
    ggplot(props_filtered(), aes(decade_built, price_by_floor_area)) +
      geom_point(aes(size = price), alpha = 1/3)
  })
  })
  
  pal <- colorNumeric(c("green", "yellow", "red"), 0:1)
  color <- function() {
    switch(
      input$dot_colour,
      "over_cv" = ~ pal(rescale(over_cv, from = c(0.8, 2))),
      "price_by_floor_area" = ~ pal(rescale(price_by_floor_area, from = c(1000, 12000))),
      "duration_bicycling" = ~ pal(rescale(duration_bicycling)),
      "duration_transit" = ~ pal(rescale(duration_transit)),
      "duration_driving" = ~ pal(rescale(duration_driving))
    )
  }
  label <- function() {
    switch(
      input$dot_colour,
      "over_cv" = ~ htmlEscape(paste("% over RV: ", as.character(over_cv))),
      "price_by_floor_area" = ~ htmlEscape(paste("Price by floor area: $", as.character(price_by_floor_area))),
      "duration_bicycling" = ~ htmlEscape(paste("Commute by bike: ", as.character(duration_bicycling))),
      "duration_driving" = ~ htmlEscape(paste("Commute by car: ", as.character(duration_driving))),
      "duration_transit" = ~ htmlEscape(paste("Commute by bus: ", as.character(duration_transit)))
    )
  }
  popup <- function() {
    # TODO Is this horribly inefficient?
    props = props_filtered()
    as.character(tagList(
      tags$a(href=props$url, target="_blank", props$address),
      tags$br(),
      tags$span(paste("Last sold: ", as.character(props$price_on))),
      tags$br(),
      tags$span(paste("RV: ", as.character(props$capital_value), ", Price: ", as.character(props$price))),
      tags$br(),
      tags$span(paste("Floor area: ", as.character(props$floor_area), ", Land area: ", as.character(props$land_area))),
      tags$br(),
      tags$span(paste("Commute by bicycling: ", as.character(props$duration_bicycling), " (", as.character(props$distance_total_bicycling), "km)")),
      tags$br(),
      tags$span(paste("Commute by driving: ", as.character(props$duration_driving), " (", as.character(props$distance_total_driving), "km)")),
      tags$br(),
      tags$span(paste("Commute by transit: ", as.character(props$duration_transit), " (", as.character(props$distance_total_transit), "km)"))
    ))
  }
  output$map <- renderLeaflet({
    # Render unfiltered list, but set bounds
    leaflet(props_filtered()) %>%
      addProviderTiles(
        providers$Stamen.TonerLite,
        options = providerTileOptions(noWrap = TRUE)
      ) %>%
      addCircleMarkers(
        radius = ~ (price / 100000),
        stroke = FALSE,
        fillOpacity = 0.8,
        # Ignore CV outliers over 2
        color = color(),
        label = label(),
        popup = popup()
      )
  })
  
  output$table <- renderDataTable(
    select(
      props_filtered(),
      href,
      capital_value,
      price,
      over_cv,
      price_on,
      decade_built,
      floor_area,
      land_area
    ),
    options = list(pageLength = 20),
    # Don't escape URL field
    escape = c(-1)
  )
  
}

enableBookmarking(store = "url")
shinyApp(ui = ui, server = server)