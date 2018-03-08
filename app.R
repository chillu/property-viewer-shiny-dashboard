library(shiny)
library(tidyverse)
library(lubridate)
library(dplyr)
library(leaflet)
library(htmltools)
library(scales)
library(geosphere)

# TODO Doesn't include all suburbs
wards = c(
  "aro valley" = "centre",
  "berhampore" = "south",
  "breaker bay" = "east",
  "broadmeadows" = "west",
  "brooklyn" = "south",
  "churton park" = "north",
  "crofton downs" = "west",
  "glenside" = "north",
  "grenada north" = "north",
  "grenada village" = "north",
  "hataitai" = "east",
  "highbury" = "centre",
  "horokiwi" = "north",
  "houghton bay" = "east",
  "island bay" = "south",
  "johnsonville" = "north",
  "kaiwharawhara" = "west",
  "karaka bays" = "east",
  "karori" = "west",
  "kelburn" = "centre",
  "khandallah" = "west",
  "kilbirnie" = "east",
  "kingston" = "south",
  "lyall bay" = "east",
  "makara beach" = "west",
  "makara" = "west",
  "maupuia" = "east",
  "melrose" = "east",
  "miramar" = "east",
  "moa point" = "east",
  "mornington" = "south",
  "mount cook" = "centre",
  "mount victoria" = "centre",
  "newlands" = "north",
  "newtown" = "south",
  "ngaio" = "west",
  "ngauranga" = "west",
  "northland" = "west",
  "ohariu" = "north",
  "oriental bay" = "centre",
  "owhiro bay" = "south",
  "paparangi" = "north",
  "pipitea" = "centre",
  "rongotai" = "east",
  "roseneath" = "east",
  "seatoun" = "east",
  "southgate" = "south",
  "strathmore park" = "east",
  "takapu valley" = "north",
  "tawa" = "north",
  "te aro" = "centre",
  "thorndon" = "centre",
  "vogeltown" = "south",
  "wadestown" = "west",
  "wellington central" = "centre",
  "wilton" = "west",
  "woodridge" = "north"
)

props <- read_csv("./sample-data.csv",
  na = c("", "NULL")
)
props <- mutate(
  props,
  # TODO Use tags$a (currently only applies for the first record)
  href = paste('<a href="', url, '">', address, "</a>"),
  over_cv = round(price / capital_value, digits = 2),
  price_by_floor_area = round(price / floor_area),
  price_by_land_area = round(price / land_area),
  view = paste(view_type, ': ', view_scope),
  ward = wards[str_to_lower(suburb_name)]
)

ui <- function(request) {
  fluidPage(
    titlePanel("Property Trends Analysis Dashboard"),
    
    sidebarLayout(
      sidebarPanel(
        radioButtons(
          "location_type",
          "Location",
          choices = list("By Surburb" = "suburb", "By Coordinates" = "coordinates", 'By Ward' = "ward"), 
          selected = "suburb"
        ),
        
        conditionalPanel(
          condition = "input.location_type == 'suburb'",
          selectInput(
            "suburb_name",
            "Suburb",
            choices = c(c("All"), sort(
              unique(props$suburb_name)
            )),
            multiple = TRUE
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
            max = 10,
            post = ' km',
            step = 0.5,
            value = 1
          )
        ),
        conditionalPanel(
          condition = "input.location_type == 'ward'",
          selectInput(
            "ward",
            "Ward",
            choices = c('north', 'south', 'east', 'west', 'centre')
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
          min = 0,
          max = 12*10,
          value = c(0,12),
          step = 6
        ),
        
        bookmarkButton()
      ),
      
      mainPanel(tabsetPanel(
        type = "tabs",
        tabPanel("Plot",
          selectInput(
            "plot_by",
            "Plot by",
            choices = c(
              "% over RV" = 'over_cv', 
              "Price by floor area" = 'price_by_floor_area',
              "Price" = 'price'
            )
          ),
          plotOutput("plot"),
          fluidRow(
            column(4,
              plotOutput("plot_price_by_floor_area_by_decade")    
            ),
            column(4,
              plotOutput("plot_duration_supermarket_over_cv")
            ),
            column(4,
              plotOutput("plot_duration_centre_over_cv")
            )
          ),
          fluidRow(
            column(6,
              plotOutput("plot_view_over_cv")
            ),
            column(6,
              plotOutput("plot_view_by_floor_area")
            )
          )
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
        tabPanel("By Suburb",
          dataTableOutput('table_by_suburb_with_price')
        ),
        tabPanel("All Data",
          dataTableOutput('table')
        )
      ))
    )
  )
}
server <- function(input, output) {
  props_filtered = function() {
    if (input$location_type == "coordinates" && !is.na(as.numeric(input$location_lat))  && !is.na(as.numeric(input$location_lng))) {
      location_type = 'coordinates'
    } else {
      location_type = input$location_type
    }
    tmp = filter(
      props,
      price_on < (now() - months(input$months_ago[1])),
      price_on > (now() - months(input$months_ago[2])),
      between(
        capital_value,
        input$capital_value[1],
        input$capital_value[2]
      ),
      between(floor_area, input$floor_area[1], input$floor_area[2]),
      between(land_area, input$land_area[1], input$land_area[2]),
      # Ignore extreme outliers that make the plots hard to read
      over_cv < 3
    )
    
    # TODO Inline into filter() for better readability
    if(location_type == 'coordinates') {
      tmp = filter(tmp, distHaversine(c(as.numeric(input$location_lng), as.numeric(input$location_lat)), cbind(lng, lat)) < (input$location_radius * 1000))
    } else if(location_type == 'suburb') {
      tmp = filter(tmp, length(input$suburb_name) == 0 | suburb_name %in% input$suburb_name)
    } else if(location_type == 'ward') {
      tmp = filter(tmp, ward == input$ward)
    }
    
    tmp
  }
  
  props_filtered_by_suburb_with_price <- function() {
    by_suburb = group_by(props_filtered(), suburb_name)
    summarise(
      by_suburb,
      count = n(),
      round(mean(over_cv), 2),
      round(mean(price)),
      round(mean(price_by_floor_area))
    )
  }
  
  output$plot <- renderPlot({
    ggplot(props_filtered(), aes(price_on, get(input$plot_by))) +
      geom_point(aes(size = price), alpha = 1/3) +
      geom_smooth() +
      labs(
        title = 'Price' 
      )
  })
  
  output$plot_price_by_floor_area_by_decade <- renderPlot({
    ggplot(props_filtered(), aes(decade_built, get(input$plot_by)), show.legend = FALSE) +
      geom_point(aes(size = price), alpha = 1/3) +
      geom_smooth() +
      labs(
        title = 'Price by floor area by decade'
      )
  })
  
  output$plot_duration_supermarket_over_cv <- renderPlot({
    ggplot(props_filtered(), aes(duration_supermarket, get(input$plot_by)), show.legend = FALSE) +
      geom_smooth() +
      labs(
        title = 'Driving time to nearest supermarket'
      )
  })
  
  output$plot_duration_centre_over_cv <- renderPlot({
    ggplot(props_filtered(), aes(duration_transit, get(input$plot_by)), show.legend = FALSE) +
      geom_smooth() +
      labs(
        title = 'Public transit time to centre',
        subtitle = 'To Courtenay Place on Monday at 7:30'
      )
  })
  
  output$plot_view_over_cv <- renderPlot({
    ggplot(props_filtered(), aes(view, get(input$plot_by)), show.legend = FALSE) +
      geom_point(aes(size = price), alpha = 1/3) +
      geom_smooth() +
      labs(
        title = 'View type'
      ) +
      coord_flip()
  })
  
  output$plot_view_by_floor_area <- renderPlot({
    ggplot(props_filtered(), aes(view, get(input$plot_by)), show.legend = FALSE) +
      geom_point(aes(size = price), alpha = 1/3) +
      geom_smooth() +
      labs(
        title = 'View type'
      ) +
      coord_flip()
  })
  
  output$table_by_suburb_with_price <- renderDataTable(
    props_filtered_by_suburb_with_price(),
    options = list(
      pageLength = 20,
      columns = list(
        list(title = "Suburb"),
        list(title = "Count"),
        list(title = "% over RV (mean)"),
        list(title = "Sale price (mean)"),
        list(title = "Price by sqm (mean)")
      )
    )
  )
  
  pal <- colorNumeric(c("green", "yellow", "red"), 0:1)
  colour <- function() {
    switch(
      input$dot_colour,
      # Ignore CV outliers over 2
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
    leaflet(props_filtered()) %>%
      addProviderTiles(
        providers$Stamen.TonerLite,
        options = providerTileOptions(noWrap = TRUE)
      ) %>%
      addCircleMarkers(
        radius = ~ (price / 100000),
        stroke = FALSE,
        fillOpacity = 0.8,
        color = colour(),
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