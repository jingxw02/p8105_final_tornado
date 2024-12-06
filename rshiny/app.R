library(shiny)
library(shinyjs)
library(shinyBS)
library(leaflet)
library(leaflet.extras)
library(dplyr)
library(leaflet.extras)
library(shinythemes)
library(ggplot2)
library(stringr)
library(rapport)
library(RColorBrewer)
library(viridis)
library(htmltools)
library(rsconnect)
library(shinytreeview)
library(shinyWidgets)
library(purrr)
library(shinydashboard)
library(DT)
library(htmlwidgets)
library(jsonlite)
library(shiny.fluent)

data_tornado <- read.csv("data/1950-2023_actual_tornadoes.csv") |>
  filter(yr >= 2000) |>
  mutate(tloss = loss * 1000000,
         ID = paste(yr, om, sep = "_"))


ui <- fluidPage(
  #choose a CSS theme -- you can also create a custom theme if you know CSS
  theme = shinytheme("cosmo"),
  #create a navigation bar for the top of the app, and give it a main title
  navbarPage("Tornado ShinyApp",
             #add the first tab panel (tab1) and annotate -- the tags$h command adds text at different sizes
             tabPanel("Tornado Map",
                      tags$h2("Interactive Tornado Map", align = "center"),
                      tags$h6("Tornado data from NOAA."),
                      #create the sidebar that will hold the input functions that we add
                      sidebarLayout(
                        sidebarPanel(
                          #create inputs for location, magnititude and date
                          # add input for observational data
                          sliderInput(inputId = 'years', 
                                      label = 'Years', 
                                      min = min(data_tornado$yr, na.rm = TRUE), 
                                      max = max(data_tornado$yr, na.rm = TRUE), 
                                      value = c(2000, 2023),
                                      step = 1,
                                      animate = animationOptions(
                                        interval = 500,
                                        loop = FALSE,
                                        playButton = icon("play", "fa-2x"),
                                        pauseButton = icon("pause", "fa-2x")
                                      )
                          ),
                          
                          numericInput("loss_min", 
                                       label = "Loss (From):", 
                                       value = 0, 
                                       min = 0, 
                                       step = 100000),
                          
                          numericInput("loss_max", 
                                       label = "Loss (To):", 
                                       value = 1.55000e+15, 
                                       min = 0, 
                                       step = 100000),
                          
                          actionButton("paths", "Display Connecting Lines", width = '150px',
                                       style='border-color: #565655;
                                       background-color: #F47831;
                                       padding:3px'),
                          
                          # add action button to clear site markers
                          actionButton("clearpaths", "Clear Connecting Lines",
                                       width = '150px',
                                       style='border-color: #565655;
                                       background-color: #F47831;
                                       padding:3px'),
                          
                          actionButton("starts", "Display Start Points", width = '150px',
                                       style='border-color: #565655;
                                       background-color: #008080;
                                       padding:3px'),
                        
                          actionButton("clearstarts", "Clear Start Points",
                                       width = '150px',
                                       style='border-color: #565655;
                                       background-color: #008080;
                                       padding:3px'),
                          
                          actionButton("ends", "Display End Points", width = '150px',
                                       style='border-color: #565655;
                                       background-color: #FF69B4;
                                       padding:3px'),
                          
                          # add action button to clear site markers
                          actionButton("clearends", "Clear End Points",
                                       width = '150px',
                                       style='border-color: #565655;
                                       background-color: #FF69B4;
                                       padding:3px'),
                          
                          
                          checkboxGroupInput("month_select", 
                                             label = "Select Month:",
                                             choices = c(1:12),
                                             selected = NULL),
                          
                          # checkboxGroupInput("state_select",
                          #                    label = "Select State:",
                          #                    choices = unique(data_tornado$st),
                          #                    selected = NULL),
                          
                          
                            selectizeInput(
                              inputId = "state_select",
                              label = "Select States (Search Enabled):",
                              choices = unique(data_tornado$st),
                              selected = NULL,
                              multiple = TRUE,  # allow multiple choice
                              options = list(placeholder = 'Type to search or select states...')
                            ),
                          
                          
                          checkboxGroupInput("mag_select",
                                             label = "Select magnititude:",
                                             choices = unique(data_tornado$mag),
                                             selected = NULL)
                        ),
                        mainPanel(
                          tags$style(type = "text/css", "#mymap {height: calc(100vh - 200px) !important;}"),
                          leafletOutput(outputId = "mymap"))
                      )
             )
  )
)



server <- function(input, output, session) {
  
  output$mymap <- renderLeaflet({
    leaflet() %>%
      setView(lng = -121, lat = 34, zoom = 6.5) %>%
      addProviderTiles(providers$CartoDB.Positron, layerId = "base")
  })   
  
  obsFilter <- reactive({
    filter(data_tornado, data_tornado$st %in% input$state_select
           & data_tornado$mo %in% input$month_select 
           & data_tornado$yr >= input$years[1] 
           & data_tornado$yr <= input$years[2]
           & data_tornado$tloss >= input$loss_min
           & data_tornado$tloss <= input$loss_max
           & data_tornado$mag %in% input$mag_select)  %>%
      mutate(
        slon = as.numeric(slon),
        slat = as.numeric(slat),
        elon = as.numeric(elon),
        elat = as.numeric(elat)
      ) 
  })
  
  pathFilter <- reactive({filter(obsFilter(), obsFilter()$elon > 0
                                 & obsFilter()$elat > 0)})
  
  mag_to_color <- c(
    "1" = "cadetblue1",
    "-9" = "blueviolet",
    "2"= "chartreuse1",
    "5" = "coral1",
    "4" = "lightpink",
    "3" = "gold2",
     "0" = "azure2"
  )
  
  num_colors = length(unique(data_tornado$mag))
  
  observeEvent(input$paths, {
    pal <- colorFactor(palette = mag_to_color, levels = as.factor(unique(data_tornado$mag)))
    
    leafletProxy("mymap", session) %>%
      clearGroup("paths") %>%
      removeControl("paths_legend")
    
    
    for (i in 1:nrow(obsFilter())) {  # go through obsFilter data
      path_color <- pal(as.factor(obsFilter()$mag[i]))  # using color in pal
      
      leafletProxy("mymap", session) %>%
        addPolylines(
          lng = c(obsFilter()$slon[i], obsFilter()$elon[i]),
          lat = c(obsFilter()$slat[i], obsFilter()$elat[i]),
          color = path_color,
          weight = 5,
          dashArray = "5, 10",
          group = "paths"
        )
    }
    
    leafletProxy("mymap", session) %>%
      addLegend("topright",
                pal = pal,
                values = obsFilter()$mag,
                title = "Tornado Magnitude",
                layerId = "paths_legend",
                opacity = 1)
    
  })
  
  observeEvent(input$clearpaths, { 
    leafletProxy("mymap", session) %>%
      clearGroup("paths") %>%
      removeControl("paths_legend")
  })
  
  
  # observeEvent(input$paths, {
  #   path_data <- obsFilter()
  #   
  #   leafletProxy("mymap", session) %>%
  #     clearGroup("paths")  
  #   
  #   for (i in 1:nrow(path_data)) {
  #     leafletProxy("mymap", session) %>%
  #       addPolylines(
  #         lng = c(path_data$slon[i], path_data$elon[i]),
  #         lat = c(path_data$slat[i], path_data$elat[i]),
  #         weight = 5,
  #         color = "blue",
  #         group = "paths"
  #       )
  #   }
  # })

  observeEvent(input$starts, { # when the user selects the display startpoint input button
    pal_start <- "black"
    if (input$starts > 0) {
    leafletProxy("mymap", session) %>% # add a layer to the map
        removeControl("starts_legend") %>%
        clearGroup("starts") %>%
      addCircleMarkers( # add circular markers for starts locations
        lng = obsFilter()$slon, obsFilter()$slat,
        color = pal_start,
        stroke = TRUE,
        popup = paste("Loss:",obsFilter()$tloss,
                      "<br>Month:", obsFilter()$mo,
                      "<br>Year_ID:",obsFilter()$ID) %>% # popup with information about line + station
          lapply(htmltools::HTML), # read this html code
        radius = 2,
        weight = 1,
        group = "starts"
      ) 
    }  else {
      leafletProxy("mymap", session) %>% clearGroup("starts")  # Clear markers if unchecked
    }
    
    leafletProxy("mymap", session) %>%
      addLegend(
        "bottomleft",
        colors = pal_start,  
        labels = "Start Point",  
        title = "Tornado Path Start Points",
        layerId = "starts_legend",
        opacity = 1
      )
  })
  
  observeEvent(input$clearstarts, { 
    leafletProxy("mymap", session) %>%
      clearGroup("starts") %>%
      removeControl("starts_legend")
  })
  
  
  observeEvent(input$ends, { # when the user selects the display startpoint input button
    pal_end <- "blue"
    if (input$ends > 0) {
    leafletProxy("mymap", session) %>% # add a layer to the map
        removeControl("ends_legend") %>%
        clearGroup("ends") %>%
      addCircleMarkers( # add circular markers for starts locations
        lng = obsFilter()$elon, lat = obsFilter()$elat,
        color = pal_end,
        stroke = TRUE,
        popup = paste("Loss:",obsFilter()$tloss,
                      "<br>Month:", obsFilter()$mo,
                      "<br>Year_ID:",obsFilter()$ID) %>% # popup with information about line + station
          lapply(htmltools::HTML), # read this html code
        radius = 2,
        weight = 1,
        group = "ends"
      )
    }  else {
      leafletProxy("mymap", session) %>% clearGroup("ends")  # Clear markers if unchecked
    }
    
    leafletProxy("mymap", session) %>%
      addLegend(
        "bottomright",
        colors = pal_end,  
        labels = "End Point",  
        title = "Tornado Path End Points",
        layerId = "ends_legend",
        opacity = 1
      )
  })
  
  observeEvent(input$clearends, { 
    leafletProxy("mymap", session) %>%
      clearGroup("ends")  %>%
      removeControl("ends_legend")
  })
  

}

shinyApp(ui = ui, server = server)