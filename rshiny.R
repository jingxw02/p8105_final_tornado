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

data_tornado <- read.csv("data/filtered_tornado_data.csv")
data_tornado$ID <- paste(data_tornado$yr, data_tornado$om, sep = "_")

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
                                      animate = animationOptions(
                                        interval = 500,
                                        loop = FALSE,
                                        playButton = icon("play", "fa-2x"),
                                        pauseButton = icon("pause", "fa-2x")
                                      )
                          ),
                          
                          actionButton("paths", "Display Tornado Pathes", width = '150px',
                                       style='border-color: #565655;
                                       background-color: #F47831;
                                       padding:3px'),
                          
                          # add action button to clear site markers
                          actionButton("clearpathes", "Clear Tornado Pathes",
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
                          
                          checkboxGroupInput("state_select",
                                             label = "Select State:",
                                             choices = unique(data_tornado$st),
                                             selected = NULL),
                          
                          checkboxGroupInput("mag_select",
                                             label = "Select magnititude:",
                                             choices = unique(data_tornado$mag),
                                             selected = NULL),
                        ),
                        mainPanel(
                          tags$style(type = "text/css", "#mymap {height: calc(100vh - 200px) !important;}"),
                          leafletOutput(outputId = "mymap")),
                      )
             ),
             tabPanel("More information",
                      tags$h1("Data description"),
                      tags$h5('California Cooperative Oceanic Fisheries Investigation (CalCOFI) has been conducting marine ecosystem surveys in the California Current since 1949. More information about the CalCOFI program can be found on
                              the CalCOFI website:'), 
                      tags$a(href="https://calcofi.org/","https://calcofi.org/", style='color:#FFFFFF'),
                      tags$h5("The purpose of this Shiny App is to provide scientists with an interactive tool to 
                              visualize marine mammal data collected onboard CalCOFI. Here we integrate multiple datastreams, 
                              highlighting how marine mammal visual sightings and eDNA detections are represented through time and space. 
                              Please stay tuned for the addition of acoustic detections in a future release. By integrating visual, acoustic, and genetic sampling methods, 
                              we hope to better understand the detection capabilities of each method for detecting marine mammals in their environment."),
                      
                      
                      
                      tags$h3("CalCOFI marine mammal visual survey data"),
                      tags$h5("Marine mammal visual line-transect surveys have been conducted on quarterly CalCOFI cruises since 2004. Visual surveys are 
                              conducted during daylight hours while the ship is in transit between CalCOFI stations. More information about visual
                              survey protocol can be found in Campbell et al. (2015):"),
                      tags$a(href="https://doi.org/10.1016/j.dsr2.2014.10.008","https://doi.org/10.1016/j.dsr2.2014.10.008", style='color:#FFFFFF'),
                      tags$h5("Per-cruise marine mammal visual survey effort is visible by clicking 'Display Visual Effort'. Additionally, sighting group size estimates are visible by selecting a species from the drop-down menu, 
                              where circle size on the map is proportional to group size. Only cetacean sightings are included in this interactive map. By selecting a sighting on the map, more information will pop up about that specific sighting."),
                      
                      tags$h3("CalCOFI marine mammal eDNA data"),
                      tags$h5("The NOAA CalCOFI Genomic Program (NCOG) has collected envrionmental DNA samples (eDNA) since 2014. Here we used metabarcoding assays to 
                              detect cetacean species from water samples collected at 10, 20, or 40 meters. The 'Display eDNA' function will plot eDNA sampling effort as opaque black circles, and eDNA detections as blue flags. By selecting a detection on the map, more information about that specific detection will pop up.  "), 
                      
                      tags$h4("Co-authors"),
                      tags$h5("Michaela Alksne, Lauren Baggett, Julie Dinasquet, Bryce Ellman, 
                              Erin Satterthwaite, Brice Semmens, and Simone Baumann-Pickering "),
                      
                      tags$h4("Funding Sources"),
                      tags$h5("This material is based upon research supported by the Office of Naval Research under Award Number (N00014-22-1-2719)"),
                      tags$h5("Office of Naval Research, US Navy Pacific Fleet"),
                      
                      
             )
  )
)

start_location <- data.frame(tornado_id = data_tornado$ID, 
                             lon = data_tornado$slon,
                             lat = data_tornado$slat)

end_location <- data.frame(tornado_id = data_tornado$ID, 
                             lon = data_tornado$elon,
                             lat = data_tornado$elat)

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
           & data_tornado$yr <= input$years[2])  %>%
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
    # 创建颜色映射
    pal <- colorFactor(palette = mag_to_color, levels = as.factor(unique(obsFilter()$mag)))
    
    leafletProxy("mymap", session) %>%
      clearGroup("paths") %>%  # 清除现有路径
      clearControls()  # 清除现有控件
    
    
    for (i in 1:nrow(obsFilter())) {
      # 获取当前路径的颜色
      path_color <- pal(as.factor(obsFilter()$mag[i]))  # 使用颜色映射来选择颜色
      
      leafletProxy("mymap", session) %>%
        addPolylines(
          lng = c(obsFilter()$slon[i], obsFilter()$elon[i]),
          lat = c(obsFilter()$slat[i], obsFilter()$elat[i]),
          color = path_color,  # 设置颜色
          weight = 5,          # 设置路径宽度
          dashArray = "5, 10", # 设置虚线样式
          group = "paths",     # 归类为 "paths"
        )
    }
    
    # 添加图例
    leafletProxy("mymap", session) %>%
      addLegend("topright", 
                pal = pal, 
                values = as.factor(obsFilter()$mag), 
                group = "paths", 
                title = "Tornado Magnitude",
                opacity = 1)
  })
  
  # observeEvent(input$paths, {
  #   path_data <- obsFilter()
  #   
  #   leafletProxy("mymap", session) %>%
  #     clearGroup("paths")  # 清除现有路径
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
    if (input$starts > 0) {
    leafletProxy("mymap", session) %>% # add a layer to the map
      clearGroup("starts") %>%
      clearGroup("paths") %>% 
      addCircleMarkers( # add circular markers for starts locations
        lng = obsFilter()$slon, obsFilter()$slat,
        color = 'black',
        stroke = TRUE,
        popup = paste("Line:",start_location$tornado_id,
                      "<br>Station:",start_location$tornado_id) %>% # popup with information about line + station
          lapply(htmltools::HTML), # read this html code
        radius = 2,
        weight = 1,
        group = "starts"
      ) 
    }  else {
      leafletProxy("mymap", session) %>% clearGroup("starts")  # Clear markers if unchecked
    }
  })
  
  
  observeEvent(input$ends, { # when the user selects the display startpoint input button
    if (input$ends > 0) {
    leafletProxy("mymap", session) %>% # add a layer to the map
      clearGroup("ends") %>%
      clearGroup("paths") %>% 
      addCircleMarkers( # add circular markers for starts locations
        lng = obsFilter()$elon, lat = obsFilter()$elat,
        color = 'blue',
        stroke = TRUE,
        popup = paste("Line:",start_location$tornado_id,
                      "<br>Station:",start_location$tornado_id) %>% # popup with information about line + station
          lapply(htmltools::HTML), # read this html code
        radius = 2,
        weight = 1,
        group = "ends"
      )
    }  else {
      leafletProxy("mymap", session) %>% clearGroup("ends")  # Clear markers if unchecked
    }
  })
  
  
  observeEvent(input$clearsites, {
    leafletProxy("mymap", session) %>%
      clearGroup("ends") %>%
      clearGroup("starts") %>%
      clearGroup("paths")  # Clear paths as well
  })                                  
}

shinyApp(ui = ui, server = server)