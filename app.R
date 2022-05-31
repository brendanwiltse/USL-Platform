library(shiny)
library(shinydashboard)
library(shinydashboardPlus)
library(ggplot2)
library(tidyverse)
library(viridis)
library(lubridate)
library(dplyr)
library(RColorBrewer)
library(rLakeAnalyzer)
library(metR)

ui <- dashboardPage(
  header = dashboardHeader(
    title = "Upper Saranac Lake Platform",
    titleWidth = 550,
    leftUi = tagList(
      socialButton(
        href = "https://twitter.com/USLPlatform",
        icon = icon("twitter-square",
                    lib = "font-awesome")
      ),
      socialButton(
        href = "https://www.facebook.com/adkwatershed",
        icon = icon("facebook-official",
                    lib = "font-awesome")
      ),
      socialButton(
        href = "https://www.instagram.com/adkwatershed/?hl=en",
        icon = icon("instagram",
                    lib = "font-awesome")
      )
    )
    ),
  sidebar = dashboardSidebar(
    minified = FALSE,
    uiOutput("logo"),
    sidebarMenu(
      menuItem("Current Conditions", tabName = "current", icon = icon("dashboard")),
      menuItem("Profile Data", tabName = "profile", icon = icon("water")),
      menuItem("Weather Data", tabName = "weather", icon = icon("cloud")),
      menuItem("About", tabName = "about", icon = icon("question-circle"))
    ),
    actionButton(
      inputId = "donate",
      label = "Donate",
      onclick ="window.open('https://ecommunity.paulsmiths.edu/awi-', '_blank')"
    )
  ),
  footer = dashboardFooter(
    left = "Developed by Brendan Wiltse. For more information contact Brendan at bwiltse@paulsmiths.edu."
  ),
  body = dashboardBody(
    tags$head(
      includeHTML("google-analytics.html")
    ),
    tabItems(
      tabItem(tabName = "current",
              h1("Current Conditions"),
              fluidRow(
                infoBoxOutput(
                  width = 9,
                  "currentinfo"
                ),
                
                box(
                  title = "Measurement System",
                  width = 3,
                  selectInput("measurement", '', choices=c(
                    "Imperial" = "imperial", 
                    "Metric" = "metric")
                  )
                )
              ),
              
              fluidRow(
                valueBoxOutput(
                  "airtempBox"
                  ),
                valueBoxOutput(
                  "dewBox"
                ),
                valueBoxOutput(
                  "watertempBox"
                )
                
              ),
              
              fluidRow(
                valueBoxOutput(
                  "heatindexBox"
                ),
                valueBoxOutput(
                  "windchillBox"
                )
                
              ),
              
              fluidRow(
                valueBoxOutput(
                  "windspeedBox"
                ),
                valueBoxOutput(
                  "winddirBox"
                ),
                valueBoxOutput(
                  "windmaxBox"
                )
              ),
              
              fluidRow(
                valueBoxOutput(
                  "rainhourBox"
                ),
                valueBoxOutput(
                  "rain24hourBox"
                ),
                valueBoxOutput(
                  "solarrad"
                )
              ),
              
              fluidRow(
                box(
                  width = 8,
                  plotOutput(
                    "baroplot"
                  )
                ),
                box(
                  width = 4,
                  plotOutput(
                    "tempprofile"
                  )
                  )
                ),
              
              fluidRow(
                uiOutput("uslphoto")
              )
      ),
      
      tabItem(tabName = "profile",
              h1("Profile Data"),
              fluidRow(
                tabBox(
                  title = "",
                  id = "tabset1",
                  width = 12,
                  tabPanel(
                    "Parameter Selection",
                    selectInput("parameter", 'Parameter', choices=c(
                      "Temperature (\u00B0C)" = "temp", 
                      "Dissolved Oxygen (mg/L)" = "ODO", 
                      "Dissolved Oxygen (% Sat)" = "ODO.percent",
                      "Conductivity (\u03BCS/cm @ 25\u00B0C)" = "cond", 
                      "Turbidity (NTU)" = "turbidity",
                      "pH" = "pH", 
                      "Chlorophyll (RFU)" = "chl.rfu", 
                      "Phycocyanin (RFU)"  = "bga.pc.rfu")
                    ),
                    dateRangeInput(
                      "dateRange",
                      label = "Select Date Range",
                      start = "2022-04-16 00:00:00", 
                      end = Sys.Date()+1
                    )
                  ),
                  tabPanel(
                    "Parameter Information",
                    htmlOutput(
                      width = 12,
                      "parinfo"
                    )
                  )
                )
              ),
              
              fluidRow(
                box(title = "Heatmap Plot",
                  width = 12,
                  plotOutput(
                    "plot.heatmap",
                    height = "600px"
                  )
                )
              ),
              
              fluidRow(
                box(
                  title = "Parameter 1",
                  selectInput("parameter1", NULL, choices=c(
                    "Temperature (\u00B0C)" = "temp", 
                    "Dissolved Oxygen (mg/L)" = "ODO", 
                    "Dissolved Oxygen (% Sat)" = "ODO.percent",
                    "Conductivity (\u03BCS/cm @ 25\u00B0C)" = "cond", 
                    "Turbidity (NTU)" = "turbidity",
                    "pH" = "pH", 
                    "Chlorophyll (RFU)" = "chl.rfu", 
                    "Phycocyanin (RFU)"  = "bga.pc.rfu")
                  )
                ),
                
                box(
                  title = "Parameter 2",
                  selectInput("parameter2", NULL, choices=c(
                    "Temperature (\u00B0C)" = "temp", 
                    "Dissolved Oxygen (mg/L)" = "ODO", 
                    "Dissolved Oxygen (% Sat)" = "ODO.percent",
                    "Conductivity (\u03BCS/cm @ 25\u00B0C)" = "cond", 
                    "Turbidity (NTU)" = "turbidity",
                    "pH" = "pH", 
                    "Chlorophyll (RFU)" = "chl.rfu", 
                    "Phycocyanin (RFU)"  = "bga.pc.rfu")
                  )
                )
              ),
              
              fluidRow(
                box(title = "Profile 1",
                  plotOutput(
                    "plot.profiles1"
                  )
                ),
                box(title = "Profile 2",
                  plotOutput(
                    "plot.profiles2"
                  )
                )
              )
      ),
      
      tabItem(tabName = "weather",
              h1("Weather Data"),
              fluidRow(
                box(
                  title = "Select Date Range",
                  dateRangeInput(
                    label = NULL,
                    "dateWRange",
                    start = Sys.Date()-6, 
                    end = Sys.Date()+1
                  )
                ),
                box(
                  title = "Measurement System",
                  width = 6,
                  selectInput("wmeasurement", '', choices=c(
                    "Imperial" = "imperial", 
                    "Metric" = "metric")
                  )
                )
              ),
              
              fluidRow(
                box(
                  width = 12,
                  plotOutput(
                    "wp.temp",
                    height = "300px"
                  )
                )
              ),
              
              fluidRow(
                box(
                  width = 12,
                  plotOutput(
                    "wp.precip",
                    height = "300px"
                  )
                )
              ),
              
              fluidRow(
                box(
                  width = 12,
                  plotOutput(
                    "wp.wspeed",
                    height = "300px"
                  )
                )
              ),
              
              fluidRow(
                box(
                  width = 12,
                  plotOutput(
                    "wp.wdir",
                    height = "300px"
                  )
                )
              ),
              
              fluidRow(
                box(
                  width = 12,
                  plotOutput(
                    "wp.rad",
                    height = "300px"
                  )
                )
              )
      ),
      
      tabItem(tabName = "about",
              h1("About"),
              fluidRow(
                column(6,
                       box(width = 12,
                          title = "General Information",
                          "The Paul Smith's College Adirondack Watershed Institute and the Upper Saranac Foundation installed an environmental monitoring platform at the South Basin of Upper Saranac Lake, NY, in May of 2017. Upper Saranac Lake is located in the northern Adirondacks (red star on map) at an elevation of 1,572 feet (479 meters), and with 47 miles of shoreline and a surface area of 7.6 square miles, this lake is among the largest in the interior highlands of the Adirondacks. The platform is an autonomous in-lake station that collects key physical, chemical, and biological data to enhance our understanding of lake ecosystems in support of lake and watershed science and management. Weather and limnological data collected by the platform are transmitted in near real-time to Paul Smith's College where data is displayed on this website. Historical data is freely available to the wider scientific community by request to the Paul Smith's College Adirondack Watershed Institute. Support for the environmental monitoring platform comes from the National Fish and Wildlife Foundation and the Upper Saranac Foundation."
                        ),
                       box(
                         width = 12,
                         title = "Equipment and Logging Interval",
                         tags$body(
                           strong("Equipment"),
                           tags$ul(
                             tags$li("YSI EXO 2 Sonde:"),
                             tags$ul(
                               tags$li("Temperature/Conductivity"),
                               tags$li("Optical Dissolved OXygen"),
                               tags$li("pH"),
                               tags$li("Turbidity"),
                               tags$li("Total Algae (Chlorophyll + Phycocyanin)"),
                               tags$li("Automated Wiper")),
                              tags$li("Rotronic Temperature/Humidity (HC2-S3) with Radiation Shield"),
                              tags$li("Young Instruments Precipitation Gauge (50203)"),
                              tags$li("Young Instruments Anemometer (05106)"),
                              tags$li("LiCor Solar Radiation (Li-200/R)")
                           ),
                           strong("Logging Intervals"),
                           tags$ul(
                             tags$li("Meterological Data: 15 minutes"),
                             tags$li("Surface Water Data: 1 hour"),
                             tags$li("Water Column Profiles: 4 hours")
                           )
                         )
                       ),
                       box(
                         width = 12,
                         title = "Partners and Funders",
                         div(a(href = "https://www.usfoundation.net", img(src = "USFoundation.jpg", width = "80%")), style = "text-align: center;"),
                         div(a(href = "https://www.adkwatershed.org", img(src = "AWI LOGO2.png", width = "80%")), style = "text-align: center;"),
                         div(a(href = "https://www.nfwf.org", img(src = "nfwf.png", width = "80%")), style = "text-align: center;")
                       )
                       ),
                column(6,
                  box(width = 12,
                      title = "Site Location",
                      tags$body(
                        tags$ul("Deep hole at South Basin"),
                        tags$ul("Latitude: 44.2535\u00B0 N"),
                        tags$ul("Longitude: 74.3257\u00B0 W"),
                        tags$ul("Water Depth: 26m"),
                        tags$ul("Lake Surface Elevation: 479m"),
                      img(src = "location.jpg", width = "100%")
                      )
                  )
                )
              ),
            fluidRow(
              column(12,
                box(
                  width = 12,
                  title = "Corrections and Calculations",
                  tags$body(
                    strong("Barometric Pressure Correction"),
                    p("Reported air pressure is corrected to sea-level pressure with the following equation:"),
                    p(
                      div(img(src = "pressure equation.png", width = "300px"), style = "text-align: center;")
                    ),
                    p("where:"),
                    p(tags$ul("   P = air pressure reported by buoy in mbars"),
                      tags$ul("   h = altitude above sea-level in meters"),
                      tags$ul("   T = air temperature reported by buoy in Centrigrade"),
                      tags$ul("   P\u2080 = sea-level pressure in mbars"))
                    
                  )
                )
              )
            )
      )
    )
  )
)

server <- function(input, output) {
  
  output$logo <- renderUI({
    tags$img(
      src = "AWI LOGO.png", 
      alt = "AWI Logo", 
      height = "60px",
      style = "vertical-align:middle;horizontal-align:middle;margin:7px 7px 7px 7px")
  })
  
  #Load USL data
  usl <- read_csv("https://dl.dropboxusercontent.com/s/mylhy2np4o8amrx/usl.csv?dl=0")
  
  usl_surf <- read_csv("https://dl.dropboxusercontent.com/s/d1ylz95glszlglz/usl_surf.csv?dl=0")
  
  usl_met_avg <- read_csv("https://dl.dropboxusercontent.com/s/khy8no6qcd5zs7g/usl_met_avg.csv?dl=0")
  
  usl_wind <- read_csv("https://dl.dropboxusercontent.com/s/5gli9ype4err7td/usl_wind.csv?dl=0")
  
  usl_windmax <- read_csv("https://dl.dropboxusercontent.com/s/prlph0kc8lxwncx/usl_windmax.csv?dl=0")
  
  usl_rain <-read_csv("https://dl.dropboxusercontent.com/s/kdcsrzas7qgizkg/usl_rain.csv?dl=0")
  
  #Load data frame of column names
  usl_names <- read_csv("parameter names.csv")
  
  #Subset data based on input
  usl_sub.heatmap <- reactive({
    filter(usl, date.time >= as_datetime(input$dateRange[1]) & date.time <= as_datetime(input$dateRange[2]) + 2)
  })
  
  #Subset data for most recent profile
  usl_sub.profiles <- filter(usl, date.time == max(usl$date.time))
  
  #Calculate metadepths for plotting
  t.d <- thermo.depth(usl_sub.profiles$temp, usl_sub.profiles$depth, seasonal = T)
  
  #Plot data
  
  output$plot.heatmap <- renderPlot({
    ggplot(usl_sub.heatmap(), aes(x = as_datetime(date.time), y = depth)) +
      geom_contour_fill(aes_string(z = input$parameter), 
                        bins = filter(usl_names, parameter == input$parameter)$bin) +
      scale_y_reverse(expand = expansion(mult = c(0,0)), limits =c(25,0)) +
      scale_x_datetime(expand = expansion(mult = c(0,0))) +
      scale_fill_viridis(option = "viridis",
                         guide = guide_colorbar(direction = "horizontal", 
                                                label.position = "bottom",
                                                title = filter(usl_names, parameter == input$parameter)[1],
                                                title.position = "top",
                                                barwidth = 20,
                                                draw.llim = T)) +
      labs(y = "Depth (m)",
           x = "Date",
           fill = "") +
      theme_classic(base_size = 18) +
      theme(legend.position = "bottom",
            legend.background = element_blank(),
            plot.margin = unit(c(1.1,1,1,1),"cm"))
  })
  
  output$plot.profiles1 <- renderPlot({
    ggplot(usl_sub.profiles, aes(y = depth)) +
      geom_hline(aes(yintercept = t.d, linetype = "Thermocline"), size = 1.05, col = "red") +
      geom_point(aes_string(x = input$parameter1), size = 4) +
      geom_path(aes_string(x = input$parameter1), size = 1.05) +
      scale_linetype_manual(name = "",
                           values = c(1),
                           guide = guide_legend(override.aes = list(color = c("red")))) +
      scale_y_reverse() +
      labs(
        y = "Depth (m)",
        x = filter(usl_names, parameter == input$parameter1)[1],
        subtitle = format(as_datetime(usl_sub.profiles$date.time), format = "%A, %B %d, %Y, %I:%M %p")
      ) +
      theme_classic(base_size = 18) +
      theme(legend.position = c(0.8, 0.2))
  })
  
  output$plot.profiles2 <- renderPlot({
    ggplot(usl_sub.profiles, aes(y = depth)) +
      geom_hline(aes(yintercept = t.d, linetype = "Thermocline"), size = 1.05, col = "red") +
      geom_point(aes_string(x = input$parameter2), size = 4) +
      geom_path(aes_string(x = input$parameter2), size = 1.05) +
      scale_linetype_manual(name = "",
                            values = c(1),
                            guide = guide_legend(override.aes = list(color = c("red")))) +
      scale_y_reverse() +
      labs(
        y = "Depth (m)",
        x = filter(usl_names, parameter == input$parameter2)[1],
        subtitle = format(as_datetime(usl_sub.profiles$date.time), format = "%A, %B %d, %Y, %I:%M %p")
      ) +
      theme_classic(base_size = 18) +
      theme(legend.position = c(0.8, 0.2))
  })
  
  output$parinfo <- renderUI({
      div(as.character(filter(usl_names, parameter == input$parameter)[4]))
  })
  
  #Current conditions page
  output$currentinfo <- renderInfoBox({
    div(h4("The Upper Saranac Lake Environmental Monitoring Platform is maintained by the", a(href = "https://www.adkwatershed.org", "Paul Smith's College Adirondack Watershed Institute"), "in collaboration and with support from the", a(href = "https://usfoundation.net", "Upper Saranac Foundation.")," Visit the about tab to learn more.", br(strong("Meterological data last updated on", format(as_datetime(last(usl_met_avg$date.time)), format = "%A, %B %d, %Y, %I:%M %p")))))
  })
  
  output$airtempBox <- renderValueBox({
    if(input$measurement == "metric"){
        valueBox(
          format(round(last(usl_met_avg$temp),1), nsmall = 1),
          "Air Temperature (°C)",
          icon = icon("thermometer-half"),
          color = "light-blue"
        )
    } else{
        valueBox(
          format(round((last(usl_met_avg$temp)*9/5)+32, 1), nsmall =1),
          "Air Temperature (°F)",
          icon = icon("thermometer-half"),
          color = "light-blue"
        )
    }
  })
  
  output$dewBox <- renderValueBox({
    if(input$measurement == "metric"){
      valueBox(
        format(round(last(usl_met_avg$dew),1), nsmall = 1),
        "Dew Point (°C)",
        icon = icon("thermometer-half"),
        color = "light-blue"
      )
    } else{
      valueBox(
        format(round((last(usl_met_avg$dew)*9/5)+32, 1), nsmall =1),
        "Dew Point (°F)",
        icon = icon("thermometer-half"),
        color = "light-blue"
      )
    }
  })
  
  output$heatindexBox <- renderValueBox({
    if(input$measurement == "metric"){
      valueBox(
        format(round(last(usl_met_avg$heatindex),1), nsmall = 1),
        "Heat Index (°C)",
        icon = icon("thermometer-half"),
        color = "light-blue"
      )
    } else{
      valueBox(
        format(round((last(usl_met_avg$heatindex)*9/5)+32, 1), nsmall =1),
        "Heat Index (°F)",
        icon = icon("thermometer-half"),
        color = "light-blue"
      )
    }
  })
  
  output$windchillBox <- renderValueBox({
    if(input$measurement == "metric"){
      valueBox(
        format(round(last(usl_met_avg$windchill),1), nsmall = 1),
        "Wind Chill (°C)",
        icon = icon("thermometer-half"),
        color = "light-blue"
      )
    } else{
      valueBox(
        format(round((last(usl_met_avg$windchill)*9/5)+32, 1), nsmall =1),
        "Wind Chill (°F)",
        icon = icon("thermometer-half"),
        color = "light-blue"
      )
    }
  })
  
  output$watertempBox <- renderValueBox({
    if(input$measurement == "metric"){
      valueBox(
        format(round(last(usl_surf$temp),1), nsmall = 1),
        "Water Temperature (°C)",
        icon = icon("thermometer-half"),
        color = "light-blue"
      )
    } else{
      valueBox(
        format(round((last(usl_surf$temp)*9/5)+32, 1), nsmall =1),
        "Water Temperature (°F)",
        icon = icon("thermometer-half"),
        color = "light-blue"
      )
    }
  })
  
  output$windspeedBox <- renderValueBox({
    if(input$measurement == "metric"){
      valueBox(
        format(round(last(usl_wind$wspeed),1), nsmall = 1),
        "Wind Speed (m/s)",
        icon = icon("wind"),
        color = "light-blue"
      )
    } else{
      valueBox(
        format(round(last(usl_wind$wspeed)*2.237, 1), nsmall =1),
        "Wind Speed (mph)",
        icon = icon("wind"),
        color = "light-blue"
      )
    }
  })
  
  output$winddirBox <- renderValueBox({
      valueBox(
        last(usl_wind$wdir.card),
        "Wind Direction",
        icon = icon("location-arrow"),
        color = "light-blue"
      )
  })
  
  output$windmaxBox <- renderValueBox({
    if(input$measurement == "metric"){
      valueBox(
        format(round(last(usl_windmax$wmax),1), nsmall = 1),
        "Wind Gust (m/s)",
        icon = icon("wind"),
        color = "light-blue"
      )
    } else{
      valueBox(
        format(round(last(usl_windmax$wmax)*2.237, 1), nsmall =1),
        "Wind Gust (mph)",
        icon = icon("wind"),
        color = "light-blue"
      )
    }
  })
  
  output$rainhourBox <- renderValueBox({
    if(input$measurement == "metric"){
      valueBox(
        round(sum(filter(usl_rain, (as_datetime(usl_rain$date.time) + 14400) >= as.POSIXlt(Sys.time(),          tz = "UTC") - 3600)$tot), digits = 2),
        "Rain Total Last Hour (mm)",
        icon = icon("cloud-rain"),
        color = "light-blue"
      )
    } else{
      valueBox(
        round(sum(filter(usl_rain, (as_datetime(usl_rain$date.time) + 14400) >= as.POSIXlt(Sys.time(),          tz = "UTC") - 3600)$tot)/25.4, digits = 2),
        "Rain Total Last Hour (in)",
        icon = icon("cloud-rain"),
        color = "light-blue"
      )
    }
  })
  
  output$rain24hourBox <- renderValueBox({
    if(input$measurement == "metric"){
      valueBox(
        round(sum(filter(usl_rain, (as_datetime(usl_rain$date.time) + 14400) >= as.POSIXlt(Sys.time(),          tz = "UTC") - 86400)$tot), digits = 2),
        "Rain Total Last 24 Hours (mm)",
        icon = icon("cloud-rain"),
        color = "light-blue"
      )
    } else{
      valueBox(
        round(sum(filter(usl_rain, (as_datetime(usl_rain$date.time) + 14400) >= as.POSIXlt(Sys.time(),          tz = "UTC") - 86400)$tot)/25.4, digits = 2),
        "Rain Total Last 24 Hours (in)",
        icon = icon("cloud-rain"),
        color = "light-blue"
      )
    }
  })
  
  output$humBox <- renderValueBox({
    valueBox(
      round(last(usl_met_avg$hum),0),
      "Relative Humidity (%)",
      icon = icon("tint"),
      color = "light-blue"
    )
  })
  
  output$solarrad <- renderValueBox({
    valueBox(
      round(last(usl_met_avg$rad),0),
      "Solar Radiation (W/m\u00B2)",
      icon = icon("sun"),
      color = "light-blue"
    )
  })
  
  output$tempprofile <- renderPlot({
    if(input$measurement == "metric"){
    ggplot(usl_sub.profiles, aes(y = depth)) +
      geom_hline(aes(yintercept = t.d, linetype = "Thermocline"), size = 1.05, col = "red") +
      geom_point(aes(x = temp), size = 3) +
      geom_path(aes(x = temp), size = 1.02) +
      scale_linetype_manual(name = "",
                            values = c(1),
                            guide = guide_legend(override.aes = list(color = c("red")))) +
      scale_y_reverse() +
      labs(
        y = "Depth (m)",
        x = filter(usl_names, parameter == "temp")[1],
        subtitle = format(as_datetime(usl_sub.profiles$date.time), format = "%I:%M %p")
      ) +
      theme_classic(base_size = 16) +
      theme(legend.position = c(0.8, 0.2))
    } else {
      ggplot(usl_sub.profiles, aes(y = depth)) +
        geom_hline(aes(yintercept = t.d, linetype = "Thermocline"), size = 1.05, col = "red") +
        geom_point(aes(x = (temp*9/5)+32), size = 3) +
        geom_path(aes(x = (temp*9/5)+32), size = 1.02) +
        scale_linetype_manual(name = "",
                              values = c(1),
                              guide = guide_legend(override.aes = list(color = c("red")))) +
        scale_y_reverse() +
        labs(
          y = "Depth (m)",
          x = "Temperature (°F)",
          subtitle = format(as_datetime(usl_sub.profiles$date.time), format = "%I:%M %p")
        ) +
        theme_classic(base_size = 16) +
        theme(legend.position = c(0.8, 0.2))
  }
  })
  
  output$baroplot <- renderPlot({
    if(input$measurement == "metric"){
      ggplot(filter(usl_met_avg, (as_datetime(usl_met_avg$date.time) + 14400) >= as.POSIXlt(Sys.time(),       tz = "UTC") - 86400), aes(y = baro.corr/1.333)) +
        geom_point(aes(x = as_datetime(date.time)), size = 3) +
        geom_path(aes(x = as_datetime(date.time)), size = 1.02) +
        labs(
          y = "Barometric Pressure (mmHg)",
          x = "Time"
        ) +
        theme_classic(base_size = 16)
    } else {
      ggplot(filter(usl_met_avg, (as_datetime(usl_met_avg$date.time) + 14400) >= as.POSIXlt(Sys.time(),       tz = "UTC") - 86400), aes(y = baro.corr/33.864)) +
        geom_point(aes(x = as_datetime(date.time)), size = 3) +
        geom_path(aes(x = as_datetime(date.time)), size = 1.02) +
        labs(
          y = "Barometric Pressure (inHg)",
          x = "Time"
        ) +
        theme_classic(base_size = 16)
    }
  })
  
  output$uslphoto <- renderUI({
    tags$img(
      src = "https://dl.dropbox.com/s/cv9awr9gw4aqy29/USL%20Profiler%20Camera.JPG?dl=0", 
      alt = "USL Camera Photo",
      width = "96%",
      style = "vertical-align:middle;horizontal-align:middle;margin:15px 15px 15px 15px")
  })
  
 
  
  #Code for weather data page
  
  usl_met_avg.wp <- reactive({
    filter(usl_met_avg, date.time >= as_datetime(input$dateWRange[1]) & date.time <= as_datetime(input$dateWRange[2]))
  })
  
  usl_rain.wp <- reactive({
    filter(usl_rain, date.time >= as_datetime(input$dateWRange[1]) & date.time <= as_datetime(input$dateWRange[2]))
  })
  
  usl_wind.wp <- reactive({
    filter(usl_wind, date.time >= as_datetime(input$dateWRange[1]) & date.time <= as_datetime(input$dateWRange[2]))
  })
  
  usl_windmax.wp <- reactive({
    filter(usl_windmax, date.time >= as_datetime(input$dateWRange[1]) & date.time <= as_datetime(input$dateWRange[2]))
  })
  
  output$wp.temp <- renderPlot({
    if(input$wmeasurement == "metric"){
      ggplot() +
        geom_path(usl_met_avg.wp(), mapping = aes(x = as_datetime(date.time), y = temp, color =                 "firebrick"),
                  size = 1.05) +
        geom_path(usl_met_avg.wp(), mapping = aes(x = as_datetime(date.time), y = dew, color =                  "steelblue4"),
                  size = 1.05) +
        scale_x_datetime(expand = expansion(mult = c(0.01,0.01)),
                         date_labels = "%b %d") +
        scale_color_identity(name = "",
                             breaks = c("firebrick", "steelblue4"),
                             labels = c("Air Temperature", "Dew Point"),
                             guide = guide_legend(override.aes = list(linetype = c(1,1)))) +
        labs(
          y = "Temperature (°C)",
          x = "Date"
        ) +
        theme_classic(base_size = 16) +
        theme(legend.position = "top")
    } else {
      ggplot() +
        geom_path(usl_met_avg.wp(), mapping = aes(x = as_datetime(date.time), y = (temp*9/5)+32, color =         "firebrick"),
                  size = 1.05) +
        geom_path(usl_met_avg.wp(), mapping = aes(x = as_datetime(date.time), y = (dew*9/5)+32, color =         "steelblue4"),
                  size = 1.05) +
        scale_x_datetime(expand = expansion(mult = c(0.01,0.01)),
                         date_labels = "%b %d") +
        scale_color_identity(name = "",
                             breaks = c("firebrick", "steelblue4"),
                             labels = c("Air Temperature", "Dew Point"),
                             guide = guide_legend(override.aes = list(linetype = c(1,1)))) +
        labs(
          y = "Temperature (°F)",
          x = "Date"
        ) +
        theme_classic(base_size = 16) +
        theme(legend.position = "top")
    }
  })
  
  output$wp.rad <- renderPlot({
    ggplot() +
      geom_path(usl_met_avg.wp(), mapping = aes(x = as_datetime(date.time), y = rad),
                size = 1.05,
                color = "goldenrod2") +
      scale_x_datetime(expand = expansion(mult = c(0.01,0.01)),
                       date_labels = "%b %d") +
      labs(
        y = expression("Solar Radiation (W/m"^2*")"),
        x = "Date"
      ) +
      theme_classic(base_size = 16)
  })
  
  output$wp.precip <- renderPlot({
    if(input$wmeasurement == "metric"){
      ggplot(usl_rain.wp(), aes(x = as_datetime(date.time), y = tot)) +
        geom_col(width = 2000,
                 fill = "royalblue4") +
        scale_x_datetime(expand = expansion(mult = c(0.01,0.01)),
                         date_labels = "%b %d") +
        labs(
          y = "Rain (mm)",
          x = "Date"
        ) +
        theme_classic(base_size = 16)
    } else {
      ggplot(usl_rain.wp(), aes(x = as_datetime(date.time), y = tot/25.4)) +
        geom_col(width = 2000,
                 fill = "royalblue4") +
        scale_x_datetime(expand = expansion(mult = c(0.01,0.01)),
                         date_labels = "%b %d") +
        labs(
          y = "Rain (in)",
          x = "Date"
        ) +
        theme_classic(base_size = 16)
    }
  })
  
  output$wp.wspeed <- renderPlot({
    if(input$wmeasurement == "metric"){
      ggplot() +
        geom_point(usl_windmax.wp(), mapping = aes(x = as_datetime(date.time), y = wmax, color =                "orange2")) +
        geom_path(usl_wind.wp(), mapping = aes(x = as_datetime(date.time), y = wspeed, color =                  "navyblue"),
                  size = 1.05
                  ) +
        scale_x_datetime(expand = expansion(mult = c(0.01,0.01)),
                         date_labels = "%b %d") +
        scale_color_identity(name = "",
                             breaks = c("orange2", "navyblue"),
                             labels = c("Wind Gust", "Wind Speed"),
                             guide = guide_legend(override.aes = list(linetype = c(0,1),
                                                                      shape = c(16, NA)))) +
        labs(
          y = "Wind Speed (m/s)",
          x = "Date"
        ) +
        theme_classic(base_size = 16) +
        theme(legend.position = "top")
    } else {
      ggplot() +
        geom_point(usl_windmax.wp(), mapping = aes(x = as_datetime(date.time), y = wmax*2.27, color = "orange2")) +
        geom_path(usl_wind.wp(), mapping = aes(x = as_datetime(date.time), y = wspeed*2.237, color = "navyblue"),
                  size = 1.05
        ) +
        scale_x_datetime(expand = expansion(mult = c(0.01,0.01)),
                         date_labels = "%b %d") +
        scale_color_identity(name = "",
                             breaks = c("orange2", "navyblue"),
                             labels = c("Wind Gust", "Wind Speed"),
                             guide = guide_legend(override.aes = list(linetype = c(0,1),
                                                                      shape = c(16, NA)))) +
        labs(
          y = "Wind Speed (mph)",
          x = "Date"
        ) +
        theme_classic(base_size = 16) +
        theme(legend.position = "top")
    }
  })
  
  output$wp.wdir <- renderPlot({
    ggplot(usl_wind.wp(), mapping = aes(x = as_datetime(date.time), y = wdir)) +
      geom_point(color = "navyblue") +
      scale_x_datetime(expand = expansion(mult = c(0.01,0.01)),
                       date_labels = "%b %d") +
      labs(
        y = "Wind Direction (Degrees)",
        x = "Date"
      ) +
      theme_classic(base_size = 16)
  })
  
}

shinyApp(ui = ui, server = server)