library(shiny)
library(htmlwidgets)
library(pivottabler)
library(leaflet)
library(RColorBrewer)
library(scales)
library(lattice)
library(dplyr)
library(DT)
library(rsconnect)
library(tidyr)
library(shinythemes)
library(ggvis)


# df <- read.csv('./clean_police_data.csv')
# saveRDS(df, "clean_police_data.rds")
# df <- readRDS(file = "./clean_police_data.rds")


df <- read.csv('./street_coords3.csv')
saveRDS(df, "street_coords3.rds")
df <- readRDS(file = "./street_coords3.rds")


df$date <- as.Date(df$date, "%Y_%m_%d")


keep <- c('address',
        'age',
        'agency',
        'charge',
        'city',
        'date',
        'race',
        'sex',
        'lat',
        'long')

df <- df[keep]



ui <- navbarPage("Williamsburg Police Database", id = 'nav', theme = shinytheme("flatly"),

                 navbarMenu('About',
                   
                           tabPanel("About",
                                    #mainPanel(
                                      h2("What does crime look like in Williamsburg?", align = "center"),

                                      
                                      h5("Every week, The Flat Hat newspaper prints its Police Beat, which highlights incidents and arrests that occur in Williamsburg and around the College of William and Mary’s campus. This year, The Flat Hat created an online database showcasing data since Jan 1. 2015 to provide a tool that analyzes these incidents in aggregate. The efforts to create a database began as a way to monitor crime in the greater Williamsburg area, in addition to monitoring the charges recorded by the Williamsburg Police Department.", align = "center"),
                                      
                                      h5("This database is updated weekly and will replace The Flat Hat’s weekly print police beat. All of the data is provided through open records requests of the Williamsburg Police Department. Read our methodology. Visit our “Data Explorer” tab to see and filter the data. Look at our “Map Explorer” tab to plot the data on a map.", align = "center"),
                                      
                                      h5("Database last updated: March 16, 2019", align = 'center')
                                    
                                    #)
                                    
                           ),
                           
                           tabPanel('Methodology',
                                    
                                    h3('Data Collection and Analysis', align = "left"),
                                    
                                    p("In 2019, The Flat Hat began tracking incidents and arrests recorded by the Williamsburg Police Department since Jan. 1, 2015 in its Police Beat Database.", align = "left"),

                                    p("The Flat Hat filed an open records request in order to obtain the data up to March 16. The data includes the charge, date and street location where each incident occurred. The database also provides information on the race, sex and age of the victim when provided.", align = "left"),
                                      
                                    p("The Flat Hat cleaned the data provided in these requests by removing all citations occurring in states outside of Virginia. Each entry was minimally cleaned for spelling errors and variations in abbreviations (for example, Road vs. Rd.). The Flat Hat also removed the case_id and sysid variables from the data. For sexual assault crimes and related charges, The Flat Hat removed information about the location where the incident occurred to protect the anonymity of possible victim(s). Missing data entries are denoted with “NaN” values and appear as blanks in the database, except for the age variable. If an age was missing, it was denoted with a -1 value.", align = "left"),
                                      
                                    p("To create the mapping tool in the “Map Explorer” tab, The Flat Hat geocoded each data point according to the street provided. If the data entry included two street address locations, the first street was selected to visualize the data point. Sexual assault crimes and related charges were not geocoded in order to protect the anonymity of possible victim(s), which follows with the Williamsburg Police Department’s original methodology.", align = "left"),
                                    
                           
                                    h3("Data Dictionary", align = "left"),
                                    
                                    p("The race variable in the dataset contains the following values: A - Asian or Pacific Islander, B - Black, I - American Indian or Alaskan Native, U - Unknown, and W - White. The sex variable is denoted by male, female, and unknown values."),

                                    p("According to the Williamsburg Police Department, the following citation charges are defined as:"),
                                      
                                    p("Found Property: Property that is found by a citizen or officer."),
                                      
                                    p("Mental Subject: A person in need of mental health services such as Temporary Detention Order or Emergency Custody Order."),
                                      
                                    p("Officer Information: Information reported that does not meet the definition of a crime, but needs to be documented (example, trespass warnings given). Information documented for a referral to another agency such as Human Services, other Law Enforcement agencies, etc."),
                                      
                                    p("Suspicious: Unusual circumstances typically pertaining to a person, vehicle or incident. Something reported that is perceived to be out of the norm.", align = "left")
                                    
                                
                 
                 
                 )),
                 
                 navbarMenu("Explore the Data",
                            
                 tabPanel("Data Explorer",
                          
                          fluidRow(
                            column(3, #h3("Filter Locations"), 
                                   selectInput("address", "Street Name", choices = c("All", unique(as.character(df$address)))),
                                   selectInput("charge", "Charge", choices = c("All", unique(as.character(df$charge))))
                            ),
                            column(3, #h3("Filter Personal Information"), 
                                   selectInput("race", "Race", choices = c("All", unique(as.character(df$race)))),
                                   selectInput("sex", "Sex", choices = c("All", unique(as.character(df$sex))))
                            ),
                            
                            column(3, #h3('Filter Date of Crime'),
                                   sliderInput("age", "Age", -1, 99, value = c(-1,99), step = 1),
                                   
                                   dateRangeInput('dateRange', 'Date',
                                                  start = ('2015-01-01'), end = ('2019-03-18'),
                                                  min = ('2015-01-01'), max = ('2019-03-18'),
                                                  separator = " - ", format = "dd/mm/yy",
                                                  startview = 'year', language = 'en', weekstart = 1
                                   )
                            ),
                            
                            hr(), 
                            DT::dataTableOutput("police_table")
                          )),
                 
                 tabPanel("Map Explorer",
                          
                          
                          leafletOutput('map_explorer', width = 1400, height = 900),
                          
                          absolutePanel(id = 'controls', 
                                        class = "panel panel-default",
                                        fixed = TRUE,
                                        draggable = TRUE,
                                        top = 60,
                                        left = 'auto',
                                        right = 20,
                                        bottom = 'auto',
                                        width = 340,
                                        height = 'auto',
                                        
                                        h2("Map Explorer"),
                                        
                                        h6("Click on a cluster to expand it and view the data. Interactive features will be added soon!")
                                        
                          ))
                 
                 ))








####SERVER####
server <- function(input, output, session) {
  
  filterData <- reactive({
        
        if (input$address != "All") {
          df <- df[df$address == input$address,]
        } else if (input$address == "All") {
          df <- df
        }
        if (input$charge != "All") {
          df <- df[df$charge == input$charge,]
        }
        if (input$sex != "All") {
          df <- df[df$sex == input$sex,]
        }
        if (input$race != "All") {
          df <- df[df$race == input$race,]
        }
        if (input$age[1] != 0 | input$age[2] != 99) {
          df <- df[df$age >= input$age[1] & df$age <= input$age[2],]
        }
      
    df <- df[df$age >= input$age[1] & df$age <= input$age[2],]
    df <- df[df$date >= input$dateRange[1] & df$date <= input$dateRange[2],]
    
      
  })
  
  
  output$police_table <- DT::renderDataTable(DT::datatable(data = filterData(), options = list(autoWidth = FALSE)))

  
  output$map_explorer <- renderLeaflet({
    leaflet() %>%
      addTiles(
        urlTemplate = "//{s}.tiles.mapbox.com/v3/jcheng.map-5ebohr46/{z}/{x}/{y}.png",
        attribution = 'Maps by <a href="http://www.mapbox.com/">Mapbox</a>'
      ) %>%
      addMarkers(data = df, ~long, ~lat, clusterOptions = markerClusterOptions(),
                 popup = ~paste(tags$b("Address:"), df[,1], "<br>",
                                tags$b("Age:"), df[,2], "<br>",
                                tags$b("Agency:"), df[,3], "<br>",
                                tags$b("Charge:"), df[,4], "<br>",
                                tags$b("City:"), df[,5], "<br>",
                                tags$b("Date:"), df[,6], "<br>",
                                tags$b("Race:"), df[,7], "<br>",
                                tags$b("Sex:"), df[,8])) %>%
      setView(lat = 37.2707, lng = -76.7075, zoom = 15)
    #addCircles(data = df, ~long, ~lat)
  })
  
  
  
  # observe({
  #   
  #   
  #   
  # 
  #   if (input$mcharge != "All") {
  #     df <- df[df$charge == as.character(input$mcharge),]
  #   }
  #   if (input$msex != "All") {
  #     df <- df[df$sex == input$msex,]
  #   }
  #   #if (input$mrace != "All") {
  #     #df <- df[df$race == input$mrace,]
  #   #}
  # 
  #   
  #   #df <- df[df$age >= input$mage[1] & df$age <= input$mage[2],]
  #   #df <- df[df$date >= input$mdateRange[1] & df$date <= input$mdateRange[2],]
  # 
  #   
  #   #charge_filter <- input$mcharge
  #   #df <- df[df$charge == as.character(charge_filter),]
  #   
  #   leafletProxy("map_explorer", data = df) %>%
  #        clearShapes() %>%
  #        addCircles(~long, ~lat)
  # })
  
  

  
  
}

shinyApp(ui = ui, server = server)


