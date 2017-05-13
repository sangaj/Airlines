#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(leaflet)
library(shiny)
library(dplyr)
library(reshape2)
library(leaflet.minicharts)

ui <- 
  shinyUI(navbarPage("Flights between Europen Continent and Mainland China",tabPanel("Details" , 
  fluidPage(
  
  leafletOutput("mymap"),
  
  hr(),

  fluidRow(
    column(
      
      5, 
      
      h4("Details of Flights per Week"),
      
      selectInput(
        
        inputId = "FROM",
        label = "Select Departure Airport",
        choices = c("All","Beijing","Shanghai","Guangzhou","Chengdu","Xiamen","Hangzhou","Shenzheng","Nanjing","Changsha","Qingdao","Wuhan","Xi'an","Kunming","Chongqing"),
        selected = "All"
        
      ),
      
      selectInput(
        
        inputId = "TO",
        label = "Select Arrival Airport",
        choices = c("Amsterdam","Paris","Frankfurt","Madrid","Zurich","Berlin","Dusseldorf","Munich","London","Barcelona","Brussels","Rome","Milan","Stockholm","Copenhagen"),
        selected = "All"
      
    )
    
  ),
  column(
    
    5,
    
    h5("Press the button to show detail numbers "),
    
    actionButton(
      
      inputId = "show",
      label = "Show Table"
      
    ),
    
    tableOutput("table")
    
  )
)
)
),
tabPanel("About",
         mainPanel(
           h3("Airlines Status between Mainland China and European Continent"),
           h5("The Figure and Table gives the current airlines status between Chinese Mainland and European Continent. You can get the airlines frequency per week by selecting the departue city in China and arrival city in Europe"),
           h6("Data only contains some airports in Europe. It would be extended in the future")
         ))
)
)


server <- function(input, output, session) {
   dir <- "data/airlines.csv"
   data <- read.csv(dir,header=T)
   cast <- dcast(data, TO + longitude + latitude ~ FROM, sum, value.var = "Frequency")
   cast$total <- rowSums(cast[-c(1:3)])
   subcast <- subset(cast, select=c("TO", "longitude","latitude","Beijing","Shanghai","total"))
   subcast$others <- subcast$total-subcast$Beijing-subcast$Shanghai
   colors <- c("#3093e5", "#fcba50", "#a0d9e8")
   output$mymap <- renderLeaflet({
   map <-  addMinicharts(addTiles(leaflet()),
     lng = subcast$longitude, 
     lat = subcast$latitude,
     type = "pie",
     data = subcast[, c("Beijing", "Shanghai","others")], 
     popup = c("Amsterdam Schiphol Airport","Barcelona El Prat Airport", "Berlin Tegel Airport","Brussels Airport","Copenhagen Airport","Dusseldorf Airport","Frankfurt am Main Airport","London Heathrow Airport","Madrid Barajas Airport","Milan Malpensa Airport","Munich Airport","Paris Charles de Gaulle Airport Airport","Rome Fiumicino Airport","Stockholm Arlanda Airport","Zurich Airport"),
     colorPalette = colors, 
     width = 60 * sqrt(subcast$total) / sqrt(max(subcast$total))
   ) %>% 
     addLegend(
       "topright",
       colors = colors, opacity = 1,
       labels = c("Beijing", "Shanghai","Others")
     )
  })
  
   observeEvent(
     
     input$show, {
       
       output$table <- renderTable({
         
         if(input$FROM != "" && input$TO != "") {
           if(input$FROM == "All"){
             filter(data, data$TO == input$TO)[,1:5]
           } else {
             frame1 <- filter(data, data$TO == input$TO)
             filter(frame1, frame1$FROM == input$FROM)[,1:5]
           }
         }
         
       })
       
     }
   )
  
  
  
}

shinyApp(ui = ui, server = server, options = list(height = 1080))
