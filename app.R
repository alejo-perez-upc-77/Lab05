library(shiny)
library(Lab05)

# Define UI for app that draws a histogram ----
ui <- fluidPage(
  
  # App title ----
  titlePanel("Lab05 project - OSM"),
  
  # Sidebar layout with input and output definitions ----
  sidebarLayout(
    
    # Sidebar panel for inputs ----
    sidebarPanel(
      
      # Input: text input and multiple selection input ----
    textInput("location", "Location", "write location"),
    
    selectInput("Item", "What you look for:",
                c("ATM" = "atm",
                  "Pharmacy" = "pharmacy",
                  "Hospital" = "hospital",
                  "Supermarket" = "supermarket",
                  "Gas station" = "fuel")),
    submitButton("Update View", icon("refresh"))
    ),
    
    # Main panel for displaying outputs ----
    mainPanel(
      
      # Output: text and plot ----
      verbatimTextOutput("value"),
      plotOutput(outputId = "distPlot", height = "800")
      
    )
  )
)

# Define server logic required to plot ----
server <- function(input, output) {
  
#text
  output$value <- renderText({ 
    
# select main location - linkoping
    if(input$location == "write location") {location <- "Linkoping"}
    else location <- input$location
    location })
  
# plot
    output$distPlot <- renderPlot({
    if(input$location == "write location") {location <- "Linkoping"}
    else location <- input$location
    osm <- osmObjects(location, input$Item)
    osm$plot()
    
  })
  
}

shinyApp(ui = ui, server = server)
