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
      
      # Input: Slider for the number of bins ----
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
      
      # Output: Histogram ----
      verbatimTextOutput("value"),
      plotOutput(outputId = "distPlot", height = "800")
      
    )
  )
)

# Define server logic required to draw a histogram ----
server <- function(input, output) {
  
  # Histogram of the Old Faithful Geyser Data ----
  # with requested number of bins
  # This expression that generates a histogram is wrapped in a call
  # to renderPlot to indicate that:
  #
  # 1. It is "reactive" and therefore should be automatically
  # 2. Its output type is a plot
  output$value <- renderText({ 
    
    if(input$location == "write location") {location <- "Linkoping"}
    else location <- input$location
    location })
    output$distPlot <- renderPlot({
    
    if(input$location == "write location") {location <- "Linkoping"}
    else location <- input$location
    osm <- osmObjects(location, input$Item)
    osm$plot()
    
  })
  
}

shinyApp(ui = ui, server = server)
