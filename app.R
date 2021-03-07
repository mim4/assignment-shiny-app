library(shiny)
library(Stat2Data)
require(summarytools)
data("Diamonds")
# Define UI for application that draws a histogram
ui <- navbarPage("My Shiny App",
                 theme = shinytheme("united"),
                 tabPanel("Dataset",
                          fluidPage(
                            titlePanel("How to use this shiny app"),
                            sidebarLayout(
                              sidebarPanel(
                                p("in this")
                              ),
                              mainPanel(
                                img(src = "diamond.png")
                              )
                            )
                          )
                 ),
                 tabPanel("Data",
                          fluidPage(
                            titlePanel("Take a look to our data"),
                            sidebarLayout(
                              sidebarPanel(
                                numericInput("obs", "Number of observations to view:", 20,
                                             min = 1, max = nrow(Diamonds)),
                                br(),
                                submitButton("Update View")
                              ),
                              mainPanel(
                                  
                                tableOutput("data")
                                ),
                            )
                          ),
                          ),
                 tabPanel("Summary",
                          fluidPage(
                            titlePanel("Main characteristics of the variables"),
                            br(),
                            sidebarLayout(
                              sidebarPanel(
                                p("VARIABLES")
                              ),
                              mainPanel(
                              verbatimTextOutput("summary")
                            ),
                          )
                          )
                 ),
                 tabPanel("Plots"),
                 useShinyjs()
)



# Define server logic required to draw a histogram
server <- function(input, output) {
  
  datasetInput <- reactive(Diamonds)
  
  output$data <- renderTable({
    n <- nrow(Diamonds)
    x <- datasetInput()
    x <- x[sample(seq_len(n), input$obs, replace=FALSE), , drop=FALSE]
    x
  })
  
  output$summary <- renderPrint({
    dfSummary(Diamonds)
  })
}

# Run the application 
shinyApp(ui = ui, server = server)