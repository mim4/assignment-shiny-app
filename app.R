library(shiny)
library(Stat2Data)
require(summarytools)
data("Diamonds")
# Define UI for application that draws a histogram
ui <- navbarPage("Marta Ilundain",
                 theme = shinytheme("united"),
                 tabPanel("Fourth Assignment: A first Shiny App",
                          fluidPage(
                            titlePanel("How to use this shiny app"),
                            br(),
                            mainPanel(
                                
                                p("In this Shiny App we will describe the main characteristics
                                  of the Diamonds dataset, which is contained in the Stat2Data 
                                  package. In the first tab we can take a look to our data. There, 
                                  we can select how many observations can be displayed by
                                  introducing a number and then clicking the button Update Table.
                                  In the second tab, we can see a summary of our data which has been
                                  generated using the dfSummary function of the summarytools package. 
                                  In this tab, we can also see how our variables are defined.
                                  In the last tab called Plots we can see "),
                                img(src = "diamond.png")
                              )
                            )
                          ),
                
                 tabPanel("Data",
                          fluidPage(
                            titlePanel("Take a look to the data"),
                            br(),
                            sidebarLayout(
                              sidebarPanel(
                                numericInput("obs", "Number of observations to view:", 20,
                                             min = 1, max = nrow(Diamonds)),
                                br(),
                                submitButton("Update Table")
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
                                p("In this dataset we have the following variables:"),
                                br(),
                                p("- Carat: Size of the diamond (in carats)"),
                                p("- Color: Coded as D (most white/bright) through J"),
                                p("- Clarity Coded as IF, VVS1, VVS2, VS1, VS2, SI1, SI2, or SI3"),
                                p("- Depth Depth (as a percentage of diameter)"),
                                p("- PricePerCt Price per carat"),
                                p("- TotalPrice Price for the diamond (in dollars)")
                                
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