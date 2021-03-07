library(shiny)
library(Stat2Data)
require(summarytools)
require(shinyjs)
require(shinythemes)
library(ggplot2)
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
                                  package. This dataset shows the price and characteristics for 
                                  a sample of 351 diamonds"),
                                p("In the first tab we can take a look to our data. There, 
                                  we can select how many observations can be displayed by
                                  introducing a number and then clicking the button Update Table."),
                                p("In the second tab, we can see a summary of our data which has been
                                  generated using the dfSummary function of the summarytools package. 
                                  In this tab, we can also see how our variables are defined."),
                                p(" In the last tab called Plots we can see histograms for the 
                                  quantitative variables and boxplots for the qualitative variables.
                                  Also, we can do the scatterplot of two selected variables
                                  to see how they are related."),
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
                                )
                            )
                          )
                          ),
                 tabPanel("Summary",
                          fluidPage(
                            titlePanel("Main characteristics of the variables"),
                            br(),
                            sidebarLayout(
                              sidebarPanel(
                                p("To show a summary of the data, we have used the function dfSummary,
                                  which shows  univariate statistics and/or frequency distributions, 
                                  bar charts or histograms, as well as missing data counts and proportions"),
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
                            )
                          )
                          )
                 ),
                 tabPanel("Plots",
                          fluidPage(
                            titlePanel("Plots of the variables"),
                            br(),
                            sidebarLayout(
                              sidebarPanel(
                                submitButton("Click to update selection and show plot"),
                                radioButtons("view", "",
                                             choices =
                                               list("Histograms" = "histogram",
                                                    "boxplots" = "Boxplots",
                                                    "Scatterplots" = "scatterplot")),
                                
                                conditionalPanel(
                                  condition = "input.view == 'histogram'",
                                  
                                  selectInput("numvariables", "Variables",
                                              choices = c("Carat", "Depth", "PricePerCt","TotalPrice"),
                                              selected = "Carat",
                                              multiple = FALSE),
                                  sliderInput("n_bins", label = NULL, min = 2, max = 30, value = 10)
                              ),
                              conditionalPanel(
                                condition = "input.view == 'Boxplots'",
                                
                                selectInput("facvariables", "Variables",
                                            choices = c("Clarity", "Color"),
                                            selected = "Clarity",
                                            multiple = FALSE)
                              ),
                              conditionalPanel(
                                condition = "input.view == 'scatterplot'",
                                
                                selectInput("allvariables1", "Variable 1 (x)",
                                            choices = c("Carat", "Depth", "PricePerCt","TotalPrice", "Clarity", "Color"),
                                            selected = "Carat",
                                            multiple = FALSE),
                                selectInput("allvariables2", "Variable 2 (y)",
                                            choices = c("Carat", "Depth", "PricePerCt","TotalPrice", "Clarity", "Color"),
                                            selected = "Carat",
                                            multiple = FALSE)
                              )
                              ),
                              mainPanel(
                                conditionalPanel(
                                  condition = "input.view == 'Boxplots'",
                                  plotOutput("boxplot")
                                ),
                                conditionalPanel(
                                  condition = "input.view == 'histogram'",
                                  plotOutput("histplot")
                                ),
                                conditionalPanel(
                                  condition = "input.view == 'scatterplot'",
                                  plotOutput("scatplot")
                                )
                              )
                            )
                          )
                              
                          ),
                         
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
  
  output$boxplot <- renderPlot({
    ggplot(data=Diamonds, aes_string( input$facvariables, Diamonds$TotalPrice)) + 
      geom_boxplot(aes_string(fill = input$facvariables))+ ylab("Total Price") +
      theme_bw() + ggtitle("Boxplot in relationship to Total Price") 
  })
  
  output$histplot = renderPlot({
    ggplot(data = Diamonds, aes_string(x = input$numvariables)) +
      geom_histogram(bins = input$n_bins, fill = "royalblue", color="black") +
      theme_bw() + ggtitle("Histogram of numerical variables")
  })
  
  output$scatplot = renderPlot({
    ggplot(data = Diamonds, aes_string(x= input$allvariables1, y= input$allvariables2)) +
      geom_point(shape=18, color="chartreuse3", size = 3) + theme_bw() +
      geom_smooth(method=lm, linetype="dashed",
                  color="black", fill="blue") + ggtitle("Scatterplot of selected variables")
  })
  
  
  

}
# Run the application 
shinyApp(ui = ui, server = server)