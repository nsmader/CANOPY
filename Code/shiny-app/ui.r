library(shiny)

# Define UI for displaying conditional education trends for given populations
shinyUI(fluidPage(
  
  # Application title
  titlePanel("CANOPY -- A Tool for Maximizing Accessibility of Head Start Services"),
  
  # Sidebar Layout for Controls and Map output
  sidebarLayout(
    sidebarPanel(
      wellPanel(
        h3("Planning Goals Scenario"),
        sliderInput("w0to50",    "Score for Engaging Youth 0-50% FPL:",
                    min = 0, max = 10, value = 10),
        sliderInput("w50to99",   "Score for Engaging Youth 50-99% FPL:",
                    min = 0, max = 10, value = 7),
        sliderInput("w100to199", "Score for Engaging Youth 100-199% FPL:",
                    min = 0, max = 10, value = 3),
        sliderInput("w200to.",   "Score for Engaging Youth above 200% FPL:",
                    min = 0, max = 10, value = 1)
      ),
     wellPanel(
        h3("Planning Context Scenario"),
        numericInput("R", "Total Resources to Allocate",
                     min = 0, value = 50),
        numericInput("rMin", "Resource Minimum per Site",
                     min = 0, value = 0),
        numericInput("rMax", "Resource Maximum per Site",
                     min = 0, value = 50)
      ),
      wellPanel(
        h3("Run CANOPY Recommendation Engine"),
        numericInput("iter", "Number of Iterations to Run",
                     min = 0, value = 500, step = 100),
        #h3("Run CANOPY"),
        p(actionButton("runCANOPY", "Run CANOPY", icon("bolt"))) # look here for all icons - http://fontawesome.io/icons/
      )
    ),
    
    mainPanel(
      tabsetPanel(
        tabPanel(
          h3("The CANOPY Method"),
          p("The CANOPY method is the best.")
        ),
        tabPanel(
          h3("Map of Recommended Allocation"),
          plotOutput("objPlot"),
          p("This is a plot note.")
        )
      )
    )
  )
  , # End of sidebar layout for the top half
  
  ### Fluid Rows for the Bottom Half with Post-run Displays
  # # Display run progress
  fluidRow(
    column(width = 3,
           h2("Run Progress")),
    column(width = 9,
           tableOutput("weights"))
  ),
  fluidRow(
    column(width = 6,
           plotOutput("objPlot2")),
    column(width = 6,
           h2("Run Benchmarks"),
           h3(paste("<b>Benchmark 1</b>",
                    "This is the target score that would result from allocating
                     resources according to X",
                    "<b>Benchmark 2</b>",
                    "This is the target score that would result from allocating
                     resources according to Y",
                    sep = "</br>")))
  )
  ,
  fluidRow(
    column(width = 3,
           h2("Table Output"),
           p(actionButton("dlData", "Download Recommendations", icon("download")))),
    column(width = 9,
           dataTableOutput("myTable"))
  )
))
