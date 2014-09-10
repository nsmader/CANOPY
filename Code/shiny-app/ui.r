library(shiny)

# Define UI for displaying conditional education trends for given populations
shinyUI(pageWithSidebar(
  
  # Application title
  headerPanel("US Educational Trends Across Time"),
  
  # Sidebar with controls for demographic
  sidebarPanel(
    selectInput("e", "Education trends to show:",
                list("Overall Trends"      = "AllLvlEds",
                     "Conditional Trends"  = "AllCondEds")),
    selectInput("b", "By:",
                list("Birth Cohort"  = "cohort",
                     "Calendar Year" = "year")),
    selectInput("comp", "Compare:",
                list("Education outcomes"= "Eds",
                     "Demographics" = "Dems")),
    conditionalPanel(
      condition = "input.comp == 'Eds'",
      selectInput("g", "Gender:",
                  list("All"    = "All",
                       "Male"   = "Male",
                       "Female" = "Female"))),
    conditionalPanel(
      condition = "input.comp == 'Eds'",
      selectInput("r", "Race/Ethnicity:",
                  list("All"      = "All",
                       "White"    = "WhiteNonH",
                       "Black"    = "BlackNonH",
                       "Hispanic" = "Hisp"))),
    conditionalPanel(
      condition = "input.comp == 'Dems'",
      selectInput("whichComps", "Show comparison across:",
                  list("Gender"              = "compGend",
                       "Race/Ethnicity"      = "compRaceEth",
                       "Gender and Race/Eth" = "compGendRace"))),
    conditionalPanel(
      condition = "input.comp == 'Dems' & input.whichComps == 'compRaceEth'",
      selectInput("whichComps", "Show comparison for:",
                  list("All Genders" = "compForAll",
                       "Males"       = "compForMales",
                       "Females"     = "compForFemales"))),
    conditionalPanel(
      condition = "input.comp == 'Dems' & input.whichComps == 'compGend'",
      selectInput("whichComps", "Show comparison for:",
                  list("All Race/Ethnicities" = "compForAll",
                       "White Non-Hispanics"  = "compForWnH",
                       "Black Non-Hispanics"  = "compForBnH",
                       "Hispanics"            = "compForH")))
  ),
  
  mainPanel(
    
    #h3(textOutput("selection")),
    plotOutput("edPlot"),
    h3(textOutput("plotNote"))
  )
  
))
