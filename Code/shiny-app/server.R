#------------------------------------------------------------------------------#
#
### RUN SERVER OPERATIONS FOR CANOPY BACK-END
#
#------------------------------------------------------------------------------#

shinyServer(function(input, output) {

output$myTable <- renderDataTable(mtcars)
output$objPlot <- renderPlot(plot(runif(10), runif(10)))
output$objPlot2 <- renderPlot(plot(runif(10), runif(10)))

#------------------------------------------------------------------------------#
### Update Data According to User Input ----------------------------------------
#------------------------------------------------------------------------------#

### Receive User Weights ------------------------------------------------------#
pov <- c("n0_50FPL", "n50_99FPL", "100_199FPL", "n200_.FPL")
w <- 
  reactive({
    data.table(pov = pov,
               w = c(input$w0to50, input$w50to99, input$w100to199, input$w200to.))
  })
})