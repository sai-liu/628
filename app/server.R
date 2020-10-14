library(shiny)
function(input,output){
  output$Bodyfat=renderPrint({a=(423.058-384.997*input$density+0.011*input$age+0.013*input$weight+0.033*input$chest-0.083*input$ankle-0.062*input$biceps);paste("Your Bodyfat is ",a,sep="")})
}