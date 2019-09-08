
server <- function(input, output) {
    set.seed(122)
    histdata <- newData
    
    output$plot1 <- renderPlot({
        data <- histdata[seq_len(input$slider)]
        hist(data)
        
    output$value <- renderText({ input$caption })
    })
   
}

shinyApp(ui, server)
