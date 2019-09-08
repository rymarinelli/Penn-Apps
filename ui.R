library(shiny)
library(shinythemes)

ui <- fluidPage(shinytheme = ("darkly"),
    
    titlePanel("Smart Tweet"),
    sidebarLayout(
        sidebarPanel(
            selectInput("outcome", label = h3("Outcome"),
                        choices = list("retweets", "favorite Count", "time",
                                       "LSA One","LSA Two", "LSA Three"), selected = 1),
            
            selectInput("indepvar", label = h3("Explanatory variable"),
                        choices = list("retweets", "favorite Count", "time", 
                                       "LSA One","LSA Two", "LSA Three"), selected = 1)

        ),
        
        mainPanel(
            
            tabsetPanel(type = "tabs",
                        
                        tabPanel("Scatterplot", plotOutput("scatterplot")), # Plot
                        tabPanel("Distribution", # Plots of distributions
                                 fluidRow(
                                     column(6, plotOutput("distribution1")),
                                     column(6, plotOutput("distribution2")))
                        ),
                        tabPanel("Model Summary", verbatimTextOutput("summary")), # Regression output
                        tabPanel("Data", DT::dataTableOutput('tbl')) # Data as datatable
                        
            )
        )
    ))



# SERVER
server <- function(input, output) {
    
    # Regression output
    output$summary <- renderPrint({
        fit <- lm(newData[,input$outcome] ~ newData[,input$indepvar])
        names(fit$coefficients) <- c("Intercept", input$var2)
        summary(fit)
    })
    
    # Data output
    output$tbl = DT::renderDataTable({
        DT::datatable(newData, options = list(lengthChange = FALSE))
    })
    
    
    # Scatterplot output
    output$scatterplot <- renderPlot({
        plot(newData[,input$indepvar], newData[,input$outcome], main="Scatterplot",
             xlab=input$indepvar, ylab=input$outcome, pch=19)
        abline(lm(newData[,input$outcome] ~ newData[,input$indepvar]), col="red")
        lines(lowess(newData[,input$indepvar],newData[,input$outcome]), col="blue")
    }, height=400)
    
    
    # Histogram output var 1
    output$distribution1 <- renderPlot({
        hist(newData[,input$outcome], main="", xlab=input$outcome)
    }, height=300, width=300)
    
    # Histogram output var 2
    output$distribution2 <- renderPlot({
        hist(newData[,input$indepvar], main="", xlab=input$indepvar)
    }, height=300, width=300)
}

shinyApp(ui = ui, server = server)
