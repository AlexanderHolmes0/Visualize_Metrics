library(shiny)
library(caret)
library(pROC)

source('Helpers.R')


# Define UI for application that draws a histogram
ui <- fluidPage(
    tags$head(includeHTML("www/head.html")),
    # Application title
    titlePanel("Visualize Classification Metrics"),

    # Sidebar with a slider input for number of bins 
    sidebarLayout(
        sidebarPanel(
        numericInput("tp", "True Positives", 20),
        numericInput("fp", "False Positives", 15),
        numericInput("tn", "True Negatives", 30),
        numericInput("fn", "False Negatives", 15),
        width = 2
        ),

        # Show a plot of the generated distribution
        mainPanel(
           plotOutput("distPlot",width = "900px",height = "450px"),
           plotOutput("rocPlot",width = "900px",height = "500px")
        )
    )
)

# Define server logic required to draw a histogram
server <- function(input, output) {

    output$distPlot <- renderPlot({
        
        validate(
            need(input$tp >= 0 & input$fp >= 0 & input$tn >= 0 & input$fn >= 0 , 
                 "Please Enter Non-negative Numbers")
        )
        
        tab <- as.table(matrix(c(input$tp,input$fp,input$fn,input$tn), nrow = 2, ncol = 2))
        rownames(tab) <- c("1","0")
        colnames(tab) <- c("1","0")
        cm <- confusionMatrix(tab)
        draw_confusion_matrix(cm)
    })
    
    output$rocPlot <- renderPlot({
        
        validate(
            need(input$tp >= 0 & input$fp >= 0 & input$tn >= 0 & input$fn >= 0 , 
                 "Please Enter Non-negative Numbers")
        )
        
        
        FundTest <- data.frame(Target_B = c(rep(0,input$tn),
                                          rep(1,input$tp),
                                          rep(1,input$fn),
                                          rep(0,input$fp)),
                             predict1 = c(rep(0,input$tn),
                                          rep(1,input$tp),
                                          rep(0,input$fn),
                                          rep(1,input$fp)))
       
        rocCurve <- roc(response = FundTest$Target_B, predictor = FundTest$predict1)
        plot(rocCurve,
             identity=TRUE, 
             legacy.axes=T, 
             col = "blue", 
             lwd = 2, main = "ROC Curve",
             print.thres.adj = c(1, -1),
             print.auc = T,
             auc.polygon=TRUE,
             max.auc.polygon=TRUE,
             print.thres=TRUE,
             add=FALSE,
             #asp=NA,
             xlim=c(1,0))
    })
}

# Run the application 
shinyApp(ui = ui, server = server)