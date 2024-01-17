library(shiny)
library(caret)
library(pROC)
library(shinyWidgets)

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
        prettySwitch(
            inputId = "simulate",
            label = "Simulate Probabilities", 
            status = "success",
            value = FALSE,
            fill = TRUE
        ),
        prettySwitch(
            inputId = "smooth",
            label = "Smooth ROC Curve", 
            status = "success",
            value = FALSE,
            fill = TRUE
        ),
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
        
        if(input$simulate==FALSE){
        FundTest <- data.frame(Target_B = c(rep(0,input$tn),
                                            rep(1,input$tp),
                                            rep(1,input$fn),
                                            rep(0,input$fp)),
                             predict1 =   c(rep(0,input$tn),
                                            rep(1,input$tp),
                                            rep(0,input$fn),
                                            rep(1,input$fp)))
        }else{
        FundTest <- data.frame(Target_B = c(rep(0,input$tn),
                                            rep(1,input$tp),
                                            rep(1,input$fn),
                                            rep(0,input$fp)),
                               predict1 = c(pnorm(sample(seq(0,.5,length=100),input$tn,replace=T)),
                                            pnorm(sample(seq(.5,1,length=100),input$tp,replace=T)),
                                            pnorm(sample(seq(0,.5,length=100),input$fn,replace=T)),
                                            pnorm(sample(seq(.5,1,length=100),input$fp,replace=T))))
        }
        
        rocCurve <- roc(response = FundTest$Target_B, predictor = FundTest$predict1)
        plot(if(input$simulate==FALSE) rocCurve else if(input$smooth == TRUE) {smooth(rocCurve)} else {rocCurve},
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