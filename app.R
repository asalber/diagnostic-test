#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(ggplot2)
#library(svglite)

# Define UI for application that draws a histogram
ui <- fluidPage(
    tags$head( tags$script(src="http://cdn.mathjax.org/mathjax/latest/MathJax.js?config=TeX-AMS_HTML-full", type = 'text/javascript'),
               tags$script(src = 'https://c328740.ssl.cf1.rackcdn.com/mathjax/2.0-latest/MathJax.js?config=TeX-AMS-MML_HTMLorMML', type = 'text/javascript'),
               tags$script( "MathJax.Hub.Config({tex2jax: {inlineMath: [['$','$'], ['\\(','\\)']]}});", type='text/x-mathjax-config')
    ),

    # Application title
    titlePanel("Diagnostic test"),

    # Sidebar with a slider input for number of bins 
    sidebarLayout(
        sidebarPanel(
            sliderInput("pre",
                        "Prevalence:",
                        min = 0,
                        max = 1,
                        value = 0.1),
            sliderInput("sen",
                        "Sensitivity:",
                        min = 0.5,
                        max = 1,
                        value = 0.8),
            sliderInput("spe",
                        "Specificity:",
                        min = 0.5,
                        max = 1,
                        value = 0.8),
        ),

        # Show a plot of the generated distribution
        mainPanel(
           plotOutput("outcomes"),
           h2('Predictive values'), 
           htmlOutput("predictive.values")
        )
    )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
    
    ppv <- function (pre, sen, spe) {
        # Compute the true and false positives and negatives
        tp <- input$pre * input$sen
        fn <- input$pre * (1-input$sen)
        fp <- (1-input$pre)*(1-input$spe)
        tn <- (1-input$pre)*input$spe
        # Return the positive predictive value
        return(tp / (tp + fp))
    }
    
    npv <- function (pre, sen, spe) {
        # Compute the true and false positives and negatives
        tp <- input$pre * input$sen
        fn <- input$pre * (1-input$sen)
        fp <- (1-input$pre)*(1-input$spe)
        tn <- (1-input$pre)*input$spe
        # Return the positive predictive value
        return( tn / (tn + fn))
    }

    test_plot <- function(pre, sen, spe) {
        # Compute the true and false positives and negatives
        tp <- pre * sen
        fn <- pre * (1-sen)
        fp <- (1-pre)*(1-spe)
        tn <- (1-pre)*spe
        # Compute the predictive values
        ppv <- tp / (tp + fp)
        npv <- tn / (tn + fn)
        # Compute the coordinates for the rectangles of the plot
        xmin <- c(0, 0, pre, pre)
        xmax <- c(pre, pre, 1, 1)
        ymin <- c(0, sen, spe, 0)
        ymax <- c(sen, 1, 1, spe)
        # Create a data frame
        data <- data.frame(Outcome=c("VP", "FN", "FP", "VN"), Frequency=paste0(c(tp, fn, fp, tn) * 100, '%'), xmin, xmax, ymin, ymax)
        # Generate the plot
        p <- ggplot(data) + geom_rect(aes(xmin=xmin, xmax=xmax, ymin=ymin, ymax=ymax, fill=Outcome), colour="white") + 
            geom_text(aes(x=(xmax+xmin)/2, y=(ymin+ymax)/2, label = Frequency), size=6) +
            scale_fill_manual(values = c("#CC0000", "#FF3333", "#33FF99", "#00CC66")) +
            ggtitle(paste0("Prevalence = ", pre*100, "%\nSensitivity = ", sen*100, "%    Specificity = ", spe*100, "%")) +
            theme(plot.title = element_text(hjust = 0.5, size = 20),
                  axis.title.x = element_blank(), axis.text.x = element_blank(), axis.ticks.x = element_blank(), 
                  axis.title.y = element_blank(), axis.text.y = element_blank(), axis.ticks.y = element_blank(),
                  panel.background = element_blank())
        return(p)
    }

    output$outcomes <- renderPlot({
        test_plot(input$pre, input$sen, input$spe)
    })
    
    output$predictive.values <- renderUI({
        HTML(paste(h4("Positive predictive value: P(D|+) =", round(ppv(input$pre, input$sen, input$spe), 2)),
        h4("Negative predictive value: P(D|+) =", round(npv(input$pre, input$sen, input$spe), 2))))
    })
}

# Run the application 
shinyApp(ui = ui, server = server)
