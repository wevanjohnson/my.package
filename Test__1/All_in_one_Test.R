#This is just a demo
# Load libraries
library(shiny)
library(circlize)

# Define UI
ui <- fluidPage(
    titlePanel("Circos plot example"),
    mainPanel(
        plotOutput("circoPlot")
    )
)

# Define server function
server <- function(input, output) {
    output$circoPlot <- renderPlot({
        data <- data.frame(
            factor = rep(c("Factor1", "Factor2", "Factor3"), each = 4),
            start = rep(1:4, 3),
            end = rep(2:5, 3)
        )

        # Initialize the Circos plot
        circos.initialize(factors = data$factor, xlim = c(1, 5))

        # Set up the track plot region
        circos.trackPlotRegion(factors = data$factor, ylim = c(0, 1),
                               panel.fun = function(x, y) {
                                   circos.axis(labels.cex = 0.5, direction = "inside",
                                               major.at = pretty(c(0, 1), n = 5),
                                               minor.ticks = 2)
                               })

        # Add points to the plot
        circos.trackPoints(data$factor, (data$start + data$end) / 2, y = rep(0.5, nrow(data)),
                           cex = 0.5, pch = 16, col = "#00000080")
    })
}

# Run the application
shinyApp(ui = ui, server = server)
