# Load the necessary packages
library(shiny)  # Needed for building the Shiny app
library(SummarizedExperiment)  # Package providing the S4 class used for the data
library(ggplot2)  # Used for plotting
library(ggraph)  # Extends ggplot2 functionality for drawing graphs
library(tidygraph)  # Used for manipulating graph objects

# Define UI for application
ui <- fluidPage(
    titlePanel("Circular Dendogram"),  # Title of the app
    sidebarLayout(
        sidebarPanel(  # Defines the sidebar panel
            fileInput("rds_file", "Choose .rds File", multiple = FALSE, accept = c(".rds")),  # File input for an .rds file
            actionButton('submit', 'Submit')  # Button to submit the file
        ),
        # Main panel for displaying outputs
        mainPanel(
            tableOutput("assay"),  # Table for displaying the 'assay' part of the data
            tableOutput("colData"),  # Table for displaying the 'colData' part of the data
            textOutput("assay_colname"),  # Text output for the 'assay' column names
            textOutput("colData_colname"),  # Text output for the 'colData' column names
            plotOutput("circular_dendogram")  # Plot output for the dendrogram (this isn't defined in your server function)
        )
    )
)
# Define server logic
server <- function(input, output, session) {
    data <- reactive({
        validate(need(input$rds_file, 'No file selected'))  # Check if a file has been selected
        file <- input$rds_file$datapath  # Get the path of the selected file
        data <- readRDS(file)  # Read the .rds file

        if (!is(data, "SummarizedExperiment")) {  # Check if the read object is a SummarizedExperiment
            stop("Data is not a SummarizedExperiment object.")  # Throw an error if it's not
        }

        data  # Return the read data
    })

    # Display the assay and colData after the Submit button is clicked
    observeEvent(input$submit, {  # Run this block when the Submit button is clicked
        output$assay <- renderTable({  # Render the 'assay' table
            as.data.frame(assay(data()))
        })
        output$colData <- renderTable({  # Render the 'colData' table
            as.data.frame(colData(data()))
        })
        output$assay_colname <- renderText({  # Render the 'assay' column names as text
            names(as.data.frame(assay(data())))
        })
        output$colData_colname <- renderText({  # Render the 'colData' column names as text
            names(as.data.frame(colData(data())))
        })
        # There is no code to generate the "circular_dendogram" plot in your current server function
        output$circular_dendogram <- renderPlot({
            # Use the dist function to compute the distance matrix
            d <- dist(as.matrix(assay(data())), method = "euclidean")

            # Use the hclust function to compute the hierarchical clustering
            hc <- hclust(d, method = "average")

            # Convert to dendrogram object
            dend <- as.dendrogram(hc)

            # Create a tidygraph object
            g <- as_tbl_graph(dend)

            # Generate a circular layout
            layout <- create_layout(g, layout = "dendrogram", circular = TRUE)

            # Create the circular dendrogram plot using ggraph
            ggraph(layout) +
                geom_edge_link(width = 1) +  # replace 'linewidth' with 'width'
                geom_node_point() +
                theme_void()  # remove axes and labels
        })
    })
}

# Run the app
shinyApp(ui, server)  # Create and run the Shiny app with the defined UI and server
