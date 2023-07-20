library(shiny)
library(shinyWidgets)
library(dendextend)
library(ggraph)
library(tidygraph)

# Assuming you have a hierarchical clustering object, 'hc', and your dataset, 'df'
# Let's create an example df and hc
set.seed(123)
df <- data.frame(Age = rnorm(100, 50, 10),
                 Variable1 = rnorm(100),
                 Variable2 = rnorm(100),
                 Variable3 = rnorm(100))
rownames(df) <- paste0("Person", 1:nrow(df))
hc <- hclust(dist(df))

# Shiny app
ui <- fluidPage(
    sidebarLayout(
        sidebarPanel(
            sliderTextInput(inputId = "ageRange",
                            label = "Age Range",
                            choices = c(min(df$Age), max(df$Age)),
                            selected = c(min(df$Age), max(df$Age))),
            actionButton("btn", "Update")
        ),
        mainPanel(
            plotOutput("plot")
        )
    )
)

server <- function(input, output, session) {
    observeEvent(input$btn, {
        output$plot <- renderPlot({
            # Filter data based on the age range selected by the user
            df_filtered <- df[df$Age >= input$ageRange[1] & df$Age <= input$ageRange[2], ]

            # Create a new hierarchical clustering object based on the filtered data
            hc_filtered <- hclust(dist(df_filtered))

            # Convert the hclust object to a dendrogram
            dend <- as.dendrogram(hc_filtered)

            # Create a tidygraph object
            graph <- as_tbl_graph(dend)

            # Plot the circular dendrogram using ggraph
            ggraph(graph, layout = 'dendrogram', circular = TRUE) +
                geom_edge_diagonal() +
                geom_node_point() +
                coord_fixed() +
                theme_void()
        })
    })
}

shinyApp(ui, server)
