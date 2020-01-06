library(shiny)

table_tpm <- readRDS("data/1.rds")
table_zscore <- readRDS("data/2.rds")
table_logfc <- readRDS("data/3.rds")

ui <- fluidPage(
    titlePanel("tableVieweR v0.1"),
    sidebarLayout(
        sidebarPanel(
            radioButtons("dataset", label=h4("Datasets"), 
                         choiceNames = list(HTML("<strong> TPM </strong> (Quantile-Normalized) "), 
                                        HTML("<strong> Zscore </strong> of log2 QN-TPM"), 
                                        HTML("<strong> Log2 Fold Change </strong> (over control) of QN-TPM")),
                         choiceValues = c(1, 2, 3), selected = 1),
            fluidRow(column(3, verbatimTextOutput("value"))),
            textInput("gene", "Gene Name", "MYC"),
            actionButton("submit", "View!")
        ),
        mainPanel(
            h3("Result:"),
            hr(),
            fluidRow(
                column(3,
                    tableOutput("gene_table")
                ),
                column(9,
                    plotOutput("gene_plot") 
                )
            )
            
            
            
        )
    )
)

server <- function(input, output) {
    
    datasetInput <- eventReactive(input$submit, {
        table_selected <- switch(input$dataset, "1" = table_tpm, "2" = table_zscore, "3" = table_logfc)
        gene_value <- table_selected[table_selected$gene_name == input$gene, 2:22]
        data.frame(ID=colnames(table_selected)[2:22], Value=as.numeric(gene_value))
    })

    output$gene_table <- renderTable({datasetInput()}, bordered = TRUE, spacing = "xs")
    output$gene_plot <- renderPlot({
        gene_value <- datasetInput()
        par(mar=c(5, 3, 0, 0.1))
        barplot(gene_value$Value, col="lightblue", names.arg = gene_value$ID, horiz = T, las=1,
                xlab = switch(input$dataset, "1" = "TPM", "2" = "Zscore", "3" = "log2 FC"), ylab="",
                space=0.5, cex.names=1, width = 2)
        }
    )
}

shinyApp(ui = ui, server = server)


