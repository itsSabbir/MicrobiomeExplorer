library(shiny)
library(shinydashboard)
library(MicrobiomeExplorer)

# UI

# Define UI for application
ui <- dashboardPage(
  dashboardHeader(title = "Microbiome Data Analysis with MicrobiomeExplorer"),
  dashboardSidebar(
    sidebarMenu(id = "sidebarMenu",
                menuItem("Data Validation", tabName = "dataValidation", icon = icon("check")),
                menuItem("Data Manipulation", tabName = "dataManipulation", icon = icon("edit")),
                menuItem("Differential Expression Analysis", tabName = "DEAnalysis", icon = icon("chart-line")),
                menuItem("Diversity Analysis", tabName = "diversityAnalysis", icon = icon("balance-scale")),
                menuItem("Heatmap Visualization", tabName = "heatmapVis", icon = icon("fire"))
    )
  ),
  dashboardBody(
    tabItems(
      # Data Validation Tab
      tabItem(tabName = "dataValidation",
              fluidPage(
                titlePanel("Data Validation"),
                fileInput("rRNADataUpload", "Upload 16S rRNA Data", accept = c(".csv", ".txt", ".tsv")),
                numericInput("minColumns", "Minimum OTUs/Taxa Columns", value = 10),
                numericInput("minNonZero", "Minimum Non-Zero Entries per Sample", value = 5),
                actionButton("validateData", "Validate Data"),
                verbatimTextOutput("validationResult")
              )
      ),
      # Tab for Data Manipulation
      tabItem(tabName = "dataManipulation",
              fluidPage(
                titlePanel("Data Manipulation"),
                fileInput("newDataUpload", "Upload New Data", accept = c(".csv", ".txt", ".tsv")),
                selectInput("dataType", "Data Type", choices = c("rRNA16S", "Metagenomic", "Metatranscriptomic")),
                selectInput("mergeStrategy", "Merge Strategy", choices = c("Average", "Replace", "Merge")),
                actionButton("addMergeData", "Add and Merge Data"),
                verbatimTextOutput("manipulationResult")
              )
      ),

      # Tab for Differential Expression Analysis
      tabItem(tabName = "DEAnalysis",
              fluidPage(
                titlePanel("Differential Expression Analysis"),
                fileInput("microbiomeDataUpload", "Upload Microbiome Data", accept = c(".csv", ".txt", ".tsv")),
                textInput("conditionsInput", "Conditions (comma-separated)"),
                selectInput("analysisType", "Analysis Type", choices = c("DESeq2", "EdgeR")),
                numericInput("countThreshold", "Count Threshold", value = 5),
                numericInput("minSamples", "Minimum Samples", value = 2),
                actionButton("performAnalysis", "Perform Analysis"),
                dataTableOutput("analysisResult")
              )
      ),

      # Tab for Diversity Analysis
      tabItem(tabName = "diversityAnalysis",
              fluidPage(
                titlePanel("Diversity Analysis"),
                fileInput("diversityDataUpload", "Upload Microbiome Data for Diversity Analysis", accept = c(".csv", ".txt", ".tsv")),
                selectInput("diversityIndices", "Select Diversity Indices", choices = c("Shannon", "Simpson", "Chao1", "ACE", "Fisher"), multiple = TRUE),
                checkboxInput("rarefiedData", "Data is Rarefied", value = FALSE),
                actionButton("calculateDiversity", "Calculate Diversity and Plot Rarefaction"),
                dataTableOutput("alphaDiversityResults"),
                plotOutput("rarefactionPlot")
              )
      ),

      # Tab for Heatmap Visualization
      tabItem(tabName = "heatmapVis",
              fluidPage(
                titlePanel("Heatmap Visualization"),
                fileInput("heatmapDataUpload", "Upload Microbiome Data for Heatmap", accept = c(".csv", ".txt", ".tsv")),
                checkboxInput("normalizeData", "Normalize Data", value = FALSE),
                checkboxInput("clusterRows", "Cluster Rows", value = TRUE),
                checkboxInput("clusterCols", "Cluster Columns", value = TRUE),
                selectInput("colorPalette", "Color Palette", choices = c("Default", "Blues", "Reds", "Greens")),
                actionButton("generateHeatmap", "Generate Heatmap"),
                plotOutput("heatmapPlot")
              )
      ) #contine from here if you have more stuff to add..
    )
  )
)


# Server
server <- function(input, output, session) {
  # Server logic for each module
  # This is where you would call functions from your packages to process data and generate outputs
  observeEvent(input$validateData, {
    # Read uploaded file
    rRNAData <- read.csv(input$rRNADataUpload$datapath)

    # Validate 16S rRNA data
    validationResult <- tryCatch({
      validate16SRNAData(rRNAData, input$minColumns, input$minNonZero)
    }, error = function(e) {
      return(e$message)
    })

    # Display validation result
    output$validationResult <- renderText({
      if (isTRUE(validationResult)) {
        "Data is valid."
      } else {
        paste("Validation failed:", validationResult)
      }
    })
  })

  observeEvent(input$addMergeData, {
    # Read uploaded file
    newData <- read.csv(input$newDataUpload$datapath)

    # Add and merge data
    manipulationResult <- tryCatch({
      # Assuming 'microbiomeDataObject' is a predefined MicrobiomeData object
      updatedObject <- addData(microbiomeDataObject, newData, input$dataType)
      # Handle the merging strategy as per input$mergeStrategy if necessary
      # ...
      return("Data successfully added and merged.")
    }, error = function(e) {
      return(e$message)
    })

    # Display manipulation result
    output$manipulationResult <- renderText({
      manipulationResult
    })
  })

  observeEvent(input$performAnalysis, {
    # Read uploaded microbiome data
    microbiomeData <- read.csv(input$microbiomeDataUpload$datapath)

    # Prepare conditions factor
    conditions <- factor(strsplit(input$conditionsInput, ",")[[1]])

    # Perform differential expression analysis
    analysisResult <- tryCatch({
      performDifferentialExpression(microbiomeData, conditions, input$analysisType,
                                    input$countThreshold, input$minSamples)
    }, error = function(e) {
      return(e$message)
    })

    # Display analysis result
    output$analysisResult <- renderDataTable({
      analysisResult
      updateTabItems(session, "sidebar", "dataManipulation")
    })
  })


  observeEvent(input$calculateDiversity, {
    # Read uploaded microbiome data
    diversityData <- read.csv(input$diversityDataUpload$datapath)

    # Calculate alpha diversity
    alphaDiversityResults <- calculate_alpha_diversity(diversityData,
                                                       input$diversityIndices,
                                                       input$rarefiedData)

    # Display alpha diversity results
    output$alphaDiversityResults <- renderDataTable({
      alphaDiversityResults
    })

    # Generate rarefaction plot
    output$rarefactionPlot <- renderPlot({
      AdvancedRarefactionPlot(diversityData,
                              indices = input$diversityIndices,
                              col = NULL,  # Customize as needed
                              lty = 1)     # Customize as needed
    })
  })

  observeEvent(input$performAnalysis, {
    # Read uploaded microbiome data
    microbiomeData <- read.csv(input$microbiomeDataUpload$datapath)
    print("Data uploaded")  # Debugging print

    # Prepare conditions factor
    conditions <- factor(strsplit(input$conditionsInput, ",")[[1]])
    print("Conditions processed")  # Debugging print

    # Perform differential expression analysis
    analysisResult <- tryCatch({
      print("Performing analysis")  # Debugging print
      performDifferentialExpression(microbiomeData, conditions, input$analysisType,
                                    input$countThreshold, input$minSamples)
    }, error = function(e) {
      print(paste("Error in analysis:", e$message))  # Debugging print
      return(e$message)
    })

    # Display analysis result
    output$analysisResult <- renderDataTable({analysisResult})
    updateTabItems(session, "sidebar", "DEAnalysis")
  })

  observeEvent(input$generateHeatmap, {
    # Read uploaded microbiome data
    heatmapData <- read.csv(input$heatmapDataUpload$datapath)

    # Set color palette based on selection
    selectedPalette <- switch(input$colorPalette,
                              "Default" = heat.colors(10),
                              "Blues" = RColorBrewer::brewer.pal(9, "Blues"),
                              "Reds" = RColorBrewer::brewer.pal(9, "Reds"),
                              "Greens" = RColorBrewer::brewer.pal(9, "Greens"))

    # Generate heatmap
    output$heatmapPlot <- renderPlot({
      plot_microbiome_heatmap(heatmapData,
                              normalize = input$normalizeData,
                              cluster_rows = input$clusterRows,
                              cluster_cols = input$clusterCols,
                              color_palette = selectedPalette)
    })
  })
}

# Run the application
shinyApp(ui = ui, server = server)
