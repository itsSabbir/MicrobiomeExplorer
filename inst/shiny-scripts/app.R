library(shiny)
library(shinydashboard)
library(shinyWidgets)
library(shinycssloaders)
library(DT)
library(plotly)
library(MicrobiomeExplorer)

# ── UI ────────────────────────────────────────────────────────────────────────

ui <- dashboardPage(
  skin = "blue",

  dashboardHeader(title = "MicrobiomeExplorer"),

  dashboardSidebar(
    sidebarMenu(id = "sidebarMenu",
      menuItem("Home",                   tabName = "home",          icon = icon("home")),
      menuItem("Data Upload",            tabName = "dataUpload",    icon = icon("upload")),
      menuItem("Preprocessing",          tabName = "preprocessing", icon = icon("filter")),
      menuItem("Taxonomic Composition",  tabName = "taxonomic",     icon = icon("layer-group")),
      menuItem("Beta Diversity",         tabName = "betaDiversity", icon = icon("project-diagram")),
      menuItem("Alpha Diversity",        tabName = "alphaDiversity",icon = icon("balance-scale")),
      menuItem("Differential Abundance", tabName = "deAnalysis",    icon = icon("chart-line")),
      menuItem("Correlation Analysis",   tabName = "correlation",   icon = icon("network-wired")),
      menuItem("Heatmap",                tabName = "heatmap",       icon = icon("fire")),
      menuItem("ML Classification",     tabName = "mlClassification", icon = icon("robot")),
      menuItem("Biomarkers & Clustering",tabName = "biomarkerClustering", icon = icon("microscope")),
      menuItem("Network Analysis",      tabName = "networkAnalysis", icon = icon("circle-nodes")),
      menuItem("Export",                 tabName = "export",        icon = icon("download"))
    )
  ),

  dashboardBody(
    tags$head(tags$style(HTML("
      .content-wrapper { background-color: #f4f6f9; }
      .box { border-radius: 6px; }
      .info-box { border-radius: 6px; }
    "))),

    tabItems(

      # ── Tab 1: Home ─────────────────────────────────────────────────────────
      tabItem(tabName = "home",
        fluidRow(
          infoBoxOutput("nSamplesBox",  width = 3),
          infoBoxOutput("nTaxaBox",     width = 3),
          infoBoxOutput("dataTypeBox",  width = 3),
          infoBoxOutput("normStatusBox",width = 3)
        ),
        fluidRow(
          box(width = 12, title = "Getting Started", status = "primary", solidHeader = TRUE,
            p("Welcome to ", strong("MicrobiomeExplorer"), " — a comprehensive platform for microbiome data analysis."),
            tags$ol(
              tags$li("Upload your OTU/ASV count table in the ", strong("Data Upload"), " tab."),
              tags$li("Optionally upload sample metadata (CSV with sample IDs as row names)."),
              tags$li("Apply filters and normalisation in the ", strong("Preprocessing"), " tab."),
              tags$li("Explore Taxonomic Composition, Beta Diversity, Alpha Diversity, Differential Abundance, Correlation, and Heatmap tabs."),
              tags$li("Download results from the ", strong("Export"), " tab.")
            )
          )
        ),
        fluidRow(
          box(width = 12, title = "Data Preview", status = "info", collapsible = TRUE,
            DT::dataTableOutput("homePreviewTable")
          )
        )
      ),

      # ── Tab 2: Data Upload ───────────────────────────────────────────────────
      tabItem(tabName = "dataUpload",
        fluidPage(
          titlePanel("Data Upload & Management"),
          fluidRow(
            box(width = 6, title = "Count Table", status = "primary", solidHeader = TRUE,
              fileInput("mainDataUpload", "Upload OTU/ASV Count Table",
                        accept = c(".csv", ".txt", ".tsv"),
                        placeholder = "CSV, TXT, or TSV (rows = samples, cols = taxa)"),
              radioButtons("dataTypeSelect", "Data Type",
                           choices = c("16S rRNA" = "rRNA16S",
                                       "Metagenomic" = "Metagenomic",
                                       "Metatranscriptomic" = "Metatranscriptomic"),
                           inline = TRUE),
              numericInput("minColsUpload",    "Minimum Taxa Columns",         value = 2, min = 1),
              numericInput("minNonZeroUpload", "Min Non-Zero Entries / Sample", value = 1, min = 0)
            ),
            box(width = 6, title = "Sample Metadata (optional)", status = "info", solidHeader = TRUE,
              fileInput("sampleInfoUpload", "Upload Sample Metadata",
                        accept = c(".csv", ".txt", ".tsv"),
                        placeholder = "CSV with sample IDs as row names"),
              p(em("Metadata columns (e.g. Treatment, Group) will be available for
                   colouring plots and running statistical tests."))
            )
          ),
          fluidRow(
            box(width = 12,
              actionButton("processUpload", "Load Data", icon = icon("upload"),
                           class = "btn-primary btn-lg"),
              br(), br(),
              verbatimTextOutput("uploadStatus")
            )
          ),
          fluidRow(
            box(width = 12, title = "Count Table Preview", collapsible = TRUE,
              DT::dataTableOutput("dataPreviewTable")
            )
          ),
          fluidRow(
            box(width = 12, title = "Metadata Preview", collapsible = TRUE,
              DT::dataTableOutput("metaPreviewTable")
            )
          )
        )
      ),

      # ── Tab 3: Preprocessing ─────────────────────────────────────────────────
      tabItem(tabName = "preprocessing",
        fluidPage(
          titlePanel("Preprocessing & Filtering"),
          fluidRow(
            box(width = 4, title = "Filtering", status = "warning", solidHeader = TRUE,
              sliderInput("prevFilter", "Min Taxon Prevalence (%)",
                          min = 0, max = 100, value = 10, step = 1),
              sliderInput("abunFilter", "Min Mean Relative Abundance (%)",
                          min = 0, max = 10, value = 0, step = 0.1)
            ),
            box(width = 4, title = "Normalisation", status = "success", solidHeader = TRUE,
              selectInput("normMethod", "Normalisation Method",
                          choices = c("None"               = "none",
                                      "Relative Abundance" = "rel",
                                      "CLR Transform"      = "clr",
                                      "Rarefaction"        = "rarefy")),
              numericInput("rarefyDepth", "Rarefaction Depth (leave blank for auto)",
                           value = NA, min = 1)
            ),
            box(width = 4, title = "Apply", status = "primary", solidHeader = TRUE,
              br(),
              actionButton("applyPreprocessing", "Apply Preprocessing",
                           icon = icon("filter"), class = "btn-primary"),
              br(), br(),
              verbatimTextOutput("preprocessSummary")
            )
          ),
          fluidRow(
            box(width = 12, title = "Filtered & Normalised Data Preview", collapsible = TRUE,
              (DT::dataTableOutput("filteredDataPreview"))
            )
          )
        )
      ),

      # ── Tab 4: Taxonomic Composition ─────────────────────────────────────────
      tabItem(tabName = "taxonomic",
        fluidPage(
          titlePanel("Taxonomic Composition"),
          fluidRow(
            box(width = 3, title = "Options", status = "primary", solidHeader = TRUE,
              selectInput("taxGroupVar", "Colour / Group By", choices = c("None" = "")),
              numericInput("taxTopN", "Show Top N Taxa", value = 10, min = 2, max = 50),
              selectInput("taxPalette", "Colour Palette",
                          choices = c("Set3", "Set1", "Set2", "Paired", "Spectral", "Dark2")),
              checkboxInput("taxNormalize", "Normalise to Relative Abundance", value = TRUE),
              actionButton("plotTaxonomy", "Generate Plot", icon = icon("chart-bar"),
                           class = "btn-primary")
            ),
            box(width = 9, title = "Taxonomic Composition Plot", status = "info",
              (plotly::plotlyOutput("taxAbundancePlot", height = "500px"))
            )
          )
        )
      ),

      # ── Tab 5: Beta Diversity ────────────────────────────────────────────────
      tabItem(tabName = "betaDiversity",
        fluidPage(
          titlePanel("Beta Diversity & Ordination"),
          fluidRow(
            box(width = 3, title = "Options", status = "primary", solidHeader = TRUE,
              selectInput("betaDistMethod", "Distance Metric",
                          choices = c("Bray-Curtis" = "bray",
                                      "Jaccard"     = "jaccard",
                                      "Euclidean"   = "euclidean")),
              selectInput("ordinationMethod", "Ordination Method",
                          choices = c("PCoA", "NMDS", "PCA")),
              selectInput("betaColorVar", "Colour Points By", choices = c("None" = "")),
              checkboxInput("betaEllipse", "Draw 95% Confidence Ellipses", value = FALSE),
              checkboxInput("betaLabels",  "Label Samples",                value = FALSE),
              actionButton("runBetaAnalysis", "Run Analysis", icon = icon("play"),
                           class = "btn-primary")
            ),
            box(width = 9, title = "Ordination Plot", status = "info",
              (plotOutput("ordinationPlot", height = "450px"))
            )
          ),
          fluidRow(
            box(width = 6, title = "PERMANOVA Results", collapsible = TRUE,
              verbatimTextOutput("permanovaResult")
            ),
            box(width = 6, title = "Distance Matrix (first 10×10)", collapsible = TRUE,
              (DT::dataTableOutput("distMatrixTable"))
            )
          )
        )
      ),

      # ── Tab 6: Alpha Diversity ────────────────────────────────────────────────
      tabItem(tabName = "alphaDiversity",
        fluidPage(
          titlePanel("Alpha Diversity"),
          fluidRow(
            box(width = 3, title = "Options", status = "primary", solidHeader = TRUE,
              checkboxGroupInput("alphaIndices", "Diversity Indices",
                                 choices = c("Shannon", "Simpson", "Chao1", "ACE", "Fisher"),
                                 selected = c("Shannon", "Simpson")),
              checkboxInput("alphaRarefied", "Use Rarefied Data", value = FALSE),
              selectInput("alphaGroupVar",  "Group By (metadata column)",
                          choices = c("None" = "")),
              selectInput("alphaIndex4Plot", "Index to Visualise", choices = NULL),
              radioButtons("alphaPlotType", "Plot Type",
                           choices = c("Boxplot" = "box", "Violin" = "violin"), inline = TRUE),
              checkboxInput("alphaAddPoints", "Show Individual Points", value = TRUE),
              actionButton("calcAlpha", "Calculate & Plot", icon = icon("calculator"),
                           class = "btn-primary")
            ),
            box(width = 9,
              tabBox(width = 12,
                tabPanel("Diversity Boxplot",
                  (plotOutput("alphaDivBoxplot", height = "400px")),
                  verbatimTextOutput("alphaTestResult")
                ),
                tabPanel("Rarefaction Curves",
                  (plotOutput("rarefactionCurvePlot", height = "400px"))
                ),
                tabPanel("Results Table",
                  (DT::dataTableOutput("alphaDivTable"))
                )
              )
            )
          )
        )
      ),

      # ── Tab 7: Differential Abundance ────────────────────────────────────────
      tabItem(tabName = "deAnalysis",
        fluidPage(
          titlePanel("Differential Abundance Analysis"),
          fluidRow(
            box(width = 3, title = "Options", status = "primary", solidHeader = TRUE,
              selectInput("deGroupVar", "Group Column (metadata)",
                          choices = c("None" = "")),
              selectInput("deAnalysisType", "Method",
                          choices = c("DESeq2", "EdgeR")),
              numericInput("deCountThreshold", "Count Threshold",      value = 5,    min = 0),
              numericInput("deMinSamples",     "Min Samples",          value = 2,    min = 1),
              numericInput("deFCThreshold",    "Log2 FC Threshold",    value = 1,    min = 0),
              numericInput("dePvalThreshold",  "P-value Threshold",    value = 0.05, min = 0, max = 1, step = 0.01),
              actionButton("runDE", "Run Analysis", icon = icon("play"),
                           class = "btn-primary")
            ),
            box(width = 9,
              tabBox(width = 12,
                tabPanel("Volcano Plot",
                  (plotOutput("volcanoPlot", height = "450px"))
                ),
                tabPanel("Results Table",
                  (DT::dataTableOutput("deResultsTable"))
                )
              )
            )
          )
        )
      ),

      # ── Tab 8: Correlation Analysis ───────────────────────────────────────────
      tabItem(tabName = "correlation",
        fluidPage(
          titlePanel("Taxon Correlation Analysis"),
          fluidRow(
            box(width = 3, title = "Options", status = "primary", solidHeader = TRUE,
              selectInput("corrMethod", "Correlation Method",
                          choices = c("Spearman (recommended)" = "spearman",
                                      "Pearson"                = "pearson")),
              sliderInput("corrMinPrev", "Min Taxon Prevalence (%)",
                          min = 0, max = 100, value = 10, step = 1),
              numericInput("corrTopN", "Top N Taxa by Variance", value = 30, min = 3, max = 100),
              actionButton("runCorr", "Compute Correlations", icon = icon("play"),
                           class = "btn-primary")
            ),
            box(width = 9, title = "Correlation Heatmap", status = "info",
              (plotOutput("corrHeatmapPlot", height = "500px"))
            )
          )
        )
      ),

      # ── Tab 9: Heatmap ────────────────────────────────────────────────────────
      tabItem(tabName = "heatmap",
        fluidPage(
          titlePanel("Microbiome Abundance Heatmap"),
          fluidRow(
            box(width = 3, title = "Options", status = "primary", solidHeader = TRUE,
              numericInput("heatmapTopN",   "Top N Taxa by Variance", value = 50, min = 5),
              checkboxInput("normalizeData", "Normalise Rows",        value = FALSE),
              checkboxInput("clusterRows",   "Cluster Rows",          value = TRUE),
              checkboxInput("clusterCols",   "Cluster Columns",       value = TRUE),
              selectInput("colorPalette", "Colour Palette",
                          choices = c("Blue-White-Red" = "Default",
                                      "Blues", "Reds", "Greens", "Purples")),
              actionButton("generateHeatmap", "Generate Heatmap", icon = icon("fire"),
                           class = "btn-primary")
            ),
            box(width = 9, title = "Heatmap", status = "info",
              (plotOutput("heatmapPlot", height = "550px"))
            )
          )
        )
      ),

      # ── Tab 11: ML Classification ──────────────────────────────────────────
      tabItem(tabName = "mlClassification",
        fluidPage(
          titlePanel("Machine Learning Classification"),
          fluidRow(
            box(width = 3, title = "Options", status = "primary", solidHeader = TRUE,
              selectInput("mlGroupVar", "Group Variable", choices = c("Upload metadata first" = "")),
              numericInput("mlNtree", "Number of Trees", value = 500, min = 100),
              sliderInput("mlTestFrac", "Test Fraction", min = 0.1, max = 0.5, value = 0.3, step = 0.05),
              numericInput("mlNfolds", "CV Folds", value = 5, min = 2, max = 10),
              numericInput("mlNrepeats", "CV Repeats", value = 5, min = 1, max = 20),
              actionButton("runRF", "Train Model", icon = icon("play"), class = "btn-primary btn-block"),
              br(),
              actionButton("runCV", "Run Cross-Validation", icon = icon("chart-line"), class = "btn-info btn-block")
            ),
            box(width = 9, title = "Results",
              tabsetPanel(
                tabPanel("Summary", (verbatimTextOutput("rfSummary"))),
                tabPanel("Feature Importance", (plotOutput("rfImportancePlot", height = "500px"))),
                tabPanel("ROC Curve", (plotOutput("rocCurvePlot", height = "450px"))),
                tabPanel("Confusion Matrix", (DT::dataTableOutput("rfConfusionTable")))
              )
            )
          )
        )
      ),

      # ── Tab 12: Biomarkers & Clustering ─────────────────────────────────────
      tabItem(tabName = "biomarkerClustering",
        fluidPage(
          titlePanel("Biomarker Discovery & Clustering"),
          fluidRow(
            box(width = 3, title = "Options", status = "primary", solidHeader = TRUE,
              h4("Biomarker Discovery"),
              selectInput("bmGroupVar", "Group Variable", choices = c("Upload metadata first" = "")),
              selectInput("bmMethod", "Method", choices = c("Kruskal-Wallis" = "kw", "Random Forest" = "rf", "Combined" = "combined")),
              numericInput("bmPvalThresh", "P-value Threshold", value = 0.05, min = 0.001, max = 0.1, step = 0.01),
              numericInput("bmLdaThresh", "Effect Size Threshold", value = 0.5, min = 0, step = 0.1),
              actionButton("runBiomarkers", "Find Biomarkers", icon = icon("search"), class = "btn-primary btn-block"),
              hr(),
              h4("Clustering"),
              selectInput("clMethod", "Method", choices = c("K-means" = "kmeans", "Hierarchical" = "hierarchical")),
              numericInput("clK", "K (blank = auto)", value = NA, min = 2, max = 20),
              numericInput("clMaxK", "Max K (auto)", value = 10, min = 2, max = 30),
              actionButton("runClustering", "Run Clustering", icon = icon("object-group"), class = "btn-info btn-block")
            ),
            box(width = 9, title = "Results",
              tabsetPanel(
                tabPanel("Biomarker Plot", (plotOutput("biomarkerPlot", height = "500px"))),
                tabPanel("Biomarker Table", (DT::dataTableOutput("biomarkerTable"))),
                tabPanel("Cluster Ordination", (plotOutput("clusterOrdPlot", height = "450px"))),
                tabPanel("Cluster Summary", (verbatimTextOutput("clusterSummary")))
              )
            )
          )
        )
      ),

      # ── Tab 13: Network Analysis ────────────────────────────────────────────
      tabItem(tabName = "networkAnalysis",
        fluidPage(
          titlePanel("Co-occurrence Network Analysis"),
          fluidRow(
            box(width = 3, title = "Options", status = "primary", solidHeader = TRUE,
              selectInput("netCorrMethod", "Correlation Method", choices = c("Spearman" = "spearman", "Pearson" = "pearson")),
              sliderInput("netCorThresh", "Min. |Correlation|", min = 0.1, max = 0.9, value = 0.6, step = 0.05),
              numericInput("netPvalThresh", "P-value Threshold", value = 0.05, min = 0.001, max = 0.1, step = 0.01),
              sliderInput("netPrev", "Min. Prevalence (%)", min = 0, max = 100, value = 10),
              numericInput("netTopTaxa", "Top Taxa (by variance)", value = 50, min = 5, max = 500),
              selectInput("netLayout", "Layout", choices = c("Fruchterman-Reingold" = "fr", "Circle" = "circle", "Kamada-Kawai" = "kk")),
              actionButton("buildNetwork", "Build Network", icon = icon("circle-nodes"), class = "btn-primary btn-block")
            ),
            box(width = 9, title = "Results",
              tabsetPanel(
                tabPanel("Network Plot", (plotOutput("networkPlot", height = "550px"))),
                tabPanel("Node Metrics", (DT::dataTableOutput("nodeMetricsTable"))),
                tabPanel("Edge List", (DT::dataTableOutput("edgeListTable")))
              )
            )
          )
        )
      ),

      # ── Tab 14: Export ────────────────────────────────────────────────────────
      tabItem(tabName = "export",
        fluidPage(
          titlePanel("Export Results"),
          fluidRow(
            box(width = 6, title = "Download Plots", status = "primary", solidHeader = TRUE,
              selectInput("exportPlotSelect", "Choose Plot",
                          choices = c("Taxonomic Composition"  = "taxonomic",
                                      "Ordination (Beta)"      = "ordination",
                                      "Alpha Diversity Boxplot"= "alpha_box",
                                      "Rarefaction Curves"     = "rarefaction",
                                      "Volcano Plot"           = "volcano",
                                      "Correlation Heatmap"    = "corr_heatmap",
                                      "Abundance Heatmap"      = "heatmap",
                                      "Feature Importance"     = "rf_importance",
                                      "ROC Curve"              = "roc_curve",
                                      "Biomarker Plot"         = "biomarker",
                                      "Network Plot"           = "network")),
              radioButtons("exportFormat", "File Format",
                           choices = c("PNG" = "png", "PDF" = "pdf", "SVG" = "svg"),
                           inline = TRUE),
              numericInput("exportWidth",  "Width (inches)",  value = 8,  min = 2),
              numericInput("exportHeight", "Height (inches)", value = 6,  min = 2),
              downloadButton("downloadPlot", "Download Plot", class = "btn-success")
            ),
            box(width = 6, title = "Download Tables", status = "info", solidHeader = TRUE,
              selectInput("exportTableSelect", "Choose Table",
                          choices = c("Alpha Diversity Results"  = "alpha",
                                      "Differential Abundance"   = "de",
                                      "Distance Matrix"          = "dist",
                                      "Correlation Matrix"       = "corr",
                                      "Preprocessed Count Data"  = "norm",
                                      "Raw Count Data"           = "raw")),
              br(),
              downloadButton("downloadTable", "Download CSV", class = "btn-info")
            )
          )
        )
      )

    ) # end tabItems
  )   # end dashboardBody
)


# ── Server ────────────────────────────────────────────────────────────────────

server <- function(input, output, session) {

  # Central reactive store
  rv <- reactiveValues(
    microbiomeData = NULL,
    sampleInfo     = NULL,
    microbiomeObj  = NULL,
    normData       = NULL,
    alphaResults   = NULL,
    deResults      = NULL,
    deResultsFlat  = NULL,
    betaDist       = NULL,
    ordinationRes  = NULL,
    corrMatrix     = NULL,
    # Plot handles for export
    lastTaxPlot    = NULL,
    lastOrdPlot    = NULL,
    lastAlphaPlot  = NULL,
    lastRarePlot   = NULL,
    lastVolcPlot   = NULL,
    lastCorrPlot   = NULL,
    lastHeatPlot   = NULL,
    # ML/AI results
    rfResult       = NULL,
    classResult    = NULL,
    biomarkerResult= NULL,
    clusterResult  = NULL,
    networkResult  = NULL,
    lastImportPlot = NULL,
    lastROCPlot    = NULL,
    lastBiomarkerPlot = NULL,
    lastNetworkPlot= NULL
  )

  # Helper: active data (normalised if available, else raw)
  activeData <- reactive({
    if (!is.null(rv$normData)) rv$normData else rv$microbiomeData
  })

  # ── Auto-load sample data on startup ──────────────────────────────────────
  local({
    data("sampleDataset", package = "MicrobiomeExplorer", envir = environment())
    rv$microbiomeData <- sampleDataset$counts
    rv$sampleInfo     <- sampleDataset$metadata
  })

  # ── Update sidebar selects when metadata loads ────────────────────────────
  observe({
    req(rv$sampleInfo)
    cols <- colnames(rv$sampleInfo)
    # Default to "Group" if it exists (common for 2-group comparisons)
    default <- if ("Group" %in% cols) "Group" else cols[1]
    updateSelectInput(session, "taxGroupVar",   choices = c("None" = "", cols), selected = default)
    updateSelectInput(session, "betaColorVar",  choices = c("None" = "", cols), selected = default)
    updateSelectInput(session, "alphaGroupVar", choices = c("None" = "", cols), selected = default)
    updateSelectInput(session, "deGroupVar",    choices = c("None" = "", cols), selected = default)
  })

  # Update alpha index choices after calculation
  observe({
    req(rv$alphaResults)
    idx_cols <- setdiff(colnames(rv$alphaResults), "Sample")
    updateSelectInput(session, "alphaIndex4Plot", choices = idx_cols,
                      selected = idx_cols[1])
  })

  # ── Home infoBoxes ─────────────────────────────────────────────────────────
  output$nSamplesBox <- renderInfoBox({
    n <- if (!is.null(rv$microbiomeData)) nrow(rv$microbiomeData) else 0
    infoBox("Samples", n, icon = icon("flask"), color = "blue")
  })
  output$nTaxaBox <- renderInfoBox({
    n <- if (!is.null(rv$microbiomeData)) ncol(rv$microbiomeData) else 0
    infoBox("Taxa", n, icon = icon("bacteria"), color = "green")
  })
  output$dataTypeBox <- renderInfoBox({
    dt <- if (!is.null(rv$microbiomeObj)) "Loaded" else "No Data"
    infoBox("Data", dt, icon = icon("database"), color = if (dt == "Loaded") "olive" else "red")
  })
  output$normStatusBox <- renderInfoBox({
    st <- if (!is.null(rv$normData)) "Preprocessed" else "Raw"
    infoBox("Status", st, icon = icon("check-circle"),
            color = if (st == "Preprocessed") "teal" else "orange")
  })

  output$homePreviewTable <- DT::renderDataTable({
    req(rv$microbiomeData)
    DT::datatable(
      as.data.frame(rv$microbiomeData[1:min(10, nrow(rv$microbiomeData)),
                                      1:min(10, ncol(rv$microbiomeData))]),
      options = list(scrollX = TRUE, pageLength = 5),
      caption = "First 10 samples x 10 taxa"
    )
  })

  # ── Default guarded renders (prevent infinite spinners on empty outputs) ──
  output$uploadStatus       <- renderText({ "" })
  output$dataPreviewTable   <- DT::renderDataTable({ req(rv$microbiomeData); NULL })
  output$metaPreviewTable   <- DT::renderDataTable({ req(rv$sampleInfo); NULL })
  output$preprocessSummary  <- renderText({ req(rv$normData); "" })
  output$filteredDataPreview<- DT::renderDataTable({ req(rv$normData); NULL })
  output$taxAbundancePlot   <- plotly::renderPlotly({ req(rv$microbiomeData); NULL })
  output$ordinationPlot     <- renderPlot({ req(rv$ordinationRes); NULL })
  output$permanovaResult    <- renderText({ "" })
  output$distMatrixTable    <- DT::renderDataTable({ req(rv$betaDist); NULL })
  output$alphaDivBoxplot    <- renderPlot({ req(rv$alphaResults); NULL })
  output$rarefactionCurvePlot <- renderPlot({ req(rv$alphaResults); NULL })
  output$alphaDivTable      <- DT::renderDataTable({ req(rv$alphaResults); NULL })
  output$alphaTestResult    <- renderText({ "" })
  output$volcanoPlot        <- renderPlot({ req(rv$deResults); NULL })
  output$deResultsTable     <- DT::renderDataTable({ req(rv$deResults); NULL })
  output$corrHeatmapPlot    <- renderPlot({ req(rv$corrMatrix); NULL })
  output$heatmapPlot        <- renderPlot({ req(rv$lastHeatPlot); NULL })
  output$rfSummary          <- renderText({ req(rv$rfResult); "" })
  output$rfImportancePlot   <- renderPlot({ req(rv$lastImportPlot); print(rv$lastImportPlot) })
  output$rocCurvePlot       <- renderPlot({ req(rv$lastROCPlot); print(rv$lastROCPlot) })
  output$rfConfusionTable   <- DT::renderDataTable({ req(rv$rfResult); NULL })
  output$biomarkerPlot      <- renderPlot({ req(rv$lastBiomarkerPlot); print(rv$lastBiomarkerPlot) })
  output$biomarkerTable     <- DT::renderDataTable({ req(rv$biomarkerResult); NULL })
  output$clusterOrdPlot     <- renderPlot({ req(rv$clusterResult); NULL })
  output$clusterSummary     <- renderText({ req(rv$clusterResult); "" })
  output$networkPlot        <- renderPlot({ req(rv$lastNetworkPlot); print(rv$lastNetworkPlot) })
  output$nodeMetricsTable   <- DT::renderDataTable({ req(rv$networkResult); NULL })
  output$edgeListTable      <- DT::renderDataTable({ req(rv$networkResult); NULL })

  # ── Tab 2: Data Upload ─────────────────────────────────────────────────────
  observeEvent(input$processUpload, {
    req(input$mainDataUpload)

    tryCatch({
      raw <- read.csv(input$mainDataUpload$datapath,
                      header = TRUE, row.names = 1, check.names = FALSE)
      raw <- as.matrix(raw)
      storage.mode(raw) <- "numeric"

      # Validation
      val_fn <- switch(input$dataTypeSelect,
        "rRNA16S"           = validate16SRNAData,
        "Metagenomic"       = validateMetagenomicData,
        "Metatranscriptomic"= validateMetatranscriptomicData
      )
      val_result <- tryCatch({
        if (input$dataTypeSelect == "rRNA16S") {
          val_fn(raw, input$minColsUpload, input$minNonZeroUpload)
        } else {
          val_fn(raw)
        }
      }, error = function(e) e$message)
      if (!isTRUE(val_result)) {
        output$uploadStatus <- renderText(paste("Validation warning:", val_result))
      }

      rv$microbiomeData <- raw
      rv$normData       <- NULL  # reset preprocessing
      rv$alphaResults   <- NULL
      rv$deResults      <- NULL
      rv$betaDist       <- NULL

      # Metadata
      if (!is.null(input$sampleInfoUpload)) {
        meta <- read.csv(input$sampleInfoUpload$datapath,
                         header = TRUE, row.names = 1, check.names = FALSE)
        rv$sampleInfo <- meta

        # Warn if sample IDs don't fully match
        data_samples <- rownames(raw)
        meta_samples <- rownames(meta)
        missing_in_meta <- setdiff(data_samples, meta_samples)
        if (length(missing_in_meta) > 0) {
          showNotification(
            paste0(length(missing_in_meta),
                   " sample(s) in count data have no matching metadata row."),
            type = "warning", duration = 10
          )
        }
      }

      # Build S4 object (best-effort)
      rv$microbiomeObj <- tryCatch({
        si <- if (!is.null(rv$sampleInfo)) as.list(rv$sampleInfo) else list()
        new("MicrobiomeData",
            rRNA16S           = if (input$dataTypeSelect == "rRNA16S") raw else NULL,
            Metagenomic       = if (input$dataTypeSelect == "Metagenomic") raw else NULL,
            Metatranscriptomic= if (input$dataTypeSelect == "Metatranscriptomic") raw else NULL,
            SampleInfo        = si)
      }, error = function(e) NULL)

      output$uploadStatus <- renderText({
        paste0("Data loaded successfully.\n",
               nrow(raw), " samples  ×  ", ncol(raw), " taxa\n",
               if (!is.null(rv$sampleInfo))
                 paste0("Metadata: ", nrow(rv$sampleInfo), " samples, ",
                        ncol(rv$sampleInfo), " columns")
               else "No metadata uploaded.")
      })

      output$dataPreviewTable <- DT::renderDataTable({
        DT::datatable(
          as.data.frame(raw[1:min(10, nrow(raw)), 1:min(15, ncol(raw))]),
          options = list(scrollX = TRUE, pageLength = 5)
        )
      })

      output$metaPreviewTable <- DT::renderDataTable({
        req(rv$sampleInfo)
        DT::datatable(head(rv$sampleInfo, 10),
                      options = list(scrollX = TRUE, pageLength = 5))
      })

    }, error = function(e) {
      output$uploadStatus <- renderText(paste("Error loading data:", e$message))
    })
  })

  # ── Tab 3: Preprocessing ───────────────────────────────────────────────────
  observeEvent(input$applyPreprocessing, {
    req(rv$microbiomeData)

    tryCatch({
      data <- rv$microbiomeData
      n_start <- ncol(data)

      # Prevalence filter
      prev_thresh <- input$prevFilter / 100
      prevalence  <- colMeans(data > 0)
      data <- data[, prevalence >= prev_thresh, drop = FALSE]

      # Abundance filter
      abun_thresh <- input$abunFilter / 100
      rel_abun    <- colMeans(sweep(data, 1, rowSums(data) + 1e-12, "/"))
      data <- data[, rel_abun >= abun_thresh, drop = FALSE]

      n_after <- ncol(data)

      # Normalisation
      data <- switch(input$normMethod,
        "none"   = data,
        "rel"    = {
          rs <- rowSums(data); rs[rs == 0] <- 1
          sweep(data, 1, rs, "/")
        },
        "clr"    = {
          pseudo <- data + 0.5
          log(pseudo) - rowMeans(log(pseudo))
        },
        "rarefy" = {
          depth <- if (!is.na(input$rarefyDepth) && input$rarefyDepth > 0)
            input$rarefyDepth else min(rowSums(data))
          if (depth <= 0) stop("Rarefaction depth must be > 0.")
          t(apply(data, 1, function(x) {
            if (sum(x) == 0) return(x)
            probs <- x / sum(x)
            as.numeric(stats::rmultinom(1, depth, probs))
          }))
        }
      )

      rv$normData <- data

      output$preprocessSummary <- renderText({
        paste0("Preprocessing complete.\n",
               "Taxa before: ", n_start, "  →  after: ", n_after, "\n",
               "Normalisation: ", input$normMethod, "\n",
               "Final dimensions: ", nrow(data), " samples × ", ncol(data), " taxa")
      })

      output$filteredDataPreview <- DT::renderDataTable({
        DT::datatable(
          as.data.frame(data[1:min(8, nrow(data)), 1:min(12, ncol(data))]),
          options = list(scrollX = TRUE, pageLength = 5)
        )
      })

    }, error = function(e) {
      output$preprocessSummary <- renderText(paste("Error:", e$message))
    })
  })

  # ── Tab 4: Taxonomic Composition ───────────────────────────────────────────
  observeEvent(input$plotTaxonomy, {
    req(activeData())

    tryCatch({
      gvar <- if (input$taxGroupVar == "") NULL else input$taxGroupVar
      p <- plotTaxonomicAbundance(
        activeData(),
        sample_info = rv$sampleInfo,
        group_var   = gvar,
        top_n       = input$taxTopN,
        normalize   = input$taxNormalize,
        palette     = input$taxPalette
      )
      rv$lastTaxPlot <- p
      output$taxAbundancePlot <- plotly::renderPlotly({ plotly::ggplotly(p) })
    }, error = function(e) {
      showNotification(paste("Error:", e$message), type = "error")
    })
  })

  # ── Tab 5: Beta Diversity ──────────────────────────────────────────────────
  observeEvent(input$runBetaAnalysis, {
    req(activeData())

    tryCatch({
      data <- activeData()
      rv$betaDist     <- calculateBetaDiversity(data, method = input$betaDistMethod)
      rv$ordinationRes <- performOrdination(rv$betaDist, method = input$ordinationMethod)

      cvar <- if (input$betaColorVar == "") NULL else input$betaColorVar
      p <- plotOrdinationBiplot(
        rv$ordinationRes,
        sample_info   = rv$sampleInfo,
        color_var     = cvar,
        ellipse       = input$betaEllipse,
        label_samples = input$betaLabels
      )
      rv$lastOrdPlot <- p
      output$ordinationPlot <- renderPlot({ print(p) })

      # PERMANOVA
      output$permanovaResult <- renderText({
        if (!is.null(rv$sampleInfo) && !is.null(cvar)) {
          tryCatch({
            group_vec <- rv$sampleInfo[[cvar]]
            perm <- vegan::adonis2(rv$betaDist ~ group_vec)
            capture.output(print(perm))
          }, error = function(e) paste("PERMANOVA error:", e$message))
        } else {
          "Upload metadata and select a group variable to run PERMANOVA."
        }
      })

      output$distMatrixTable <- DT::renderDataTable({
        m <- as.matrix(rv$betaDist)
        idx <- 1:min(10, nrow(m))
        DT::datatable(round(as.data.frame(m[idx, idx]), 4),
                      options = list(scrollX = TRUE, pageLength = 5))
      })

    }, error = function(e) {
      showNotification(paste("Beta diversity error:", e$message), type = "error")
    })
  })

  # ── Tab 6: Alpha Diversity ─────────────────────────────────────────────────
  observeEvent(input$calcAlpha, {
    req(activeData())
    req(length(input$alphaIndices) > 0)

    tryCatch({
      data <- activeData()

      rv$alphaResults <- calculateAlphaDiversity(
        data,
        indices  = input$alphaIndices,
        rarefied = input$alphaRarefied
      )

      # Rarefaction curves
      rare_p <- advancedRarefactionPlot(data, indices = input$alphaIndices)
      rv$lastRarePlot <- rare_p
      output$rarefactionCurvePlot <- renderPlot({ print(rare_p) })

      # Diversity table
      output$alphaDivTable <- DT::renderDataTable({
        DT::datatable(rv$alphaResults,
                      options = list(scrollX = TRUE, pageLength = 10))
      })

      # Boxplot (only if metadata and grouping available)
      gvar <- if (input$alphaGroupVar == "") NULL else input$alphaGroupVar
      idx  <- input$alphaIndex4Plot
      if (is.null(idx) || idx == "") idx <- input$alphaIndices[1]

      if (!is.null(gvar) && !is.null(rv$sampleInfo)) {
        result <- tryCatch(
          plotAlphaDiversityBoxplot(
            rv$alphaResults, rv$sampleInfo, gvar,
            index      = idx,
            plot_type  = input$alphaPlotType,
            add_points = input$alphaAddPoints
          ),
          error = function(e) list(plot = NULL, test_result = e$message)
        )
        rv$lastAlphaPlot <- result$plot
        output$alphaDivBoxplot  <- renderPlot({ req(result$plot); print(result$plot) })
        output$alphaTestResult  <- renderText({
          if (inherits(result$test_result, "htest"))
            capture.output(print(result$test_result))
          else
            as.character(result$test_result)
        })
      } else {
        output$alphaDivBoxplot <- renderPlot({
          message("Upload metadata and select a group variable to generate boxplots.")
        })
        output$alphaTestResult <- renderText(
          "Select a metadata group variable to run statistical tests."
        )
      }

    }, error = function(e) {
      showNotification(paste("Alpha diversity error:", e$message), type = "error")
    })
  })

  # ── Tab 7: Differential Abundance ─────────────────────────────────────────
  observeEvent(input$runDE, {
    req(rv$microbiomeData)
    req(input$deGroupVar != "")
    req(!is.null(rv$sampleInfo))

    tryCatch({
      groups <- rv$sampleInfo[[input$deGroupVar]]
      n_groups <- length(unique(groups))
      if (n_groups != 2) {
        showNotification(
          paste0("Differential analysis requires exactly 2 groups, found ", n_groups, "."),
          type = "error"
        )
        return()
      }

      # DE analysis expects rows=taxa, cols=samples -> transpose
      count_mat   <- t(rv$microbiomeData)
      storage.mode(count_mat) <- "integer"
      conditions  <- factor(groups)

      rv$deResults <- performDifferentialExpression(
        count_mat, conditions,
        analysisType   = input$deAnalysisType,
        countThreshold = input$deCountThreshold,
        minSamples     = input$deMinSamples
      )

      # Flatten result to data.frame
      flat <- tryCatch({
        res <- rv$deResults[[1]]
        if (inherits(res, "DESeqResults")) {
          df <- as.data.frame(res)
          df$taxon <- rownames(df)
          df
        } else {
          # edgeR TopTags
          df <- as.data.frame(res$table)
          df$taxon <- rownames(df)
          # Rename to standard names for plotVolcano
          if ("logFC" %in% colnames(df)) {
            df$log2FoldChange <- df$logFC
            df$pvalue         <- df$PValue
          }
          df
        }
      }, error = function(e) {
        as.data.frame(rv$deResults[[1]])
      })
      rv$deResultsFlat <- flat

      output$deResultsTable <- DT::renderDataTable({
        DT::datatable(flat,
                      options = list(scrollX = TRUE, pageLength = 15),
                      filter = "top")
      })

      # Volcano plot
      fc_col   <- if ("log2FoldChange" %in% colnames(flat)) "log2FoldChange" else "logFC"
      pval_col <- if ("pvalue" %in% colnames(flat)) "pvalue" else "PValue"
      lab_col  <- if ("taxon" %in% colnames(flat)) "taxon" else NULL

      vol_p <- plotVolcano(flat,
                           fc_col        = fc_col,
                           pval_col      = pval_col,
                           fc_threshold  = input$deFCThreshold,
                           pval_threshold= input$dePvalThreshold,
                           label_col     = lab_col)
      rv$lastVolcPlot <- vol_p
      output$volcanoPlot <- renderPlot({ print(vol_p) })

    }, error = function(e) {
      showNotification(paste("DE analysis error:", e$message), type = "error")
    })
  })

  # ── Tab 8: Correlation ─────────────────────────────────────────────────────
  observeEvent(input$runCorr, {
    req(activeData())

    tryCatch({
      data <- activeData()

      # Select top-N by variance
      variances <- apply(data, 2, var)
      top_taxa  <- names(sort(variances, decreasing = TRUE))[
        seq_len(min(input$corrTopN, ncol(data)))]
      sub_data  <- data[, top_taxa, drop = FALSE]

      rv$corrMatrix <- calculateCorrelation(
        sub_data,
        method         = input$corrMethod,
        min_prevalence = input$corrMinPrev / 100
      )

      corr_p <- plotMicrobiomeHeatmap(
        rv$corrMatrix$correlation,
        normalize     = FALSE,
        cluster_rows  = TRUE,
        cluster_cols  = TRUE,
        color_palette = NULL
      )
      rv$lastCorrPlot <- corr_p
      output$corrHeatmapPlot <- renderPlot({
        ComplexHeatmap::draw(corr_p)
      })

    }, error = function(e) {
      showNotification(paste("Correlation error:", e$message), type = "error")
    })
  })

  # ── Tab 9: Heatmap ─────────────────────────────────────────────────────────
  observeEvent(input$generateHeatmap, {
    req(activeData())

    tryCatch({
      data <- activeData()

      # Subset top-N most variable taxa
      variances <- apply(data, 2, var)
      top_taxa  <- names(sort(variances, decreasing = TRUE))[
        seq_len(min(input$heatmapTopN, ncol(data)))]
      sub_data  <- data[, top_taxa, drop = FALSE]

      selected_palette <- switch(input$colorPalette,
        "Default" = NULL,
        RColorBrewer::brewer.pal(9, input$colorPalette)
      )

      heat_p <- plotMicrobiomeHeatmap(
        sub_data,
        normalize     = input$normalizeData,
        cluster_rows  = input$clusterRows,
        cluster_cols  = input$clusterCols,
        color_palette = selected_palette
      )
      rv$lastHeatPlot <- heat_p
      output$heatmapPlot <- renderPlot({
        ComplexHeatmap::draw(heat_p)
      })

    }, error = function(e) {
      showNotification(paste("Heatmap error:", e$message), type = "error")
    })
  })

  # ── Tab 11: ML Classification ───────────────────────────────────────────
  observe({
    req(rv$sampleInfo)
    updateSelectInput(session, "mlGroupVar", choices = colnames(rv$sampleInfo))
  })

  observeEvent(input$runRF, {
    req(activeData(), rv$sampleInfo, input$mlGroupVar != "")
    tryCatch({
      rv$rfResult <- performRandomForest(
        activeData(), rv$sampleInfo, input$mlGroupVar,
        ntree = input$mlNtree, test_fraction = input$mlTestFrac
      )
      output$rfSummary <- renderText({
        rf <- rv$rfResult
        paste0("Random Forest Results\n",
               "=====================\n",
               "Test accuracy: ", round(rf$accuracy * 100, 1), "%\n",
               "OOB error:     ", round(rf$oob_error * 100, 1), "%\n",
               "Train samples: ", length(rf$train_indices), "\n",
               "Test samples:  ", length(rf$test_indices), "\n",
               "Features used: ", ncol(activeData()))
      })
      imp_p <- plotFeatureImportance(rv$rfResult, top_n = 20)
      rv$lastImportPlot <- imp_p
      output$rfImportancePlot <- renderPlot({ print(imp_p) })
      output$rfConfusionTable <- DT::renderDataTable({
        DT::datatable(as.data.frame.matrix(rv$rfResult$confusion_matrix))
      })
    }, error = function(e) {
      showNotification(paste("RF error:", e$message), type = "error")
    })
  })

  observeEvent(input$runCV, {
    req(activeData(), rv$sampleInfo, input$mlGroupVar != "")
    tryCatch({
      rv$classResult <- performClassification(
        activeData(), rv$sampleInfo, input$mlGroupVar,
        n_folds = input$mlNfolds, n_repeats = input$mlNrepeats,
        ntree = input$mlNtree
      )
      output$rfSummary <- renderText({
        cv <- rv$classResult
        paste0("Cross-Validation Results\n",
               "========================\n",
               "Mean accuracy: ", round(cv$cv_accuracy * 100, 1), "% (+/- ",
                 round(cv$cv_accuracy_sd * 100, 1), "%)\n",
               "AUC:           ", if (!is.na(cv$auc)) round(cv$auc, 3) else "N/A", "\n",
               "Folds: ", input$mlNfolds, " x ", input$mlNrepeats, " repeats")
      })
      if (!is.null(rv$classResult$roc_data)) {
        roc_p <- plotROCCurve(rv$classResult)
        rv$lastROCPlot <- roc_p
        output$rocCurvePlot <- renderPlot({ print(roc_p) })
      }
    }, error = function(e) {
      showNotification(paste("CV error:", e$message), type = "error")
    })
  })

  # ── Tab 12: Biomarkers & Clustering ────────────────────────────────────────
  observe({
    req(rv$sampleInfo)
    updateSelectInput(session, "bmGroupVar", choices = colnames(rv$sampleInfo))
  })

  observeEvent(input$runBiomarkers, {
    req(activeData(), rv$sampleInfo, input$bmGroupVar != "")
    tryCatch({
      rv$biomarkerResult <- discoverBiomarkers(
        activeData(), rv$sampleInfo, input$bmGroupVar,
        method = input$bmMethod,
        pval_threshold = input$bmPvalThresh,
        lda_threshold = input$bmLdaThresh
      )
      bm_p <- plotBiomarkers(rv$biomarkerResult, top_n = 20)
      rv$lastBiomarkerPlot <- bm_p
      output$biomarkerPlot <- renderPlot({ print(bm_p) })
      output$biomarkerTable <- DT::renderDataTable({
        DT::datatable(rv$biomarkerResult$biomarkers,
                      options = list(scrollX = TRUE, pageLength = 15))
      })
    }, error = function(e) {
      showNotification(paste("Biomarker error:", e$message), type = "error")
    })
  })

  observeEvent(input$runClustering, {
    req(activeData())
    tryCatch({
      k_val <- if (is.na(input$clK)) NULL else input$clK
      rv$clusterResult <- performClustering(
        activeData(), method = input$clMethod,
        k = k_val, max_k = input$clMaxK
      )
      output$clusterSummary <- renderText({
        cl <- rv$clusterResult
        paste0("Clustering Results\n",
               "==================\n",
               "Method: ", cl$method, "\n",
               "K: ", cl$k, "\n",
               "Avg silhouette: ", round(cl$avg_silhouette, 3), "\n",
               "Cluster sizes: ", paste(table(cl$cluster_assignments), collapse = ", "))
      })
      # Show clusters on ordination
      ord_res <- performOrdination(activeData(), method = "PCoA")
      cluster_meta <- data.frame(
        Cluster = factor(rv$clusterResult$cluster_assignments),
        row.names = names(rv$clusterResult$cluster_assignments)
      )
      cl_p <- plotOrdinationBiplot(ord_res, sample_info = cluster_meta,
                                    color_var = "Cluster", ellipse = TRUE)
      output$clusterOrdPlot <- renderPlot({ print(cl_p) })
    }, error = function(e) {
      showNotification(paste("Clustering error:", e$message), type = "error")
    })
  })

  # ── Tab 13: Network Analysis ──────────────────────────────────────────────
  observeEvent(input$buildNetwork, {
    req(activeData())
    tryCatch({
      data <- activeData()
      variances <- apply(data, 2, var)
      top_taxa <- names(sort(variances, decreasing = TRUE))[
        seq_len(min(input$netTopTaxa, ncol(data)))]
      sub_data <- data[, top_taxa, drop = FALSE]

      rv$networkResult <- buildCooccurrenceNetwork(
        sub_data,
        method = input$netCorrMethod,
        cor_threshold = input$netCorThresh,
        pval_threshold = input$netPvalThresh,
        min_prevalence = input$netPrev / 100
      )

      net_p <- plotCooccurrenceNetwork(rv$networkResult, layout = input$netLayout)
      rv$lastNetworkPlot <- net_p
      output$networkPlot <- renderPlot({ print(net_p) })

      output$nodeMetricsTable <- DT::renderDataTable({
        DT::datatable(rv$networkResult$node_metrics,
                      options = list(scrollX = TRUE, pageLength = 15))
      })
      output$edgeListTable <- DT::renderDataTable({
        DT::datatable(rv$networkResult$edge_list,
                      options = list(scrollX = TRUE, pageLength = 15))
      })
    }, error = function(e) {
      showNotification(paste("Network error:", e$message), type = "error")
    })
  })

  # ── Tab 14: Export ─────────────────────────────────────────────────────────
  output$downloadPlot <- downloadHandler(
    filename = function() {
      paste0(input$exportPlotSelect, "_", Sys.Date(), ".", input$exportFormat)
    },
    content = function(file) {
      p <- switch(input$exportPlotSelect,
        "taxonomic"   = rv$lastTaxPlot,
        "ordination"  = rv$lastOrdPlot,
        "alpha_box"   = rv$lastAlphaPlot,
        "rarefaction" = rv$lastRarePlot,
        "volcano"     = rv$lastVolcPlot,
        "corr_heatmap"= rv$lastCorrPlot,
        "heatmap"     = rv$lastHeatPlot,
        "rf_importance"= rv$lastImportPlot,
        "roc_curve"   = rv$lastROCPlot,
        "biomarker"   = rv$lastBiomarkerPlot,
        "network"     = rv$lastNetworkPlot
      )
      if (is.null(p)) {
        showNotification("Generate this plot first before exporting.", type = "warning")
        if (input$exportFormat == "pdf") {
          grDevices::pdf(file, width = 4, height = 2)
        } else if (input$exportFormat == "png") {
          grDevices::png(file, width = 400, height = 200)
        } else {
          grDevices::svg(file, width = 4, height = 2)
        }
        graphics::plot.new()
        graphics::text(0.5, 0.5, "No plot generated yet.", cex = 1.5)
        grDevices::dev.off()
        return()
      }
      if (inherits(p, "Heatmap") || inherits(p, "HeatmapList")) {
        # ComplexHeatmap
        grDevices::pdf(NULL)
        grDevices::dev.off()
        if (input$exportFormat == "pdf") {
          grDevices::pdf(file, width = input$exportWidth, height = input$exportHeight)
        } else if (input$exportFormat == "png") {
          grDevices::png(file, width = input$exportWidth, height = input$exportHeight,
                         units = "in", res = 150)
        } else {
          grDevices::svg(file, width = input$exportWidth, height = input$exportHeight)
        }
        ComplexHeatmap::draw(p)
        grDevices::dev.off()
      } else {
        ggplot2::ggsave(file, plot = p,
                        width  = input$exportWidth,
                        height = input$exportHeight,
                        device = input$exportFormat)
      }
    }
  )

  output$downloadTable <- downloadHandler(
    filename = function() {
      paste0(input$exportTableSelect, "_", Sys.Date(), ".csv")
    },
    content = function(file) {
      tbl <- switch(input$exportTableSelect,
        "alpha" = rv$alphaResults,
        "de"    = rv$deResultsFlat,
        "dist"  = if (!is.null(rv$betaDist)) as.data.frame(as.matrix(rv$betaDist)) else NULL,
        "corr"  = if (!is.null(rv$corrMatrix)) as.data.frame(rv$corrMatrix$correlation) else NULL,
        "norm"  = if (!is.null(rv$normData)) as.data.frame(rv$normData) else NULL,
        "raw"   = if (!is.null(rv$microbiomeData)) as.data.frame(rv$microbiomeData) else NULL
      )
      if (is.null(tbl)) {
        showNotification("No data available for this table. Run the analysis first.", type = "warning")
        write.csv(data.frame(message = "No data"), file, row.names = FALSE)
        return(NULL)
      }
      write.csv(tbl, file, row.names = TRUE)
    }
  )

}

# ── Launch ────────────────────────────────────────────────────────────────────
shinyApp(ui = ui, server = server)
