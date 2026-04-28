FROM bioconductor/bioconductor_docker:RELEASE_3_20

RUN apt-get update && apt-get install -y --no-install-recommends \
    libcurl4-openssl-dev \
    libssl-dev \
    libxml2-dev \
    libfontconfig1-dev \
    libfreetype6-dev \
    libpng-dev \
    libtiff5-dev \
    libjpeg-dev \
    libglpk-dev \
    && rm -rf /var/lib/apt/lists/*

RUN R -e "BiocManager::install(c('DESeq2', 'edgeR', 'limma', 'phyloseq', 'ComplexHeatmap'), ask = FALSE, update = FALSE)"

RUN R -e "install.packages(c( \
    'shiny', 'shinydashboard', 'shinyWidgets', 'shinycssloaders', \
    'ggplot2', 'plotly', 'DT', 'tidyr', 'vegan', 'ape', \
    'ggrepel', 'RColorBrewer', 'rlang', \
    'randomForest', 'igraph', 'Rtsne', 'umap', 'pROC', \
    'cluster', 'dbscan' \
    ), repos = 'https://cloud.r-project.org')"

COPY . /app/MicrobiomeExplorer
WORKDIR /app/MicrobiomeExplorer

RUN R CMD INSTALL .

EXPOSE 3838

CMD ["R", "-e", "MicrobiomeExplorer::runMicrobiomeExplorerApp(host = '0.0.0.0', port = 3838)"]
