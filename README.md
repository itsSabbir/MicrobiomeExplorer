# MicrobiomeExplorer

## Description

MicrobiomeExplorer is an R package designed for streamlined analysis of microbiome data. It offers a comprehensive suite of tools for processing, analysis, and high-quality visualization of microbial communities. The package is tailored to work with data from 16S rRNA gene sequencing, metagenomics, and metatranscriptomics, and aims to facilitate integrated analyses within the bioinformatics workflow. Developed for cross-platform compatibility, MicrobiomeExplorer introduces novel visualization techniques to provide dynamic and in-depth views of microbiota shifts and patterns.

## Installation

To install the latest version of MicrobiomeExplorer directly from GitHub, execute the following commands in R:

```r
install.packages("devtools")
library("devtools")
devtools::install_github("itsSabbir/MicrobiomeExplorer", build_vignettes = TRUE)
library(MicrobiomeExplorer)

```

## Overview
MicrobiomeExplorer offers a suite of functions that facilitate the exploration and analysis of microbiome data. 
*Data preprocessing and normalization.
*Taxonomic classification and abundance estimation.
*Biodiversity analysis and comparative analysis.
*Functional profiling of microbial communities.
*Advanced visualization of analysis results.

To list the functions and datasets provided by MicrobiomeExplorer, use:

```r

ls("package:MicrobiomeExplorer")
data(package = "MicrobiomeExplorer") # if applicable

```

For a comprehensive tutorial on how to use MicrobiomeExplorer, check out the vignettes:

```r
browseVignettes("MicrobiomeExplorer")
```



## Contributions
The MicrobiomeExplorer package was developed by Sabbir Hossain. The development of this package was inspired and informed by various established tools and methods within the bioinformatics community. Contributions from existing R packages and other sources are duly noted for each function within the package.This package synergizes with tools like phyloseq for data management, ggplot2 for visualization, vegan for diversity analysis, dplyr and tidyr for data manipulation. The aim is to enhance workflow efficiency in microbiome studies.



## References
MicrobiomeExplorer integrates methods and insights from various sources, ensuring a robust and versatile tool for the scientific community:

## References

MicrobiomeExplorer integrates methods and insights from various sources, ensuring a robust and versatile tool for the scientific community. Key references include:

- McMurdie, P.J., and Holmes, S. (2013). [phyloseq: An R Package for Reproducible Interactive Analysis and Graphics of Microbiome Census Data](https://journals.plos.org/plosone/article?id=10.1371/journal.pone.0061217). PLoS ONE 8(4): e61217.
- Wickham, H. (2016). [ggplot2: Elegant Graphics for Data Analysis](https://ggplot2.tidyverse.org/reference/ggplot2-package.html). Springer-Verlag New York.
- Oksanen, J., et al. (2019). [vegan: Community Ecology Package](https://cran.r-project.org/web/packages/vegan/index.html). R package version 2.5-6.

Additional literature and resources that have contributed to the development of MicrobiomeExplorer:

- Callahan, B.J., et al. (2016). [DADA2: High-resolution sample inference from Illumina amplicon data](https://www.nature.com/articles/nmeth.3869). Nature Methods, 13(7), 581-583.
- Paulson, J.N., et al. (2013). [Differential abundance analysis for microbial marker-gene surveys](https://www.nature.com/articles/nmeth.2658). Nature Methods, 10(12), 1200-1202.
- Love, M.I., Huber, W., & Anders, S. (2014). [Moderated estimation of fold change and dispersion for RNA-seq data with DESeq2](https://genomebiology.biomedcentral.com/articles/10.1186/s13059-014-0550-8). Genome Biology, 15(12), 550.


## Acknowledgements
This package was developed as part of an assessment for 2023 BCB410H: Applied Bioinformatics course at the University of Toronto, Toronto, CANADA. MicrobiomeExplorer welcomes issues,
enhancement requests, and other contributions. To submit an issue, use the GitHub issues.

## Equity, Diversity, and Inclusion (EDI)
MicrobiomeExplorer is committed to EDI principles, providing accessible documentation and actively seeking community feedback to cater to a broad audience. Challenges such as data bias, accessibility, and global representation are acknowledged and addressed to the best of the developers' abilities.

## Reproducibility

(PENDING)
Reproducibility is ensured through rigorous version control, comprehensive documentation, clear specification of dependencies, automated testing, and continuous integration. A Docker container is also provided to facilitate a consistent and controlled environment for all users.
