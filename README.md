# MicrobiomeExplorer

## Description

MicrobiomeExplorer is an R package designed for streamlined analysis of microbiome data. Its comprehensive toolset allows for efficient processing, analysis, and high-quality visualization of microbial communities derived from 16S rRNA gene sequencing, metagenomics, and metatranscriptomics. This package specifically addresses the need for integrated analysis in the bioinformatics workflow, introducing novel functionalities that complement and enhance existing methods. Developed on R version 4.1.0 (insert your platform, e.g., macOS Big Sur 10.16), MicrobiomeExplorer is engineered to support cross-platform compatibility.

## Installation

To install the latest version of MicrobiomeExplorer directly from GitHub, execute the following commands in R:

```r
install.packages("devtools")
library("devtools")
devtools::install_github("itsSabbir/MicrobiomeExplorer", build_vignettes = TRUE)
library(MicrobiomeExplorer)
```

## Overview
MicrobiomeExplorer offers a suite of functions that facilitate the exploration and analysis of microbiome data. To list the functions and datasets provided by MicrobiomeExplorer, use:

```r

ls("package:MicrobiomeExplorer")
data(package = "MicrobiomeExplorer") # if applicable

```

For a comprehensive tutorial on how to use MicrobiomeExplorer, check out the vignettes:

```r
browseVignettes("MicrobiomeExplorer")
```


## Contributions
The MicrobiomeExplorer package was developed by Sabbir Hossain. The development of this package was inspired and informed by various established tools and methods within the bioinformatics community. Contributions from existing R packages and other sources are duly noted for each function within the package.

## References
Please include full references to all tools, packages, or literature you utilized or adapted in developing MicrobiomeExplorer.

## Acknowledgements
This package was developed as part of the 2023 BCB410H: Applied Bioinformatics course at the University of Toronto, Toronto, CANADA. The development of MicrobiomeExplorer has benefited from the contributions and input from peers and the instructive support of Anjali Silva.

## Other Topics
