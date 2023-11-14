
<!-- README.md is generated from README.Rmd. Please edit that file -->

# MicrobiomeExplorer

## Description

`MicrobiomeExplorer` is designed to facilitate the analysis of
microbiome data, helping scientists and researchers to gain insights
into microbial communities. Through this package, users can process,
analyze, and visualize data from 16S rRNA gene sequencing, metagenomic,
and metatranscriptomic studies. The goal is to provide a comprehensive
toolset that improves upon existing workflows by integrating multiple
data types and offering advanced graphical outputs. This package was
developed using R 4.1.0 (Platform information) and is intended to be
platform-independent, supporting macOS, Windows, and Linux
distributions.

## Installation

To install the current version of `MicrobiomeExplorer` from GitHub, use:

``` r
install.packages("devtools")
library("devtools")
devtools::install_github("itsSabbir/MicrobiomeExplorer", build_vignettes = TRUE)
library(MicrobiomeExplorer)
```

To run the shinyApp: Under construction

## Overview

`MicrobiomeExplorer` offers a suite of functions that facilitate the
exploration and analysis of microbiome data. Here is a list of the main
functions along with a brief description:

- `calculate_stats`: Calculates extended statistics (mean, median,
  standard deviation, variance, range, and interquartile range) for each
  feature in a microbiome dataset, providing a comprehensive statistical
  overview essential for preliminary data analysis.

- `plot_microbiome_heatmap`: Creates a comprehensive heatmap for
  microbiome data analysis, with support for normalization, various
  clustering methods, and enhanced visualization options using the
  `ComplexHeatmap` package.

- `plot_rarefaction`: Generates rarefaction curves for microbiome count
  data, useful for comparing species richness among samples with
  customizable sampling and plot aesthetics.

- `calculate_alpha_diversity`: Computes various alpha diversity indices
  (like Shannon, Simpson, Chao1, and ACE) for each sample in a
  microbiome dataset, facilitating in-depth ecological and diversity
  studies.

Each function is designed with robustness in mind, ensuring that they
handle missing values appropriately, provide flexible clustering
options, and allow for extensive plot customization to aid in data
interpretation.

To list the functions and datasets provided by `MicrobiomeExplorer`,
use:

``` r
ls("package:MicrobiomeExplorer")

data(package = "MicrobiomeExplorer") # if applicable
```

For a comprehensive tutorial on how to use MicrobiomeExplorer, check out
the vignettes:

``` r
browseVignettes("MicrobiomeExplorer")
```

## Contributions

The MicrobiomeExplorer package was developed by Sabbir Hossain. The
development of this package was inspired and informed by various
established tools and methods within the bioinformatics community.
Contributions from existing R packages and other sources are duly noted
for each function within the package.

- **phyloseq**: Enhances data management, particularly in functions like
  `calculate_stats` and `calculate_alpha_diversity`, offering robust
  handling of complex microbiome datasets.

- **ggplot2**: Underpins the visualization aspects in
  `plot_microbiome_heatmap` and `plot_rarefaction`, providing elegant
  and informative data representation.

- **vegan**: Contributes to diversity analysis, especially in
  `calculate_alpha_diversity` and `plot_rarefaction`, delivering
  extensive ecological insights.

- **dplyr** and **tidyr**: Facilitate efficient data manipulation across
  the package, crucial for preprocessing and organizing data for
  analysis.

## References

MicrobiomeExplorer integrates methods and insights from various sources,
ensuring a robust and versatile tool for the scientific community. Key
references include:

- McMurdie, P.J., and Holmes, S. (2013). [phyloseq: An R Package for
  Reproducible Interactive Analysis and Graphics of Microbiome Census
  Data](https://journals.plos.org/plosone/article?id=10.1371/journal.pone.0061217).
  PLoS ONE 8(4): e61217.
- Wickham, H. (2016). [ggplot2: Elegant Graphics for Data
  Analysis](https://ggplot2.tidyverse.org/reference/ggplot2-package.html).
  Springer-Verlag New York.
- Oksanen, J., et al. (2019). [vegan: Community Ecology
  Package](https://cran.r-project.org/web/packages/vegan/index.html). R
  package version 2.5-6.

Additional literature and resources that have contributed to the
development of MicrobiomeExplorer:

- Callahan, B.J., et al. (2016). [DADA2: High-resolution sample
  inference from Illumina amplicon
  data](https://www.nature.com/articles/nmeth.3869). Nature Methods,
  13(7), 581-583.
- Paulson, J.N., et al. (2013). [Differential abundance analysis for
  microbial marker-gene
  surveys](https://www.nature.com/articles/nmeth.2658). Nature Methods,
  10(12), 1200-1202.
- Love, M.I., Huber, W., & Anders, S. (2014). [Moderated estimation of
  fold change and dispersion for RNA-seq data with
  DESeq2](https://genomebiology.biomedcentral.com/articles/10.1186/s13059-014-0550-8).
  Genome Biology, 15(12), 550.

## Acknowledgements

This package was developed as part of an assessment for 2023 BCB410H:
Applied Bioinformatics course at the University of Toronto, Toronto,
CANADA. MicrobiomeExplorer welcomes issues, enhancement requests, and
other contributions. To submit an issue, use the GitHub issues.

## Equity, Diversity, and Inclusion (EDI)

MicrobiomeExplorer is committed to EDI principles, providing accessible
documentation and actively seeking community feedback to cater to a
broad audience. Challenges such as data bias, accessibility, and global
representation are acknowledged and addressed to the best of the
developers’ abilities.

## Reproducibility

(PENDING) Reproducibility is ensured through rigorous version control,
comprehensive documentation, clear specification of dependencies,
automated testing, and continuous integration. A Docker container is
also provided to facilitate a consistent and controlled environment for
all users.

## Package Structure

The MicrobiomeExplorer package is organized as follows:

``` r
- MicrobiomeExplorer
  |- DESCRIPTION
  |- LICENSE
  |- LICENSE.md
  |- MicrobiomeExplorer.Rproj
  |- MicrobiomeExplorer_utilities.R
  |- NAMESPACE
  |- README.Rmd
  |- README.md
  |- samples.R
  |- data/
  |- inst/
  |- man/
  |- R/
  |- tests/
  |- vignettes/
  
```
