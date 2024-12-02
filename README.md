---
editor_options: 
  markdown: 
    wrap: 72
---

# Project Contributors

s244028, Sjofski,

s225099, Soph-DTU,

s241901, madelineshah4,

s204668, NorbertAuroraBalint,

s233415, bbd52

# Presentation Link

Click the following link to access the presentation:

[Presentation](https://raw.githack.com/rforbiodatascience24/group_07_project/main/doc/presentation.html)

# Data Retrieval

In order to load our dataset, run the script 01_load.qmd. It loads the
data using the `GEOquery` package from the [Bioconductor
Website](https://www.bioconductor.org/packages//2.10/bioc/html/GEOquery.html).

# Our Code

We have split up our code into several subsections to organize the
workflow. We start with loading, cleaning, and augmenting the data to
prepare it for analysis. Then we perform a PCA, a cube analysis, a
multi-linear correlation model, and a heterochrony analysis. After that,
we have several files for plots following the analyses. These combined
make up the framework for our results.

As a note, to ensure the proper calling of select(), we use dplyr::select() throughout the script. 

## Libraries Used

-  `GEOquery`
-  `Biobase`
-  `dplyr`
-  `broom`
-  `MASS`
-  `tidyverse`
-  `cowplot`
-  `ggtext`
-  `gridtext`
-  `patchwork`
-  `RColorBrewer`
-  `ggplot2`
