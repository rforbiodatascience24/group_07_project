---
editor_options: 
  markdown: 
    wrap: 72
---

# Project Contributors

s244028, Sjofski,

s225099, Soph-DTU,

s241901, madelineshah4,

[student id], [GitHub username],

[student id], [GitHub username]

# To do, all

-   Add yourself at the top of this doc
-   Check that all code commited follows
    <https://r4bds.github.io/code_styling.htmlllows>
-   Provide all scripts in .qmd visual format - remember to add comments
    to sections
-   Ensure all scripts are organized according to "Organisation" in
    <https://r4bds.github.io/project_description.html>
-   Create a branch to work in, and only merge to main once code has
    been properly edited

# To do, to be divided - maybe into different analyses?
=======
- [v] Augment data, extract relevant attributes (there are two separate tibbles to do this on)
- [v] Augment data, format attribute names and values (there are two separate tibbles to do this on)
- [v] Augment data, join data into one tibble
Norb -- Augment data, nest tibble to prepare for multiple regression, extract variables, unnest tibble
Bad -- Augment data, nest tibble to prepare for heterochronic expression test, extract variables, unnest tibble
Sof -- PCA
Mad (see below) -- Visualize PCA and gene expression over age
Soph & Sof (see below )-- Visualze multiple regression and heterochronic expression test
Soph & Sof - Presentation

-   Augment data, extract relevant attributes (there are two separate
    tibbles to do this on)
-   Augment data, format attribute names and values (there are two
    separate tibbles to do this on)
-   Augment data, join data into one tibble
-   Augment data, nest tibble nest to prepare for analysis
-   Augment data, add relevant variables for anlysis (e.g. age, brain
    section)
-   Analyse data, add calculated variables (e.g. differential gene
    expression, age groups)
-   Augment data, unnest tibble to prepare for visualization
-   Visualize data, create plots, consider patchwork and The glamout of
    graphics (<https://www.youtube.com/watch?v=h5cTacaWE6I>)

Currently the data are loaded and cleaned using the libraries "GEOquery"
and "Biobase". We should consider looking into making our own script,
based on Biobase.

# Paper analyzing data / similar data

\_<https://doi.org/10.1073/pnas.0900544106>

findings from paper to display:

"We show that the brain transcriptome is dramatically remodeled during
postnatal development"

"developmental changes in the human brain are indeed delayed relative to
other primates"

"delay is not uniform across the human transcriptome but affects a
specific subset of genes that play a potential role in neural
development"

"age explains the largest part of the total expression variation at 29%,
followed by species at 17% and sex at \<2" - fig 1B

"71% of the 7,958 genes expressed in the human brain change
significantly during postnatal development" fig 1C

"among all genes that change during prefrontal cortex development,
approximately half change with age differently in the 2 species" - fig
1D

"we could confidently assign 299 to one of these 4 categories. Of these
299 genes, approximately the same proportions (15 to 25%) fall into
categories ii, iii, and iv. In contrast, we find approximately twice as
many genes (38%) in category i, human neotenic genes" fig 3A

"the superior frontal gyrus [...] e again find a significant excess of
human neotenic genes compared with the other 3 phyloontogenetic
categories" fig 3A

"our test detects a substantial delay in postnatal development of the
prefrontal cortex compared with the caudate nucleus on the gene
expression level" - appendix, possible side figure related to timing of
differential gene expression at differing developmental states

"we find that the neotenic shift affects a limited group of genes
expressed in the brain rather than the entire brain" conclusion based on
fig 3 and appendices, comparing to earlier studies

=======
Mad - "age explains the largest part of the total expression variation at 29%, followed by species at 17% and sex at <2" - fig 1B

Mad - "71% of the 7,958 genes expressed in the human brain change significantly during postnatal development" fig 1C

Mad - "among all genes that change during prefrontal cortex development, approximately half change with age differently in the 2 species" - fig 1D

Soph & Soph - "we could confidently assign 299 to one of these 4 categories. Of these 299 genes, approximately the same proportions (15 to 25%) fall into categories ii, iii, and iv. In contrast, we find approximately twice as many genes (38%) in category i, human neotenic genes" fig 3A

Soph & Sof - "our test detects a substantial delay in postnatal development of the prefrontal cortex compared with the caudate nucleus on the gene expression level" - appendix, possible side figure related to timing of differential gene expression at differing developmental states

# Good to know terms

Postnatal development

Ontogenesis

Multiple regression models

Heterochronic expression test - used to compare gene expression trajectories to to classify genes based on the relative timing of expression across species
- neotony: species 1 expression curve fits better with a significant positive age shift when compared to species 2
- acceleration: gene expression changes occur faster or earlier in species 1 compared to species 2

# Madeline's Notes on Research Paper

neoteny: developmental retardation

-   slower development in humans is a possible explanation of
    human-specific cognitive abilities

-   unknown if all genes in humans are delayed compared to other
    primates

goal: understand human evolution by analyzing gene levels in humans vs
chimps vs macaques during postnatal development

## Figure 1 Plots

Plot 1, variance explained

-   **NEED**: variance explained and expected variance explained
    (maybe?) in gene expression for age, species, and sex for each
    species (will do separate bars for each of the 3 species) - created in 5.1 > cube regression done, calculating variance explained from R squared

-   show that age explains the largest part of gene expression

-   similar to plot B in report

Plot 2, significant human gene change

- multiple regression used for fig 1c done by norbert
-   **NEED**: yes or no significant change based on age (Age +/-) and
    yes or no significant change based on species (Sp +/-) count

    -   both above booleans/counts for all genes

    -   do for humans vs macaques and humans vs chimps

-   proportions of genes showing significant differences between species

-   make 2 separate pie charts (or other plot) for humans vs each
    primate

-   differences based on age +/-

-   differences based on species +/-

-   similar to plot C in report

Plot 3, change in postnatal expression

-   plot 3 species together showing % change in ALL genes relative to
    newborns for each individual

-   100 percent is difference between newborns and oldest

-   similar to plot D in report

-   age (normalized) x axis

    -   normalize age to average (find online) life expectancy

-   for age, add an average (find online) sexual maturity line for each
    species
=======
