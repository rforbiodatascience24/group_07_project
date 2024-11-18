# Project Contributors
s244028, Sjofski,

s225099, Soph-DTU,

[student id], [GitHub username],

[student id], [GitHub username],

[student id], [GitHub username]

# To do, all
- Add yourself at the top of this doc
- Check that all code commited follows https://r4bds.github.io/code_styling.htmlllows
- Provide all scripts in .qmd visual format - remember to add comments to sections
- Ensure all scripts are organized according to "Organisation" in https://r4bds.github.io/project_description.html
- Create a branch to work in, and only merge to main once code has been properly edited

# To do, to be divided - maybe into different analyses?
- Augment data, extract relevant attributes (there are two separate tibbles to do this on)
- Augment data, format attribute names and values (there are two separate tibbles to do this on)
- Augment data, join data into one tibble
- Augment data, nest tibble nest to prepare for analysis
- Augment data, add relevant variables for anlysis (e.g. age, brain section)
- Analyse data, add calculated variables (e.g. differential gene expression, age groups)
- Augment data, unnest tibble to prepare for visualization
- Visualize data, create plots, consider patchwork and The glamout of graphics (https://www.youtube.com/watch?v=h5cTacaWE6I)

Currently the data are loaded and cleaned using the libraries "GEOquery" and "Biobase". We should consider looking into making our own script, based on Biobase.


# Paper analyzing data / similar data
_https://doi.org/10.1073/pnas.0900544106_

findings from paper to display:

"We show that the brain transcriptome is dramatically remodeled during postnatal development"

"developmental changes in the human brain are indeed delayed relative to other primates"

"delay is not uniform across the human transcriptome but affects a specific subset of genes that play a potential role in neural development"

"age explains the largest part of the total expression variation at 29%, followed by species at 17% and sex at <2" - fig 1B

"71% of the 7,958 genes expressed in the human brain change significantly during postnatal development" fig 1C

"among all genes that change during prefrontal cortex development, approximately half change with age differently in the 2 species" - fig 1D

"we could confidently assign 299 to one of these 4 categories. Of these 299 genes, approximately the same proportions (15 to 25%) fall into categories ii, iii, and iv. In contrast, we find approximately twice as many genes (38%) in category i, human neotenic genes" fig 3A

"the superior frontal gyrus [...] e again find a significant excess of human neotenic genes compared with the other 3 phyloontogenetic categories" fig 3A

"our test detects a substantial delay in postnatal development of the prefrontal cortex compared with the caudate nucleus on the gene expression level" - appendix, possible side figure related to timing of differential gene expression at differing developmental states

"we find that the neotenic shift affects a limited group of genes expressed in the brain rather than the entire brain" conclusion based on fig 3 and appendices, comparing to earlier studies

# Good to know terms
Postnatal development

Ontogenesis

Multiple regression models

Heterochronic expression

Phyloontogenetic categories:

  (i) human neoteny–expression changes occurred on the human lineage, and human expression corresponds to that in younger chimpanzees

  (ii) human acceleration–expression changes occurred on the human lineage, and human expression corresponds to that in older chimpanzees

  (iii) chimpanzee neoteny

  (iv) chimpanzee acceleration
