#| output: false

library("GEOquery")
library("Biobase")
library("tidyverse")


#| output: false

gset <- getGEO("GSE11512", GSEMatrix = TRUE, getGPL = FALSE) |>
  pluck(1)


#| output: false

# Set path for raw data directory and check if exists
raw_data_dir <- file.path("..", "data", "_raw")
if (!dir.exists(raw_data_dir)) {
  dir.create(raw_data_dir, recursive = TRUE)
  message("Created directory: ", raw_data_dir)}

saveRDS(gset, file = file.path(raw_data_dir, "GSE11512.RDS"))


#| output: false

# Set path for processed data directory
processed_data_dir <- file.path("..","data")

# Write tsv file for expression data
expr_data <- exprs(gset)
write.table(expr_data,
            file = file.path(processed_data_dir,
                             "01.1_dat_load_expr.tsv"),
            sep = "\t",
            row.names = TRUE,
            col.names = NA,
            quote = FALSE)

# Write tsv file for phenotype data
pheno_data <- pData(phenoData(gset))
write.table(pheno_data,
            file = file.path(processed_data_dir,
                             "01.2_dat_load_pheno.tsv"),
            sep = "\t",
            row.names = TRUE,
            col.names = NA,
            quote = TRUE)

