#| output: false

library("tidyverse")


#| output: false

# Set path for processed data directory
processed_data_dir <- file.path("..","data")

# Read the expression data
expr_data <- read.table(file = file.path(processed_data_dir,
                                         "01.1_dat_load_expr.tsv"),
                        sep = "\t",
                        header = TRUE,
                        row.names = 1,
                        check.names = FALSE)

#Tidy expression data
expr_data_tidy <- expr_data |>
  as_tibble(rownames = "gene")

# Reshape expression data to long format
expr_data_tidy_long <- expr_data_tidy |>
  pivot_longer(cols = -gene,
               names_to = "sample",
               values_to = "expression")

# Convert back to wide format
expr_data_tidy_wide <- expr_data_tidy_long |>
  pivot_wider(names_from = gene,
              values_from = expression)

# Read the phenotype data
pheno_data <- read.table(file = file.path(processed_data_dir,
                                         "01.2_dat_load_pheno.tsv"),
                        sep = "\t",
                        header = TRUE,
                        row.names = 1,
                        check.names = FALSE)

# Tidy phenotype data
pheno_data_tidy <- pheno_data |>
  as_tibble()


#| output: false

# Write cleaned expression data to TSV file
expr_data_tidy_wide |>
  write_tsv(file = file.path(processed_data_dir,
                             "02.1_dat_clean_expr.tsv"))

# Write cleaned expression data to TSV file
pheno_data_tidy |>
  write_tsv(file = file.path(processed_data_dir,
                             "02.2_dat_clean_pheno.tsv"))

