#| output: false

library("tidyverse")


#| output: false

# Set path for processed data directory
processed_data_dir <- file.path("..","data")

# Load the expr dataset
expr_data <- read_tsv(file = file.path(processed_data_dir,
                                         "02.1_dat_clean_expr.tsv"))

# Apply above described augmentatitons
expr_data_augment <- expr_data |>
  rename_with(~ sub("00000", "", .), starts_with("ENSG")) |>
  rename_with(~ gsub("ENS|_at", "", .), starts_with("ENSG"))


#| output: false

# Load the pheno dataset
pheno_data <- read_tsv(file = file.path(processed_data_dir,
                                         "02.2_dat_clean_pheno.tsv"))

# extract above columns
pheno_data_filter <- pheno_data |>
  rename(sample = geo_accession,
         species = organism_ch1,
         age = `age:ch1`,
         sex = `sex:ch1`,
         tissue = `tissue:ch1`) |>
  select(sample, species, age, sex, tissue)


#| output: false

pheno_data_augment <- pheno_data_filter |>
  mutate(age = gsub("\\D", "", age) |>
           parse_number(),
         sex = case_when(sex == "Female" ~ "F",
                         sex == "Male" ~ "M"),
         species = case_when(species == "Homo sapiens" ~ "Human",
                             species == "Pan troglodytes" ~ "Chimpanzee",
                             species == "Macaca mulatta" ~ "Macaque"))


#| output: false

pheno_expr_data_joined <- full_join(pheno_data_augment,
                                    expr_data_augment,
                                    by = "sample")


#| output: false

pheno_expr_data_joined |>
  write_tsv(file = file.path(processed_data_dir,
                             "03_dat_joined.tsv"))

