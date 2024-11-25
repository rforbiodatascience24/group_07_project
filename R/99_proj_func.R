
calculate_var_explained <- function(data_frame,
                                    principal_component,
                                    factors)

  {

  # Input:
  # data_frame: PCA results and metadata
  # principal_component: PC to analyze (e.g., "PC1")
  # factors: Column names for factors (e.g., c("age", "species"))

  full_formula <- as.formula(paste(principal_component,
                                   "~",
                                   paste(factors,
                                         collapse = " + ")))

  # Fit the model and extract R-squared
  total_r2 <- data_frame |>
    lm(formula = full_formula,
       data = _) |>
    glance() |>
    pull(r.squared)

  # R-squared contributions for each factor
  var_explained_df <- factors |>
    map_df(~ { reduced_formula <- as.formula(paste(principal_component,
                                                   "~",
                                                   paste(setdiff(factors,
                                                                 .x),
                                                         collapse = " + ")))

    # Fit reduced model and extract R-squared
    reduced_r2 <- data_frame |>
      lm(formula = reduced_formula,
         data = _) |>
      glance() |>
      pull(r.squared)

    # Variance explained
    tibble(Factor = .x,
           Var_Explained = total_r2 - reduced_r2,
           Total_R2 = total_r2,
           Reduced_R2 = reduced_r2)})

  return(var_explained_df)

  }


apply_variance_explained <- function(data_frame,
                                     spec,
                                     fact)

  {

  data_frame |>
    filter(species == spec) |>
    group_by(PC) |>
    nest() |>
    mutate(
      var_explained = map(
        .x = data,
        .f = ~ calculate_var_explained(
          data_frame = .x,
          principal_component = "value",
          factors = fact))) |>
    unnest(cols = c(var_explained)) |>
    dplyr::select(-data)

  }



calculate_pve <- function(data_frame)

  {

  data_frame |>
    group_by(Factor) |>
    summarize(Mean_PVE = mean(Var_Explained / Total_R2)) |>
    ungroup()

  }
