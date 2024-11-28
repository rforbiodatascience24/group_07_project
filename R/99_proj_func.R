
calculate_var_explained <- function(data_frame,
                                    principal_component,
                                    attributes)

  {

  # Input:
  # data_frame: PCA results and metadata
  # principal_component: PC to analyze (e.g., "PC1")
  # Attributes: Column names for attributes (e.g., c("log_age", "species_num"))

  full_formula <- as.formula(paste(principal_component,
                                   "~",
                                   paste(attributes,
                                         collapse = " + ")))

  # Fit the model and extract R-squared
  total_r2 <- data_frame |>
    lm(formula = full_formula,
       data = _) |>
    glance() |>
    pull(r.squared)

  # R-squared contributions for each attribute
  var_explained_df <- attributes |>
    map_df(~ {
      reduced_formula <- as.formula(paste(principal_component,
                                          "~",
                                          paste(setdiff(attributes,
                                                        .x),
                                                collapse = " + ")))

    # Fit reduced model and extract R-squared
    reduced_r2 <- data_frame |>
      lm(formula = reduced_formula,
         data = _) |>
      glance() |>
      pull(r.squared)

    # Variance explained
    tibble(attribute = .x,
           var_expl_pc = total_r2 - reduced_r2,
           total_r2 = total_r2,
           reduced_r2 = reduced_r2)})

  return(var_explained_df)

  }


apply_variance_explained <- function(data_frame,
                                     spec,
                                     fact)

  {


  # Input:
  # data_frame: PCA results and metadata
  # spec: species to apply to (e.g., "Human")
  # fact: Column names for attributes (e.g., c("log_age", "sex_num"))

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
          attributes = fact))) |>
    unnest(cols = c(var_explained)) |>
    dplyr::select(-data)

  }



calculate_pve <- function(data_frame)

  {

  # Input:
  # data_frame: variance explained explained data

  data_frame |>
    group_by(attribute) |>
    summarize(mean_pve = mean(var_expl_pc / total_r2)) |>
    ungroup()

}




shift_model <- function(data_frame,
                        set_shift = 0,
                        target_species = "Chimpanzee") {

  # Transform expression data
  base_data <- data_frame |>
    mutate(
      log_expr = log2(expr),
      modeled_expr = intercept +
        poly1 * log_age +
        poly2 * log_age^2 +
        poly3 * log_age^3)

  # Fit baseline model
  baseline_model <- lm(log_expr ~ poly(log_age, 3) * species, data = base_data)

  #  Use NLS to estimate the best shifts
  tryCatch({
    nls_fit <- nls(
      log_expr ~ intercept +
        poly1 * (log_age + age_shift) +
        poly2 * (log_age + age_shift)^2 +
        poly3 * (log_age + age_shift)^3 + expr_shift,
      data = base_data |> filter(species == target_species),
      start = list(age_shift = 0, expr_shift = 0),
      control = nls.control(maxiter = 100000))

    # Extract the optimized shift parameters
    optimized_shifts <- coef(nls_fit)
    age_shift_opt <- optimized_shifts["age_shift"]
    expr_shift_opt <- optimized_shifts["expr_shift"]

    # Apply the optimized shifts to the entire dataset
    shifted_data <- base_data |>
      mutate(
        age_shifted = case_when(
          species == target_species ~ log_age + age_shift_opt,
          TRUE ~ log_age),
        log_expr_shifted = case_when(
          species == target_species ~ log_expr + expr_shift_opt,
          TRUE ~ log_expr),
        modeled_expr = intercept +
          poly1 * age_shifted +
          poly2 * age_shifted^2 +
          poly3 * age_shifted^3)

    # Fit the shifted model
    shifted_model <- lm(log_expr_shifted ~ poly(age_shifted, 3) * species, data = shifted_data)

    # Perform ANOVA
    f_test <- anova(baseline_model, shifted_model)
    p_value <- f_test[["Pr(>F)"]][2]

    # I can't get this to work using
    # f_test |>
    # filter(term == "shifted_model") |>
    # pull(p.value)

    # Calculate RSS difference
    baseline_rss <- sum(residuals(baseline_model)^2)
    shifted_rss <- sum(residuals(shifted_model)^2)
    rss_diff <- baseline_rss - shifted_rss

    # Return the results
    list(
      age_shift = age_shift_opt,
      expr_shift = expr_shift_opt,
      p_value = p_value,
      rss_diff = rss_diff)},
    error = function(e) {
      list(
        age_shift = NA,
        expr_shift = NA,
        p_value = NA,
        rss_diff = NA)})}





apply_shift_model <- function(data_frame,
                              original_species,
                              compared_species) {
  data_frame |>
    filter(species %in% c(original_species,
                          compared_species)) |>
    group_by(gene) |>
    nest() |>
    mutate(
      shift_modeling = map(.x = data,
                           .f = ~ shift_model(data_frame = .x,
                                              target_species = compared_species))) |>
    mutate(
      age_shift = map_dbl(shift_modeling, "age_shift"),
      expr_shift = map_dbl(shift_modeling, "expr_shift"),
      p_value = map_dbl(shift_modeling, "p_value"),
      rss_diff = map_dbl(shift_modeling, "rss_diff")) |>
    dplyr::select(-shift_modeling)}




class_model <- function(data_frame){
  classification <- data_frame |>
    mutate(class = case_when(
      p_value < 0.5 ~ case_when(
        age_shift > 0 ~ "Neogenic",
        age_shift < 0 ~ "Accelerated",
        TRUE ~ "Unclassified"),
      TRUE ~ "Unclassified"))

  neogenic <- classification |>
    filter(class == "Neogenic") |>
    pull(gene)

  accelerated <- classification |>
    filter(class == "Accelerated") |>
    pull(gene)

  unclassified <- classification |>
    filter(class == "Unclassified") |>
    pull(gene)

  # Return
  list(
    neogenic = neogenic,
    accelerated = accelerated,
    unclassified = unclassified)}
