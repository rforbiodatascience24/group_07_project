#' Script: Function used for R4BDS project
#'
#' This script contains custom functions for analyzing data as part of the project.
#' The functions are designed for the analyses PCA/PVE, ,
#' and visualizing data using tidyverse principles.
#'
#' How to Use:
#' - Source this script using `source("99_proj_func.R")`.
#' - Call the functions as needed (e.g. calculate_var_explaine(df, pc, attr).


#' Calculate the variance explained
#'
#' Explanation
#'
#' @param data_frame A data frame containing PCA analysis and attribute data.
#' @param principal_component A character string of PC to analyze.
#' @param attribute A character vector of column names for attributes.
#' @return A tibble with variance explained results for each attribute.

calculate_var_explained <- function(data_frame,
                                    principal_component,
                                    attributes) {

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


#' Apply the above function to a species for multiple attributes
#'
#' Explanation.
#'
#' @param data_frame A data frame containing PCA analysis and attribute data.
#' @param spec A character string of species to analyze.
#' @param attr A character vector of column names for attributes.
#' @return A tibble with variance explained results for species and principal components.

apply_variance_explained <- function(data_frame,
                                     spec,
                                     attr) {

  # Filter input data frame and group for preparation
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
          attributes = attr))) |>
    unnest(cols = c(var_explained)) |>
    dplyr::select(-data)

  }


#' Calculate the proportion of variance explained
#'
#' Explanation.
#'
#' @param data_frame A data frame containing the variance explained.
#' @return A tibble of mean PVE for each attribute.

calculate_pve <- function(data_frame){

  data_frame |>
    group_by(attribute) |>
    summarize(mean_pve = mean(var_expl_pc / total_r2)) |>
    ungroup()

}


#' Applies shifts to regression and evaluates best combination of shifta
#'
#' Explanation.
#'
#' @param data_frame A data frame containing regression parameters, age values and min two species.
#' @param target_species A character string of species to apply shift to.
#' @return A list with the results of the shift model analysis.

shift_model <- function(data_frame,
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
  tryCatch( {
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
      rss_diff = rss_diff)

    },

    error = function(e) {
      list(
        age_shift = NA,
        expr_shift = NA,
        p_value = NA,
        rss_diff = NA)

      })

  }


#' Apply the above function to sekected species
#'
#' Explanation
#'
#' @param data_frame A data frame containing regression parameters, age values and min two species.
#' @param original_species A character string of species to match with shift.
#' @param compared_species A character string of species to apply shift to.
#' @return A tibble with the results of the shift model analysis.

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


#' Classify genes based on significance of age shift
#'
#' Explanation
#'
#' @param data_frame A data frame containing p_values and optimal age_shift for each gene
#' @return A list containing genes for each classification.

class_model <- function(data_frame) {

  # Evaluate significance and classify based og age_sift
  classification <- data_frame |>
    mutate(class = case_when(
      p_value < 0.5 ~ case_when(
        age_shift > 0 ~ "Neogenic",
        age_shift < 0 ~ "Accelerated",
        TRUE ~ "Unclassified"),
      TRUE ~ "Unclassified"))

  # Create vector of each classification
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
    unclassified = unclassified)

  }


#' Plot projection of data points unto two principal components
#'
#' Explantion
#'
#' @param data_frame A data frame containing the projection data for principal components.
#' @param principal_component_1 A character string of first PC to use projection data from.
#' @param principal_component_2 A character string of second PC to use projection data from.
#' @return A ggplot object of projection onto two PCs.

pca_proj_plot <- function(data_frame,
                          principal_component_1,
                          principal_component_2) {

  data_frame |>
    ggplot(aes(
      x = !!sym(principal_component_1),
      y = !!sym(principal_component_2),
      color = species,
      size = age,
      shape = sex)) +
    geom_point(stroke = 1) +
    labs(x = NULL,
         y = NULL) +
    coord_fixed() +
    theme_minimal() +
    theme(
      legend.position = "none",
      panel.grid.major = element_blank(),
      panel.grid.minor = element_blank(),
      axis.text.x = element_blank(),
      axis.text.y = element_blank(),
      axis.ticks.x = element_blank(),
      axis.ticks.y = element_blank(),
      axis.line = element_blank(),
      plot.margin = margin(0, 0, 0, 0),
      panel.spacing = unit(0, "null"),
      panel.border = element_blank()) +
    scale_color_manual(values = color_palette |>
                         filter(group == "Species") |>
                         pull(color)) +
    scale_shape_manual(values = c(1, 4)) +
    scale_size_continuous(range = c(0.1, 2.5)) +
    annotate(
      "text",
      x = -Inf,
      y = -Inf,
      label = paste(principal_component_2, "vs", principal_component_1),
      hjust = 0,
      vjust = -1,
      size = 3,
      fontface = "bold") +
    theme(plot.background = element_rect(fill = NA,
                                         color = NA))

}
