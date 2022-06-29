#' @title Fiber Cutback Method
#'
#' @description
#' Fit Waveguide Insertion Loss
#'
#' @importFrom data.table setDT
#' @importFrom assertthat assert_that
#' @importFrom methods is
#' @importFrom data.table .N
#' @importFrom data.table .SD
#'
#' @param wg_df
#' Requires `list` or `data` frame containing columns that describe:
#' Wavelengths used
#' Losses measured
#' Length of Waveguides
#'
#' @param group_sel Variable(s) to group to fit by.
#' This needs to be a list or single variable name
#'
#' @param ... Column names to find required data from 'wg_df'.
#' Must be in the following order:
#' 1: Wavelengths
#' 2: losses
#' 3: Waveguide Lengths
#'
#' @return `data table`
#'
#' @export
cut_back_method <- function(wg_df, group_sel, ...) {

  # Declare local var names to be used by function
  lambda_col <- "lambda"
  loss_col <- "loss"
  wg_len_col <- "wg_length"
  group_names <- group_sel

  ########## ##########
  # Find column names from paramatized values
  # Check for list in case of embedded values
  if (...length() > 1) {
    lambda_col <- ..1
    loss_col <- ..2
    wg_len_col <- ..3
  } else if (is(..., "list")) {
    temp_list <- unlist(...)
    lambda_col <- temp_list[1]
    loss_col <- temp_list[2]
    wg_len_col <- temp_list[3]
  } else {
    lambda_col <- ...[1]
    loss_col <- ...[2]
    wg_len_col <- ...[3]
  }

  # group_names check for list in case of embedded values
  if (is(group_names, "list")) {
    group_names <- unlist(group_sel)
  }
  ########## ##########

  # Combine paramaterized column names
  required_cols <- c(lambda_col, loss_col, wg_len_col, group_names)
  fit_grouping <- c(group_names, lambda_col)
  ########## ##########

  # Create Data table from required fitting columns
  wg_dt <- data.table::setDT(wg_df)[, required_cols, with = FALSE]

  # You can use >with = False< or >..{var}]< to get variable col
  wg_lengths <- wg_dt[, c(unique(.SD[, get(wg_len_col)]), Samples = list(.N)),
                      by = fit_grouping]

  # This statement forces you to use at least 3 data points to fit
  assertthat::assert_that(all(wg_lengths$Samples > 2))
  ########## ##########

  # .N will return how large each group in the fit was
  wg_dt <- wg_dt[, c(lm_metrics(.SD, loss_col, wg_len_col), Samples = list(.N)),
                 by = fit_grouping]
  ########## ##########

  return(wg_dt)
}

#'@title Fitting Linear Models for perfomance
#'
#' @description
#' Linear fitting with term coefficients and r-squared
#'
#' @importFrom stats as.formula
#' @importFrom stats lm
#'
#' @param data
#' `data table` expected
#'
#' @param y_term This varibale will be on LHS of equation in the form:
#' y = mx + b
#'
#' @param x_term This varibale will be on RHS of equation in the form:
#' y = mx + b
#'
#' @return `data table`
#'
#' @export
lm_metrics <- function(data, y_term, x_term) {

  fitting_list <- stats::as.formula(paste(y_term, "~", x_term))

  fit <- stats::lm(data, formula = fitting_list)

  fit_coeff <- fit$coefficients
  fit_rsq <- summary(fit)$r.squared

  fit_results <- c(fit_coeff, fit_rsq)
  names(fit_results) <- c("intercept", "coeff1", "residuals")

  return(fit_results)
}
