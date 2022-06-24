#' Map files to df
#' @description
#' Creates a `FileID` column that contains
#' the file path and makes a new dataframe
#' for each file with the new column
#'
#' @importFrom purrr set_names
#' @importFrom purrr map_dfr
#' @importFrom dplyr %>%
#' @importFrom utils read.csv
#'
#' @param directory Files that have been collected from Keysight Software
#'
#' @return `dataframe`
map_files_to_df <- function(directory) {
  dfwg <- directory %>% list.files(full.names = TRUE) %>% purrr::set_names() %>%
    purrr::map_dfr(.f = utils::read.csv, .id = "FileID")

  return(dfwg)
}


#' Map files to dfs
#'
#' @description
#' Creates a `FileID` column that contains
#' the file path and makes a new dataframe
#' for each file with the new column
#'
#' @importFrom dplyr %>%
#'
#' @param directory Files that have been collected from Keysight Software
#'
#' @return `dataframe`
map_files_to_dfs <- function(directory) {
  df_list <- list()
  filenames <- list.files(path = directory, full.names = TRUE)

  # TODO make this a for loop for more concise syntax
  # for loops creates a bunch of 'NA' values...
  file_index <- 1
  while (file_index <= length(filenames)) {
    df_list[[file_index]] <- utils::read.csv(filenames[file_index])
    file_index <- file_index + 1
  }
  names(df_list) <- filenames

  return(df_list)
}


#' Get Number from string
#'
#' @description
#' Converts a string into an int/double
#' by decoding the string
#'
#' @param input_string Waveguide dimensions as a string
#'
#'
#' @return `int` or `double`
get_number_from_string <- function(input_string) {

  # first replace "p" with a "."
  num_str <- stringr::str_replace_all(tolower(trimws(input_string)), "p", ".") %>%
    stringr::str_replace_all("[a-z]", "") %>% as.numeric()

  return(num_str)
}

#' Cutback Method
#'
#' @description
#' Fit Waveguide Insertion Loss
#'
#' @importFrom dplyr %>%
#' @importFrom dplyr group_by
#' @importFrom dplyr mutate
#' @importFrom dplyr summarise
#' @importFrom stats lm
#' @importFrom assertthat assert_that
#'
#' @param wg_loss_df This requires a dataframe with `lot`, `wafer`, `lambda`, `loss1`, `x_y_die`, & `dims`
#'
#' @return `dataframe`
cut_back_method <- function(wg_loss_df, grouping) {

  # Force user to have each of the following in their dataframe
  assertthat::assert_that("lambda" %in% names(wg_loss_df))
  assertthat::assert_that("loss1" %in% names(wg_loss_df))
  assertthat::assert_that("dims" %in% names(wg_loss_df))

  dfwg_summary <- wg_loss_df %>% dplyr::group_by(lambda, {{grouping}}) %>%
    dplyr::mutate(deltaL = dims - min(dims)) %>%
    dplyr::summarise(wg_loss  = stats::lm(formula = loss1 ~ deltaL)$coefficients[2],
                     cal_loss = stats::lm(formula = loss1 ~ deltaL)$coefficients[1],
                     rsq      = summary(stats::lm(formula = loss1 ~ deltaL))$r.squared
    )
  return(dfwg_summary)
}
