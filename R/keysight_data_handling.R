#' @title Keysight Data Filter
#'
#' @description Split data from Keysight data drop
#' Keysight tool creates csv files that contain:
#' fab info
#' Overview
#' mueller_row_1_TLS0
#' average_il_TLS0_
#' pdl_TLS0_
#' te_tm_TLS0
#'
#' @importFrom readr read_lines
#' @importFrom utils read.csv
#'
#' @param raw_data_path path to data
#' @param section_delim section delim
#'
#' @return
#' `dataframe`
#'
#' @export
split_keysight_csv <- function(raw_data_path, section_delim = "=") {

  ########### Add the raw data by given delimiter ################
  section_df_list <- list()

  raw_data <- readr::read_lines(raw_data_path, skip_empty_rows = T)
  section_indices <- grep(section_delim, raw_data)

  # Loop variables
  skip_count <- 0
  section_index <- 1

  # Seperates raw dataframe into a list of dataframes for each section
  while (section_index <= length(section_indices)) {

    temp_df <- utils::read.csv(raw_data_path, header = FALSE,
                               skip = skip_count,
                               nrows = section_indices[section_index] - skip_count - 1)

    section_df_list[[section_index]] <- temp_df

    # Controls how loop iterates
    skip_count <- section_indices[section_index]
    section_index <- section_index + 1

    # This is to add the last section of the file
    if (section_index > length(section_indices)) {
      temp_df <- utils::read.csv(raw_data_path,
                                 header = FALSE, skip = skip_count)
      section_df_list[[section_index]] <- temp_df
    }
  }

  # TODO automate section names based on raw file
  section_name_vector <- list("fab_info", "job_overview",
                              "mueller_row_1_TLS0", "average_il_TLS0_",
                              "pdl_TLS0_", "te_tm_TLS0")
  names(section_df_list) <- section_name_vector

  return(section_df_list)

}

#' @title Generate Section Names
#'
#' @description generate names for each column of data
#' For example, the TE/TM section is named with the amount
#' of fibers observed and modes
#'
#' @param df_section
#'
#' @return
#' `List`
#'
#' @export
generate_section_col_names <- function(df_section) {

  if ("te_tm_TLS0" %in% names(df_section)) {

    return(generate_te_tm_col_names(df_section["te_tm_TLS0"]))
  }
  else {

    section_cols <- list("lambda")
    for (fiber_num in 1:(dim(df_section[[names(df_section)]])[2] - 1))
      section_cols[fiber_num + 1] <- paste("f", fiber_num, sep = "")

    return(section_cols)
  }
}



#' @title Generate Section Names
#'
#' @description generate names for the TE/TM column of data
#' For example, the TE/TM section is named with the amount
#' of fibers observed and modes
#'
#' @param df_te_tm
#'
#' @return
#' `List`
#'
#' @export
generate_te_tm_col_names <- function(df_te_tm) {

  te_tm_cols <- list("lambda")
  # Fiber num equal to half amount of cols (without lambda)
  mode_index <- 2
  for (fiber_num in 1:((dim(df_te_tm[[names(df_te_tm)]])[2] - 1) / 2)) {

    # Add TE and TM for each fiber
    modes <- list("te", "tm")
    for (mode in modes) {
      te_tm_cols[mode_index] <- paste("loss_f", fiber_num, "_", mode, sep = "")
      mode_index <- mode_index + 1
    }
  }

  return(te_tm_cols)
}


#' @title Adjust columns form Keysight data
#'
#' @description Iterates through all sections
#' in the raw data file
#' and names their columns accordingly
#'
#' @param raw_section_list
#'
#' @return
#' `dataframe`
#'
#' @export
adjust_raw_colnames <- function(raw_section_list) {

  df_sections <- raw_section_list

  for (section_num in 3:length(raw_section_list)) {
    temp_colnames <- generate_section_col_names(raw_section_list[section_num])
    colnames(df_sections[[section_num]]) <- temp_colnames
  }

  return(df_sections)

}


#' @title Get TE mode data
#'
#' @description Get TE mode data from
#' a dataframe that contains te_tm data.
#' This function allows you to put in specific fibers
#' for analysis.
#'
#' @importFrom dplyr %>%
#' @importFrom dplyr select
#' @importFrom dplyr mutate_all
#'
#' @param df_te_tm dataframe containing te_tm data
#' @param fiber_nums Number of optical fibers
#'
#' @return `dataframe`
#'
#' @export
get_te_data <- function(df_te_tm, fiber_nums = c()) {

  # Finds how many fibers were used in dataframe
  # This does take the lambda coloumn into consideration
  fiber_count <- (length(colnames(df_te_tm)) - 1) / 2

  fiber_selections <- fiber_nums
  # Defualts to all fibers
  if (length(fiber_selections) < 1) {
    fiber_selections <- c(1:fiber_count)
  }

  # Create list for dataframe variable selection
  te_data <- c("lambda")
  index <- 2
  for (fiber_num in fiber_selections) {
    te_data[index]  <- paste("loss_f", fiber_num, "_", "te", sep = "")
    index <- index + 1
  }

  df_te <- dplyr::select(df_te_tm, te_data)
  df_te <- df_te %>% dplyr::mutate_all(as.numeric)
  return(df_te)
}


#' @title Write Keysight Sections
#'
#' @description Creates files for each
#' section contained in the `section body` param
#'
#' @importFrom stringr str_replace
#' @importFrom readr write_csv
#'
#' @param section_bodys list sections from keysight data
#' @param path Directory for files to be saved
#'
#' @return void
#'
#' @export
write_sections_to_files <- function(section_bodys, path) {

  data_dir <- stringr::str_replace(path, ".csv", "")
  dir.create(data_dir)

  section_index <- 1
  for (section in section_bodys) {
    new_file_path <- paste(data_dir, "\\",
                           names(section_bodys)[section_index], ".csv", sep = "")

    readr::write_csv(x = section, file = new_file_path)
    section_index <- section_index + 1
  }

}
