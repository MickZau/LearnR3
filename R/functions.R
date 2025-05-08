#' Import data from the DIME study dataset.
#'
#' @param file_path Path to the CSV file.
#'
#' @returns A data frame
#'
import_dime <- function(file_path) {
  data <- file_path |>
    readr::read_csv(
      show_col_types = FALSE,
      name_repair = snakecase::to_snake_case,
      n_max = 100
    )
  return(data)
}

# " Import all DIME csv
# "
#' @param folder_path Path to file
#'
#' @returns Returns a data frame
#'
import_csv_files <- function(folder_path) {
  files <- folder_path |>
    fs::dir_ls(glob = "*.csv")

  data <- files |>
    purrr::map(import_dime) |>
    purrr::list_rbind(names_to = "file_path_id")
  return(data)
}

#' Extract participant id from CSV
#'
#' @param data The path to the files
#'
#' @returns Returns a data fram
#'
get_participant_id <- function(data) {
  data_with_id <- data |>
    dplyr::mutate(
      id = stringr::str_extract(
        file_path_id,
        "[:digit:]+\\.csv$"
      ) |>
        stringr::str_remove("\\.csv$") |>
        as.integer(),
      .before = file_path_id
    ) |>
    dplyr::select(-file_path_id)
  return(data_with_id)
}

prepare_dates <- function(data, column) {
  prepared_dates <- data |>
    mutate(
      date = as_date({{ column }}),
      hour = hour({{ column }}),
      .before = {{ column }}
    )
  return(prepared_dates)
}

#' Clean and prepare the CGM data for joining
#'
#' @param data the CGM dataset
#'
#' @returns A cleaner data frame
clean_cgm <- function(data) {
  cleaned <- data |>
    get_participant_id() |>
    prepare_dates(device_timestamp) |>
    dplyr::rename(Glucose = historic_glucose_mmol_l) |>
    summarise_column(Glucose, list(
      mean = mean,
      sd = sd
    ))
  return(cleaned)
}

#' Clean and prepare sleep data for joining
#'
#' @param data The sleep dataset
#'
#' @returns A cleaner data frame
clean_sleep <- function(data) {
  cleaned <- data |>
    get_participant_id() |>
    dplyr::rename(datetime = date) |>
    prepare_dates(datetime) |>
    summarise_column(seconds, list(sum = sum)) |>
    sleep_types_to_wider()
  return(cleaned)
}

#' Summarise a single column based on one or more functions
#'
#' @param data Either the CGM or sleep data in DIME
#' @param column The column we want to summarise
#' @param functions One or more functions to apply to the column. IF more than one added, use list().
#'
#' @returns A summarised column
summarise_column <- function(data, column, functions) {
  summarized_data <- data |>
    dplyr::select(-tidyselect::contains("timestamp"), -tidyselect::contains("datetime")) |>
    dplyr::group_by(dplyr::pick(-{{ column }})) |>
    dplyr::summarise(
      dplyr::across(
        {{ column }},
        functions
      ),
      .groups = "drop"
    )
  return(summarized_data)
}

#' Convert the participant details data to long and clean it up.
#'
#' @param data The DIME participant details data
#'
#' @returns A data frame
clean_participant_details <- function(data) {
  cleaned <- data |>
    tidyr::pivot_longer(tidyselect::ends_with("date"), names_to = NULL, values_to = "date") |>
    dplyr::group_by(dplyr::pick(-date)) |>
    tidyr::complete(
      date = seq(min(date), max(date), by = "1 day")
    )
  return(cleaned)
}

#' Convert sleep data to pivot wider
#'
#' @param data Sleep data
#'
#' @returns A cleaner data frame
sleep_types_to_wider <- function(data) {
  wider <- data |>
    tidyr::pivot_wider(
      names_from = sleep_type,
      values_from = seconds_sum,
      names_prefix = "seconds_"
    )
  return(wider)
}
