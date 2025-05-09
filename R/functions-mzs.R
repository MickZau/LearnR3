#' Import HR parquet files
#'
#' @param file_path HR files
#'
#' @returns A data frame
import_hr_files <- function(file_path) {
  files <- file_path |>
  fs::dir_ls(regexp = "HR\\.parquet", recursive = TRUE) |>
  map(read_parquet) |>
    list_rbind(names_to = "path_id")
return(files)
}

#' Import TEMP parquet files
#'
#' @param file_path TEMP files
#'
#' @returns A data frame
import_temp_files <- function(file_path) {
  files <- file_path |>
    fs::dir_ls(regexp = "TEMP\\.parquet", recursive = TRUE) |>
    map(read_parquet) |>
    list_rbind(names_to = "path_id")
  return(files)
}

#' Import BVP parquet files
#'
#' @param file_path BVP files
#'
#' @returns A data frame
import_bvp_files <- function(file_path) {
  files <- file_path |>
    fs::dir_ls(regexp = "BVP\\.parquet", recursive = TRUE) |>
    map(read_parquet) |>
    list_rbind(names_to = "path_id")
  return(files)
}

#' Generate ID from parquet path ID
#'
#' @param data Parquet data
#'
#' @returns A data frame
generate_id <- function(data) {
new_id <- data |>
  mutate(
    ID = stringr::str_extract(
      path_id,
      "stress/stress/[:alnum:]+\\/") |>
      str_remove("stress/stress/") |>
      str_remove("/"),
    .before = path_id)
return(new_id)
}

#' Splits datetime into date, hour and minutes
#'
#' @param data Parguet files
#' @param column Datetime columns
#'
#' @returns Date, hour and minutes
split_datetime <- function(data, column) {
  split_dates <- data |>
    mutate(
      date = as_date({{ column }}),
      hour = hour({{ column }}),
      .before = {{ column }})
  return(split_dates)
}

#' Select only columns we need
#'
#' @param column All columns
#'
#' @returns All columns except path and datetime
select_columns <- function(column) {
  raw_data <- column |>
    select(-tidyselect::contains("path"), -tidyselect::contains("collection_datetime"))
  return(raw_data)
}
