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
