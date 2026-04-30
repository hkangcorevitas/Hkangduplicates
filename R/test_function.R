#' Check for Duplicates
#'
#' @description
#' Identify duplicate records in a dataset based on a set of variables.
#'
#' @param df The analysis dataset being assessed
#' @param variable_list Character vector of variables used to assess duplicates
#'
#' @returns A list containing:
#' \itemize{
#'   \item data_with_flag: Original dataset with duplicate flags
#'   \item duplicate_rows: Rows identified as duplicates
#'   \item duplicate_summary: Unique duplicate combinations with counts
#' }
#'
#' @export
#' @importFrom dplyr group_by across all_of mutate n ungroup filter distinct
#' @importFrom magrittr %>%


Hkangduplicates <- function(df, variable_list) {
  df_flagged <- df %>%
    dplyr::group_by(dplyr::across(dplyr::all_of(variable_list))) %>%
    dplyr::mutate(
      dup_count = dplyr::n(),
      is_duplicate = dplyr::n() > 1
    ) %>%
    dplyr::ungroup()

  dup_rows <- df_flagged %>%
    dplyr::filter(is_duplicate)

  dup_summary <- dup_rows %>%
    dplyr::distinct(dplyr::across(dplyr::all_of(variable_list)), dup_count)

  list(
    data_with_flag = df_flagged,
    duplicate_rows = dup_rows,
    duplicate_summary = dup_summary
  )
}
