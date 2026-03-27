#' Check for Percent Missing
#'
#' @description
#' Assess the duplicates count and list id & visitdate in the analysis dataset.
#'
#'
#' @param df the analysis dataset being assessed
#'
#' @returns A list of duplicates
#'
#' @export


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
