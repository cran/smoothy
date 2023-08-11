#' @title Compute the Difference Between Initial and Smoothed Treatment
#' @usage smooth_diff(treatment, smoothed_treatment)
#' @description This function computes the differences between the initial treatment and the treatment when it's smoothed.
#'
#' @param treatment a character vector containing the original treatment data..
#' @param smoothed_treatment a character vector containing the smoothed treatment return by \code{smooth_algorithm} function.
#'
#' @return A data.frame with three columns: diff_type , diff, change and treatment:
#'
#' \describe{
#'  \item{type}{A character vector representing indicating the type of difference computed.}
#'  \item{days_changed}{The number of different items.}
#'  \item{proportion_of_change}{The proportion of difference computed as number of diferent rows over number of rows.}
#'  \item{treatment}{A character vector representing the type of treatment given to each patient.}
#' }
#'
#' @examples
#'
#' library(smoothy)
#' library(dplyr)
#'
#' data(drugstreatment)
#'
#' my_data <- filter(drugstreatment, id == "01f13c15-d9f1-4106-a04f-976c457edd0a")
#'
#' structured_df <- smooth_parse(
#'   id = my_data$id,
#'   start_date = my_data$start_date,
#'   end_date = my_data$end_date,
#'   drug = my_data$drug,
#'   study_from = "1970-01-01",
#'   study_to = "1975-01-01"
#' )
#'
#' head(structured_df)
#'
#' id = structured_df$id
#' treatment = structured_df$treatment
#' day = structured_df$day
#' N = structured_df$N
#' width = 61
#'
#' smoothed <- smooth_algorithm(id = id, treatment = treatment, day = day, N = N, width = width)
#'
#' head(smoothed)
#'
#' smooth_diff(treatment = smoothed$treatment, smoothed_treatment = smoothed$smoothed_treatment)
#'
#' @import dplyr
#' @import tidyr
#'
#' @export
smooth_diff <- function(treatment, smoothed_treatment){

  .data <- data.frame(treatment = treatment, smoothed_treatment = smoothed_treatment)

  # Global change
  global_change =
    .data |>
    mutate(diff = treatment != smoothed_treatment) |>
    ungroup() |>
    reframe(diff = sum(diff, na.rm = TRUE), change = sum(diff, na.rm = T) / n()) |>
    select(diff, .data$change)

  #Expousure change

  expousure_change =
    # Remove rows corresponding to the first "None" until a treatment different from "None"
    .data |>
    mutate(first_non_none = min(which(treatment != "None"))) |>
    filter(row_number() > .data$first_non_none | is.na(.data$first_non_none)) |>
    ungroup() |>
    select(-.data$first_non_none) |>
    # Remove rows corresponding to the last "None"
    mutate(last_non_none = max(which(treatment != "None"))) |>
    filter(row_number() < .data$last_non_none | is.na(.data$last_non_none)) |>
    ungroup() |>
    select(-.data$last_non_none) |>
    # Expousure change
    mutate(diff = treatment != smoothed_treatment) |>
    ungroup() |>
    summarise(diff = sum(diff, na.rm = T), change = sum(diff, na.rm = T) / n()) |>
    select(diff, .data$change)

  # Drug change
  drug_change =
    .data |>
    filter(treatment != "None") |>
    group_by(treatment) |>
    mutate(diff = treatment != smoothed_treatment) |>
    summarise(diff = sum(diff, na.rm = T), change = sum(diff, na.rm = T) / n()) |>
    select(treatment, diff, .data$change)


  # output:
  res <- list(
    Global = global_change,
    Expousure_period = expousure_change,
    drug_change = drug_change
    ) |> bind_rows(.id = "diff_type")

  names(res) <- c("type", "days_changed", "proportion_of_change", "treatment")

  res$type = paste(res$type,res$treatment) %>%
    stringr::str_replace('_',' ') %>%
    stringr::str_remove(' NA') %>%
    stringr::str_remove('_change')

  res = res %>% select(-treatment)

  return(res)

}
