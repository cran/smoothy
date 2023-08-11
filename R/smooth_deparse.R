#' Deparse
#'
#' @description Transforms the Data with a Row by Date to a Row by Individual.
#'
#' @param id Unique identifier of the patient.
#' @param day Day of the treatment.
#' @param treatment A character vector representing the type of treatment given to each patient.
#'
#' @return A data.frame with the following structure:
#' \describe{
#' \item{id}{A character vector representing the unique identifier for each patient.}
#' \item{start_date}{Start date of the treatment.}
#' \item{end_date}{End date of the treatment.}
#' \item{treatment}{A character vector representing the type of treatment given to each patient.}
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
#' deparsed_treatment <- smooth_deparse(smoothed$id, smoothed$day, smoothed$treatment)
#' deparsed_smothed <- smooth_deparse(smoothed$id, smoothed$day, smoothed$smoothed_treatment)
#'
#' @export
smooth_deparse <- function(id, day, treatment){

  aux <-
    as.factor(treatment) |>
    as.integer()

  aux <- c(FALSE, diff(aux) != 0)

  res <- data.frame(
    id = id,
    day = day,
    treatment = treatment,
    aux = cumsum(aux)
  )

  # revisar dia
  res <-
    res |>
    group_by(id, aux, treatment) |>
    reframe(start_date = min(day), end_date = max(day)) |>
    select(id, .data$start_date, .data$end_date, treatment)

  return(res)

}
