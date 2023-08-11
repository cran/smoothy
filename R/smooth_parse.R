#' Transform Data to be Used in \code{smooth_algorithm()} Function
#'
#' @description This function transforms the data to obtain the daily treatment.
#'
#' @param id Unique identifier of the patient.
#' @param start_date Start date of the treatment.
#' @param end_date End date of the treatment.
#' @param drug Name of the drug used.
#' @param study_from A date indicating when the study start.
#' @param study_to A date indicating when the study finish.
#'
#' @return A data.frame with the following structure:
#'
#' \describe{
#' \item{id}{Unique identifier of the patient.}
#' \item{drug}{Name of the drug used.}
#' \item{day}{Day of the treatment.}
#' \item{N}{Number of drugs used in the treatment}
#' }
#'
#' @examples
#' library(smoothy)
#' library(dplyr)
#'
#' data(drugstreatment)
#'
#' df <- drugstreatment |>
#'   filter(id == "01f13c15-d9f1-4106-a04f-976c457edd0a")
#'
#' structured_df <- smooth_parse(
#'   id = df$id,
#'   start_date = df$start_date,
#'   end_date = df$end_date,
#'   drug = df$drug,
#'   study_from = "1970-01-01",
#'   study_to = "1975-01-01"
#' )
#'
#' head(structured_df)
#'
#' @importFrom dplyr bind_rows
#' @importFrom dplyr group_by
#' @importFrom dplyr reframe
#' @importFrom dplyr n_distinct
#' @importFrom dplyr left_join
#' @importFrom dplyr bind_rows
#' @importFrom dplyr filter
#'
#' @export
smooth_parse <- function(id, start_date, end_date, drug, study_from = min(start_date), study_to = max(end_date)) {

  # crear el data.frame desde study_from fins study_to.
  study_days <- seq.Date(from = as.Date(study_from), to = as.Date(study_to), by = 'days')
  aux <- lapply(id |> unique(), FUN = function(x) data.frame(id = x, day = study_days)) |> bind_rows()

  patients <- data.frame(id = id, start_date, end_date, drug) |>
    filter(end_date >= study_from) |>
    filter(start_date <= study_to)

  res <- apply(patients, 1, FUN = function(x_i){

    res <- data.frame(
      id = x_i[["id"]],
      drug = x_i[["drug"]],
      day = seq(as.Date(x_i[["start_date"]]), as.Date(x_i[["end_date"]]), "days")
    )

    return(res)

  }) |>
    bind_rows() |>
    group_by(id, .data$day) |>
    reframe(
      N = n_distinct(drug),
      treatment = drug %>% unique() %>% sort() %>% paste0(collapse = "+")
    )

  res <- left_join(aux, res, relationship = "many-to-many", by = c('id', 'day')) # hauria de tenir una unica fila. revisar.
  res$treatment[is.na(res$treatment)] <- "None"
  res$N[is.na(res$N)] <- 0

  res <- res |>
    filter(.data$day >= study_from) |>
    filter(.data$day <= study_to)

  return(res)

}
