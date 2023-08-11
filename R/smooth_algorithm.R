#' Apply Smooth Algorithm in a Dataset
#'
#' @description Description part
#'
#' @param id Unique identifier of the patient.
#' @param treatment Name of the drug used.
#' @param day Day of the treatment.
#' @param N Number of drugs used in the treatment.
#' @param width An integer specifying the window width (in numbers of days, 61 by default).
#'
#' @return A data.frame with the following structure:
#' \describe{
#' \item{id}{A character vector representing the unique identifier for each patient.}
#' \item{day}{A character vector representing the date when the treatment was administered to the patients.}
#' \item{treatment}{A character vector representing the type of treatment given to each patient.}
#' \item{smoothed_treatment}{A character vector representing the smoothed treatment given to each patient.}
#' }
#'
#' @examples
#'
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
#' id = structured_df$id
#' treatment = structured_df$treatment
#' day = structured_df$day
#' N = structured_df$N
#' width = 61
#'
#' smoothed <- smooth_algorithm(id = id, treatment = treatment, day = day, N = N, width = width)
#' head(smoothed)
#'
#' @importFrom dplyr filter
#' @importFrom dplyr group_by
#' @importFrom dplyr summarise
#' @importFrom dplyr mutate
#' @importFrom dplyr select
#' @importFrom dplyr bind_rows
#' @importFrom dplyr n
#' @importFrom tidyr fill
#' @importFrom stats embed
#' @importFrom zoo na.locf
#'
#' @export
smooth_algorithm <- function(id, treatment, day, N, width = 61){

    patients <- data.frame(id = id, treatment = treatment, day = day, N = N)

    fun_01 <- function(x) {

      tmp <- table(x)
      tmp <- names(tmp)[tmp == max(tmp)]
      ifelse(length(tmp) != 1, NA, tmp)

    }

    fun_02 <- function(x, width){

      w <- embed(x, width)
      apply(w, 1, fun_01) |>
        unlist() |>
        zoo::na.locf(na.rm = FALSE)

    }

    fun_03 <- function(treatment, width){

      aux <- rep(NA, width - 1)
      treatment <- c(aux, treatment, aux) |> fun_02(width) |> fun_02(width)

    }

    patients <- patients |>
      group_by(id) |>
      arrange(id, day)

    res <- split(patients, f = patients$id) |>
      lapply(FUN = function(x) {
        mutate(x, smoothed_treatment = fun_03(x$treatment, width = width))
      }) |>
      bind_rows()

    return(res)

}
