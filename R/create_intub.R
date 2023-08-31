#' Create Intubation Variable
#'
#' This function creates a new variable indicating whether a patient
#' was intubated, regardless of whether the intubation was performed
#' prehospital or in the emergency department. The result is a vector
#' containing the intubation status for each patient.
#'
#' @param data A data frame containing the data from which the
#'     intubation variable will be created.
#' @return A vector indicating the intubation status for each patient.
#'
#' @examples
#' data$intub <- create_intub(data)
#'
#' @export
create_intub <- function(data) {
    ## Check arguments
    assertthat::assert_that(is.data.frame(data))

    ## Create intubation variable
    intub <- with(data, ifelse(`pre_intubated` == 1 & is.na(data$pre_intubated) == FALSE, 3, `ed_intubated`))

    ## Return intubation variable
    return(intub)
}
