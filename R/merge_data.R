#' Merge data
#'
#' Merges the different opportunities for improvement datasets into a
#' single data.frame.
#' @param datasets A list of data.frames. Has to include the datasets
#'     "swetrau", "fmp", "atgarder", "problem", and
#'     "kvalgranskning2014.2017". The dataset "swetrau" has to include
#'     the variables "DateTime_ArrivalAtHospital", "PersonIdentity",
#'     and "TempIdentity". The datasets "fmp" and "problem" have to
#'     include the variables "Ankomst_te", "Personnummer", and
#'     "Reservnummer".
merge_data <- function(datasets) {
    dataset.names <- c("swetrau", "fmp", "atgarder", "problem", "kvalgranskning2014.2017")
    assertthat::assert_that(is.list(datasets))
    assertthat::assert_that(all(dataset.names %in% names(datasets)))
    attach(datasets)
    assertthat::assert_that(all(c("DateTime_ArrivalAtHospital", "PersonIdentity", "TempIdentity") %in% names(swetrau)))
    assertthat::assert_that(all(c("Ankomst_te", "Personnummer", "Reservnummer") %in% names(fmp)))
    assertthat::assert_that(all(c("Ankomst_te", "Personnummer", "Reservnummer") %in% names(problem)))
    swetrau$arrival <- as.POSIXct(strptime(swetrau$DateTime_ArrivalAtHospital, format = "%Y-%m-%d %H:%M"))
    fmp$arrival <- as.POSIXct(strptime(fmp$Ankomst_te, format = "%Y%m%d %H:%M"))
    problem$arrival <- as.POSIXct(strptime(problem$Ankomst_te, format = "%Y%m%d %H:%M"))
    swetrau$id <- paste(swetrau$arrival, swetrau$PersonIdentity, swetrau$TempIdentity)
    fmp$id <- paste(fmp$arrival, fmp$Personnummer, fmp$Reservnummer)
    problem$id <- paste(problem$arrival, problem$Personnummer, problem$Reservnummer)
    ## Combine datasets
    combined.datasets <- merge(fmp, problem, by = "id", all.x = TRUE)
    combined.datasets <- merge(combined.datasets, swetrau, by = "id", all.x = TRUE)
    detach(datasets)
    return (combined.datasets)
}
