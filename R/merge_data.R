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
    ## Check arguments
    assertthat::assert_that(is.list(datasets))
    dataset.names <- c("swetrau", "fmp", "atgarder", "problem", "kvalgranskning2014.2017")
    assertthat::assert_that(all(dataset.names %in% names(datasets)))
    attach(datasets)
    assertthat::assert_that(all(c("DateTime_ArrivalAtHospital", "PersonIdentity", "TempIdentity") %in% names(swetrau)))
    assertthat::assert_that(all(c("Ankomst_te", "Personnummer", "Reservnummer") %in% names(fmp)))
    assertthat::assert_that(all(c("Ankomst_te", "Personnummer", "Reservnummer") %in% names(problem)))
    assertthat::assert_that(all(c("DateTime_ArrivalAtHospital", "pat_personnummer", "pat_TempPersonnummer") %in% names(kvalgranskning2014.2017)))
    ## Format datetime variable
    swetrau$arrival <- as.POSIXct(strptime(swetrau$DateTime_ArrivalAtHospital, format = "%Y-%m-%d %H:%M"))
    fmp$arrival <- as.POSIXct(strptime(fmp$Ankomst_te, format = "%Y%m%d %H:%M"))
    problem$arrival <- as.POSIXct(strptime(problem$Ankomst_te, format = "%Y%m%d %H:%M"))
    kvalgranskning2014.2017$arrival <- as.POSIXct(strptime(as.character(sub(" UTC", "", kvalgranskning2014.2017$DateTime_ArrivalAtHospital, fixed = TRUE)), format = "%Y-%m-%d %H:%M:%S"))
    ## Create id variable by pasting arrival time and hashed identify
    swetrau$id <- with(swetrau, paste(arrival, PersonIdentity, TempIdentity))
    fmp$id <- with(fmpt, paste(arrival, Personnummer, Reservnummer))
    problem$id <- with(problem, paste(arrival, Personnummer, Reservnummer))
    kvalgranskning2014.2017$id <- with(kvalgranskning2014.2017, paste(arrival, pat_personnummer, pat_TempPersonnummer))
    ## Combine datasets
    combined.datasets <- merge(fmp, problem, by = "id", all.x = TRUE)
    combined.datasets <- merge(combined.datasets, swetrau, by = "id", all.x = TRUE)
    detach(datasets)
    return (combined.datasets)
}
