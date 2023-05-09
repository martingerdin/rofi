#' Merge FMP and problem
#'
#' This function merges the File Maker Pro database from 2017 to 2021
#' with a column describing the opportunities for improvment. These
#' have to be merged because they were distributed to us as separate
#' files.
#' @param fmp.data A data.frame. The FMP data. No default.
#' @param problem.data A data.frame. The data describing the opportunities for improvement. No default.
merge_fmp_with_problem <- function(fmp.data, problem.data) {
    assertthat::assert_that(is.data.frame(fmp.data))
    assertthat::assert_that(is.data.frame(problem.data))

    ## Reassign object to work with legacy code
    fmp <- fmp.data
    problem <- problem.data

    ## Combine id variables
    fmp$id <- with(fmp, paste(Personnummer, Reservnummer, Ankomst_te))
    problem$id <- with(problem, paste(Personnummer, Reservnummer, Ankomst_te))
    identical(fmp$id, problem$id) ## These vectors are exactly the same, meaning that we can combine these datasets using cbind

    ## Combine fmp and problem
    fmp.problem <- as.data.frame(cbind(fmp, problem$Problemomrade_.FMP))

    ##
    ## "Identical" includes same order? 
    ## Just change column name back to Problemomrade_.FMP
    ##
    fmp.problem$Problemomrade_.FMP <- fmp.problem$`problem$Problemomrade_.FMP`
    fmp.problem$`problem$Problemomrade_.FMP` <- NULL
    ##

    ## Create id, arrival and did variables
    fmp.problem$id <- fmp$Personnummer
    fmp.problem$id[is.na(fmp.problem$id)] <- fmp.problem$Reservnummer[is.na(fmp.problem$Personnummer)]
    ## sum(is.na(fmp.problem$id)) ## No missing id values
    fmp.problem$arrival <- as.Date(strptime(fmp.problem$Ankomst_te, format = "%Y%m%d %H:%M"))
    fmp.problem$did <- paste(fmp.problem$id, fmp.problem$arrival)

    ## Check for duplicates
    ## sum(duplicated(fmp.problem$did)) ## 22 duplicates

    ## Check each of the 22 duplicates
    ## duplicated.dids <- fmp$did[duplicated(fmp.problem$did)]
    ## remove <- as.numeric(unlist(lapply(duplicated.dids, function(did) {
    ##     rows <- fmp[fmp$did == did, ]
    ##     print(rows)
    ##     message("Remove? (press y and then Enter. Otherwise press just Enter)")
    ##     if (readLines(n = 1) == "y") {
    ##         return(row.names(rows)[2:nrow(rows)])
    ##     } else {
    ##         return (NA)
    ##     }
    ## })))

    ## Not all of them are obvious duplicates, for example there are cases
    ## with different arrival time and even sex, where they appear to be
    ## different, but the personal numbers are the same. I suggest we
    ## remove all these duplicates, and only keep the first match, as we
    ## can't match them to SweTrau.

    ## The vector was obtained with dput(remove)
    remove <- c(3491, 4234, 4354, 6124, 7062, 9182, 9683, 9946, 9792, 9793, 
                9792, 9793, 9794, 9928, 9940, 9683, 9946, 10097, 10700, 11230, 
                11360, 11460, 11360, 11460, 11586, 11613, 11615, 11686)  
    fmp.problem <- fmp.problem[-remove, ]

    ## Check that all duplicates are removed
    ## sum(duplicated(fmp.problem$did)) ## 0, all duplicates removed

    ## Now let's merge SweTrau with this fmp.problem using did
    fmp.problem$origin <- 1:nrow(fmp.problem)
    
    ## Return merged data
    return (fmp.problem)
}
