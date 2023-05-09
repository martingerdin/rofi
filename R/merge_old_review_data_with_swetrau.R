#' Merge old review data with SweTrau
#'
#' This function merges review data from the period 2014 to 2017 with SweTrau. During this period the review process was not formalised and the data structure different, so it takes quite a but of manual data manipulation to get it to work.
#' @param swetrau.data A data.frame. The SweTrau data. No default.
#' @param old.review.data A data.frame. The review data from the period 2014-2017. No default.
merge_old_review_data_with_swetrau <- function(swetrau.data, old.review.data) {
    assertthat::assert_that(is.data.frame(swetrau.data))
    assertthat::assert_that(is.data.frame(old.review.data))

    kval <- old.review.data
    swe <- swetrau.data

    ## Create variable origin in kval, to be able to check how many from
    ## kval are later present in the merged data
    kval$origin <- "kval"

    ## Create id variable, which is equal to personnummer if present,
    ## otherwise temporary id (reservnummer)
    kval$id <- kval$pat_personnummer
    kval$id[is.na(kval$pat_personnummer)] <- kval$pat_TempPersonnummer[is.na(kval$pat_personnummer)]
    swe$id <- swe$PersonIdentity
    swe$id[is.na(swe$id)] <- swe$TempIdentity[is.na(swe$PersonIdentity)]
    ## Manually found missing tra_id
    kval[285,"tra_id"] <- 24432

    ## Combine id variable with date (year-month-day) of arrival to hospital
    kval$did <- paste(kval$id, as.Date(strptime(kval$DateTime_ArrivalAtHospital, format = "%Y-%m-%d %H:%M")))
    swe$did <- paste(swe$id, as.Date(strptime(swe$DateTime_ArrivalAtHospital, format = "%Y-%m-%d %H:%M")))

    ## Create new date of arrival variable
    kval$arrival <- as.Date(strptime(kval$DateTime_ArrivalAtHospital, format = "%Y-%m-%d %H:%M"))
    swe$arrival <- as.Date(strptime(swe$DateTime_ArrivalAtHospital, format = "%Y-%m-%d %H:%M"))

    ## Check number of duplicate did in kval and SweTrau
    ## sum(duplicated(kval$did)) ## 0 duplicates
    ## sum(duplicated(swe$did)) ## 2 duplicates

    ## Check number of patients from kval that are not present in SweTrau
    not.in.swetrau <- !(kval$did %in% swe$did)
    ## sum(not.in.swetrau) ## 9 patients from kval are not present in SweTrau

    ## Check number of matched patients if we merge on did
    ## sum(merge(swe, kval, by = "did", all.x = TRUE)$origin == "kval", na.rm = TRUE) ## 596 patients are matched, i.e. 605 - 9, which makes sense

    ## The result of the above is the same as if merging on each of the
    ## variables included in did "separately", i.e:
    ## sum(merge(swe, kval,
    ##           by.x = c("PersonIdentity", "TempIdentity", "arrival"),
    ##           by.y = c("pat_personnummer", "pat_TempPersonnummer", "arrival"),
    ##           all.x = TRUE)$origin == "kval", na.rm = TRUE) ## Also results in 9 missng

    ## Check if tra_id or pat_id of those in kval whose did is missing from SweTrau
    ## are present in SweTrau
    ## sum(kval$tra_id[not.in.swetrau] %in% swe$tra_id) ## The tra_id of 4/9 patients are in SweTrau
    ## sum(kval$pat_id[not.in.swetrau] %in% swe$pat_id) ## The pat_id of 4/9 patients are in SweTrau
    ## identical(sum(kval$tra_id[not.in.swetrau] %in% swe$tra_id), sum(kval$pat_id[not.in.swetrau] %in% swe$pat_id)) ## It's the same 4 patients whose tra_id and pat_id are in SweTrau, so it doesn't matter if we try matching on tra_id or pat_id

    ##
    ## NEW CODE - need to change pat_id, screened and know these two are wrong and should be:
    kval[is.element(kval$pat_id,c(33973)),"tra_id"] <- c(36078)
    kval[is.element(kval$pat_id,c(33922)),"tra_id"] <- c(36024)
    ##

    ## Try matching some extra patients based on tra_id 
    merged <- merge(swe, kval, by = "did", all.x = TRUE)
    merged$tra_id.y <- NULL
    merged$tra_id <- merged$tra_id.x
    merged$tra_id.x <- NULL
    kval.not.matched <- kval[not.in.swetrau, ]

    ##
    ## NEW CODE
    ##

    ##
    ## To keep it clean and not duplicate all columns again i want to clean all .x columns between merges. 
    ##
    col.names.x <- names(dplyr::select(merged, ends_with(".x")))
    col.names <- gsub(".x", "", col.names.x)
    col.names.y <- paste(col.names, ".y", sep = "")

    ##
    ## Loop keeps values from .x (swe), 
    ## but if they are empty insert values from .y (kval) insted.
    ## Manually checked, extremely rare for both .x and .y to exist and when it does, .y is usually an obvious error.
    ##
    merged2 <- merged
    for (x in 1:length(col.names.x)){
        merged2[is.na(merged2[,col.names.x[x]]) == TRUE, col.names.x[x]] <- 
            merged2[is.na(merged2[,col.names.x[x]]) == TRUE,col.names.y[x]]
    }

    ## Removes excess columns (.y) since .y is inserted into .x wherever .x was empty.
    merged2[,col.names.y] <- NULL
    ##

    ## change .x to original names
    colnames(merged2) <- gsub(".x", "", colnames(merged2))
    ##

    merged <- merged2
    ##
    ## END new code
    ##

    merged <- merge(merged, kval.not.matched, by = "tra_id", all.x = TRUE)
    merged.y <- merged[]

    ## Compare did on these newly matched cases
    with(merged, cbind(did.x[!is.na(did.y)], did.y[!is.na(did.y)])) ## Seems like they differ on the date, which are maybe one day off between the two datasets. I suggest that we keep the swetrau did

    ## Keep SweTrau did
    merged$did <- merged$did.x
    merged$did.x <- NULL
    ## Check how many additional matches we got
    ## sum(merged$origin.x == "kval" | merged$origin.y == "kval", na.rm = TRUE ) ## 600, so we matched four additional patients, leaving 5 unmatched

    ## This is probably as good as it gets, and we'll have to live with
    ## having 5 unmatched patients between kval and SweTrau

    ##
    ## OK clean again, since we merged.
    ##
    col.names.x <- names(dplyr::select(merged,ends_with(".x")))
    col.names <- gsub(".x", "", col.names.x)
    col.names.y <- paste(col.names, ".y", sep = "")

    merged2 <- merged
    for (x in 1:length(col.names.x)) {
        merged2[is.na(merged2[, col.names.x[x]]) == TRUE, col.names.x[x]] <- 
            merged2[is.na(merged2[, col.names.x[x]]) == TRUE, col.names.y[x]]
    }

    ## Removes excess collumnes (.y) since .y is inserted into .x wherever .x was empty.
    merged2[, col.names.y] <- NULL
    ##

    ## change .x to original names
    colnames(merged2) <- gsub(".x", "", colnames(merged2))

    merged <- merged2

    ## Return merged data
    return(merged)
}
