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
#' @export
merge_data <- function(datasets , test = FALSE) {
    ## Ideally all "checks" below should be refactored as tests
    
    ## Check arguments
    assertthat::assert_that(is.list(datasets))
    if (test == TRUE) {
        dataset.names <- c("swetrau_scrambled", "fmp_scrambled", "atgarder_scrambled", "problem_scrambled", "kvalgranskning2014.2017_scrambled")
    } else {
        dataset.names <- c("swetrau",
                           "swetrau.20210602.20230228",
                           "atgarder",
                           "atgarder.20210602.20230228",
                           "problem",
                           "fmp",
                           "fmp.20210602.20230228",
                           "kvalgranskning2014.2017")
    }
    
    assertthat::assert_that(all(dataset.names %in% names(datasets)))

    ## First merge old review data with swetrau
    old.review.data.and.swetrau <- merge_old_review_data_with_swetrau(
        swetrau.data = datasets$swetrau,
        old.review.data = datasets$kvalgranskning2014.2017
    )

    ## Now merge fmp and problem
    fmp.and.problem <- merge_fmp_with_problem(
        fmp.data = datasets$fmp,
        problem.data = datasets$problem
    )
    
    
    merged.swetrau.fmp.problem <- merge(merged, fmp.problem, by = "did", all.x = TRUE)

    ## Check how many from swetrau that were not matched in fmp.problem
    ## sum(is.na(merged.swetrau.fmp.problem$origin.y)) ## 264 cases from SweTrau are not in fmp.problem
    not.in.fmp.problem <- merged[!(merged$did %in% fmp.problem$did), c("did", "did", "id", "PersonIdentity", "TempIdentity", "Gender", "DateTime_ArrivalAtHospital", "arrival")]
    ## nrow(not.in.fmp.problem)
    ## to.keep <- apply(not.in.fmp.problem, 1, function(case) {
    ##     matching.cases <- fmp.problem[fmp.problem$id == case["id.x"], ]
    ##     if (nrow(matching.cases) > 0) {
    ##         print(case)
    ##         cat("\n")
    ##         print(matching.cases)
    ##         message("Keep? (Enter index and Enter or just Enter to discard)")
    ##         row.to.keep <- as.numeric(readLines(n = 1))
    ##         if (!is.na(row.to.keep)) {
    ##             return(matching.cases[row.to.keep, ])
    ##         } else {
    ##             return(NULL)
    ##         }
    ##     } else {
    ##         return(NULL)
    ##     }
    ## })

    ## In most cases where there is a match on personal number or
    ## temporary number but not on did the date is wrong, for example the
    ## year is entered in fmp as 2021 instead of 2020. In the majority of
    ## cases there are no matches in fmp however

    ## Change did in fmp to same as in swetrau for matching cases
    ## to.keep <- to.keep[!sapply(to.keep, is.null)]
    ## swetrau.rows <- as.numeric(names(to.keep))
    ## fmp.problem.rows <- as.numeric(sapply(to.keep, function(x) row.names(x)))

    ## Vectors were obtained with dput
    fmp.problem.rows <- c(1423, 2825, 9073, 11624, 11583, 11749, 11880, 11881, 11913, 11967)
    swetrau.rows <- c(1571, 2854, 8962, 11387, 11388, 11578, 11670, 11671, 11691, 11764)
    fmp.problem[fmp.problem.rows, "did"] <- merged[swetrau.rows, "did"]

    ## Redo merge
    merged.swetrau.fmp.problem <- merge(merged, fmp.problem, by = "did", all.x = TRUE)
    ## sum(is.na(merged.swetrau.fmp.problem$origin.y)) ## 262 cases are still missing from swetrau in fmp.problem

    merged <- merged.swetrau.fmp.problem

    ##
    ## Combine ID:S
    ##

    merged[is.na(merged[, "PersonIdentity"]) == TRUE, "PersonIdentity"] <- 
        merged[is.na(merged[, "PersonIdentity"]) == TRUE, "pat_personnummer"]

    merged[is.na(merged[, "TempIdentity"]) == TRUE, "TempIdentity"] <- 
        merged[is.na(merged[, "TempIdentity"]) == TRUE, "pat_TempPersonnummer"]

    ##merged$pat_personnummer <- NULL     Shold we remove?
    ##merged$pat_TempPersonnummer <- NULL

    ## Columns that should be translated into another column, cant find another way but manual? 
    VK.colnames <- c("VK_hlr_thorak","VK_sap_less90","VK_iss_15_ej_iva",
                     "VK_gcs_less9_ej_intubTE","VK_mer_30min_DT","VK_mer_60min_interv")

    kval.colnames <- c("Antal_thorakotomier_JN","sap_less_90_JN","ISS_moore_15_not_IVA",
                       "gcs_less_9_ej_intub_TE","DT_moore_30.minuter_ejTR3","akut_intervent_moore_60_min")

    merged2 <- merged
    for (x in 1:length(VK.colnames)){
        merged2[, VK.colnames[x]] <- with(merged2, ifelse(merged2[, kval.colnames[x]] == 1 & is.na(merged2[, VK.colnames[x]]) == TRUE, "Ja", merged2[, VK.colnames[x]]))
        merged2[, VK.colnames[x]] <- with(merged2, ifelse(merged2[, kval.colnames[x]] == 0 & is.na(merged2[, VK.colnames[x]]) == TRUE, "Nej", merged2[, VK.colnames[x]]))
    }
    ## Now all kval.colnames should be in VK_ collumns instead, hence remove

    merged2[, kval.colnames] <- NULL

    ## Convert problemområde to problemområde_.FMP
    ##merged2$Problemomrade_.FMP <- with(merged2, ifelse(is.na(merged2$Problemomrade_.FMP) == TRUE, `problemområde`, `Problemomrade_.FMP`))
    merged2$Problemomrade_.FMP[is.na(merged2$Problemomrade_.FMP)] <- merged2$problemområde[is.na(merged2$Problemomrade_.FMP)]
    merged2[,"problemområde"] <- NULL

    ## Need to fill in VK_avslutad to get them through create_ofi?
    ## Need to get some data for mortality?

    merged2$VK_avslutad <- with(merged2, ifelse(is.na(merged2$bedomn_primar_granskning) == FALSE, "Ja", `VK_avslutad`))
    ## Dont want to remove "bedomn_primar_granskning" since its an easy way of identifying patients from kvaldata.

    ## To convert "riktlinjer" to corresponding VK_ column

    torakotomi_list <- c("Torakotomi","Torakotomi","Thoracotomi\r\nMassiv tranfusion","Nöd thoracotomi\r\nMassiv tranfusion")
    spleen_list <- c("Mjältskada","Lever skada\r\nMjältskada\r\nMassiv transfusion")
    transfusion_list <- c("Thoracotomi\r\nMassiv tranfusion","Nöd thoracotomi\r\nMassiv tranfusion",
                          "Massiv transfusion\r\nEj nödthoracotomi  el. bäcken - stabil cirk vid ankomst","Massiv transfusion",
                          "Lever skada\r\nMjältskada\r\nMassiv transfusion","Nöd thoracotomi\r\nMassiv tranfusion")
    liver_list <- c("Leverskada - ej extravasering","Leverskada","Lever skada\r\nMjältskada\r\nMassiv transfusion")
    merged3 <- merged2

    merged3$VK_hlr_thorak[merged3$Riktlinje %in% torakotomi_list] <- "Ja"
    merged3$VK_mjaltskada[merged3$Riktlinje %in% spleen_list] <- "Ja"
    merged3$VK_mass_transf[merged3$Riktlinje %in% transfusion_list] <- "Ja"
    merged3$VK_leverskada[merged3$Riktlinje %in% liver_list] <- "Ja"

    ## Change column class
    merged3$dt_alarm_hosp <- as.numeric(merged3$dt_alarm_hosp)
    merged3$dt_alarm_scene <- as.numeric(merged3$dt_alarm_scene)
    merged3$dt_ed_emerg_proc <- as.numeric(merged3$dt_ed_emerg_proc)
    merged3$dt_ed_first_ct <- as.numeric(merged3$dt_ed_first_ct)
    merged3$ISS <- as.numeric(merged3$ISS)
    merged3$NISS <- as.numeric(merged3$NISS)
    merged3$pt_age_yrs <- as.numeric(merged3$pt_age_yrs)

    merged3$DateTime_ArrivalAtHospital <- as.POSIXct(strptime(merged3$DateTime_ArrivalAtHospital, format = "%Y-%m-%d %H:%M"))
    merged3$DateTime_LeaveScene  <- as.POSIXct(strptime(merged3$DateTime_LeaveScene, format = "%Y-%m-%d %H:%M"))
    merged3$DateTime_of_Alarm  <- as.POSIXct(strptime(merged3$DateTime_of_Alarm, format = "%Y-%m-%d %H:%M"))
    merged3$DateTime_Of_Trauma <- as.POSIXct(strptime(merged3$DateTime_Of_Trauma, format = "%Y-%m-%d %H:%M"))
    merged3$DateTime_ArrivalAtScene <- as.POSIXct(strptime(merged3$DateTime_ArrivalAtScene, format = "%Y-%m-%d %H:%M"))

    ## Rename combined datasets
    merged <- merged3

    ## Remove extra columns?

    #columns.to.remove <- c("id.x", "id.y", "arrival.x", "arrival.y", "origin.x", "origin.y")
    #for (column in columns.to.remove) combined.datasets[, column] <- NULL
    ##
    ## 
    ## I suggest keeping id, origin and arrival?
    ##
    ## OK clean again, since we merged.
    ##
    col.names.x <- names(dplyr::select(merged,ends_with(".x")))
    col.names <- gsub(".x", "", col.names.x)
    col.names.y <- paste(col.names, ".y", sep = "")
    
    merged2 <- merged
    for (x in 1:length(col.names.x)) {
        merged2[is.na(merged2[,col.names.x[x]]) == TRUE, col.names.x[x]] <- 
            merged2[is.na(merged2[,col.names.x[x]]) == TRUE,col.names.y[x]]
    }
    
    ## Removes excess collumnes (.y) since .y is inserted into .x wherever .x was empty.
    merged2[, col.names.y] <- NULL
    ##
    
    ## change .x to original names
    colnames(merged2) <- gsub(".x", "", colnames(merged2))
    
    combined.datasets <- merged2
    combined.datasets$did.y <- NULL
    return(combined.datasets)
}
