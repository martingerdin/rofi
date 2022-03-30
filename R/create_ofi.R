#' Create Opportunities For Improvement Variable
#'
#' Create opportunities for improvement variable, which is either yes or no
#'
#' Yes: The case was flagged and reviewed in a meeting and the
#' consensus was that there were opportunities for improvement
#' (variable Problemomrade_.FMP), or that the patient died and the
#' death was determined as preventable or potentially preventable
#' (variable Fr1-14 2 or 3).
#' 
#' No: The consensus was that there were no opportunities for
#' improvement, or the nurses in the initial review did not send the
#' case for review because everything was okay (variable VK_avslutad).
#' 
#' @param data A data.frame. The data needed to create the
#'     opportunities for improvements variable. Has to include the
#'     columns VK_avslutad, Problemomrade_.FMP, and Fr1-14. No default.
create_ofi <- function(data) {
    ## Check arguments
    assertthat::assert_that(is.data.frame(data))
    assertthat::assert_that(all(c("VK_avslutad", "Problemomrade_.FMP", "Fr1-14") %in% names(data)))
    ## Create ofi variable
    data$Problemomrade_.FMP <- tolower(data$Problemomrade_.FMP)
    levels.Problemomrade_.FMP <- unique(data$Problemomrade_.FMP)
    original.levels.Problemomrade_.FMP <- c(NA, "ok", "triage på akutmottagningen",
                                            "resurs", "lång tid till op", "lång tid till dt",
                                            "vårdnivå", "traumakriterier/styrning",
                                            "missad skada", "kommunikation", "neurokirurg",
                                            "föredömligt handlagd", "logistik/teknik",
                                            "dokumentation", "dokumetation", "bristande rutin", 
                                            "handläggning", "kompetens brist", "tertiär survey")
    if (!identical(levels.Problemomrade_.FMP, original.levels.Problemomrade_.FMP))
        stop ("Levels in Problemomrade._FMP have changed.")
    levels.Fr1.14 <- unique(data$`Fr1-14`)
    original.levels.Fr1.14 <- c(NA, "1", "3", "2", "999")
    if (!identical(levels.Fr1.14, original.levels.Fr1.14))
        stop ("Levels in Fr1-14 have changed.")
    prob.filters <- with(data, `Problemomrade_.FMP` != "ok" & `Problemomrade_.FMP` != "föredömligt handlagd")
    prob.mortality <- with(data, `Fr1-14` == "2" | `Fr1-14` == "3")
    prob <- prob.filters | prob.mortality
    data$VK_avslutad <- tolower(data$VK_avslutad)
    levels.VK_avslutad <- unique(data$VK_avslutad)
    original.levels.VK_avslutad <- c("ja", NA, "nej")
    if (!identical(levels.VK_avslutad, original.levels.VK_avslutad))
        stop ("Levels in VK_avslutad have changed.")
    quality.process.done <- data$VK_avslutad == "ja"
    ofi <- ifelse(prob, "Yes",
           ifelse(quality.process.done & !prob, "No", NA))
    ofi[quality.process.done & is.na(prob)] <- "No"
    return (ofi)
}