#' Create detailed OFI categories
#'
#' This function creates detailed Opportunities for Improvement (OFI) categories
#' based on the 'Problemomrade_.FMP' column in the input data.
#'
#' @param data A data frame containing the 'Problemomrade_.FMP' column
#' @return A character vector of detailed OFI categories
#' @export
create_detailed_ofi_categories <- function(data) {
  # Check if data is a data frame
  if (!is.data.frame(data)) {
    stop("Input must be a data frame")
  }

  # Check if the required column exists
  if (!"Problemomrade_.FMP" %in% colnames(data)) {
    stop("Column 'Problemomrade_.FMP' not found in the data")
  }

  # Check if the column is of character type
  if (!is.character(data$Problemomrade_.FMP)) {
    stop("Column 'Problemomrade_.FMP' must be of character type")
  }

  input <- data$Problemomrade_.FMP |> tolower()


  # Define the expected unique values
  expected.values <- c(
    NA, "ok", "nej", "inget problemområde", "föredömligt handlagd", "dokumentation",
    "dokumetation", "handläggning", "logistik/teknik", "lång tid till op", "lång tid till dt",
    "kompetens brist", "kommunikation", "kommunikation+missad skada",
    "handläggning/logistik", "handläggning+dokumentation", "handläggning prehosp",
    "traumakriterier/styrning", "tertiär survey", "bristande rutin", "annat",
    "missad skada", "resurs", "triage på akm", "triage på akutmottagningen",
    "vårdnivå", "vårdnivå+\r\nmissade skador", "handläggning\r\ndokumentation", "neurokirurg"
  )

  # Check if all actual values can be found in expected values
  missing.values <- setdiff(unique(input), expected.values)

  if (length(missing.values) > 0) {
    stop(paste(
      "Values found in 'Problemomrade_.FMP' that are not in expected values:",
      paste(missing.values, collapse = ", ")
    ))
  }

  result <- stringr::str_replace_all(
    input,
    c(
      "^ok$" = NA_character_,
      "^nej$" = NA_character_,
      "^inget problemområde$" = NA_character_,
      "^föredömligt handlagd$" = NA_character_,
      "^dokumentation$" = "Documentation",
      "^dokumetation$" = "Documentation",
      "^handläggning$" = "Patient management",
      "^annat$" = "Other",
      "^kommunikation\\+missad skada$" = "Communication + missed injury",
      "^handläggning/logistik$" = "Patient management/logistics",
      "^handläggning\\+dokumentation$" = "Patient management + documentation",
      "^handläggning prehosp$" = "Prehospital management",
      "^traumakriterier/styrning$" = "Trauma criteria/guidelines",
      "^tertiär survey$" = "Tertiary survey",
      "^bristande rutin$" = "Inadequate routine",
      "^missad skada$" = "Missed injury",
      "^resurs$" = "Resources",
      "^triage på akm$" = "Triage in the ED",
      "^triage på akutmottagningen$" = "Triage in the ED",
      "^vårdnivå$" = "Level of care",
      "^vårdnivå\\+\r\nmissade skador$" = "Level of care + missed injury",
      "^handläggning\r\ndokumentation$" = "Patient management + documentation",
      "^lång tid till op$" = "Delay to surgery",
      "^logistik/teknik$" = "Logistics/technical",
      "^lång tid till dt$" = "Delay to CT",
      "^kompetens brist$" = "Competence",
      "^kommunikation$" = "Communication",
      "^neurokirurg$" = "Neurosurgeon"
    )
  )

  # Ensure no input values are left in the result
  unreplaced_indices <- which(result == input)
  if (length(unreplaced_indices) > 0) {
    unreplaced_values <- unique(result[unreplaced_indices])
    stop(paste(
      paste(unreplaced_values, collapse = ", ")
    ))
  }

  return(result)
}

#' Create broad OFI categories
#'
#' This function creates broad Opportunities for Improvement (OFI) categories
#' based on the detailed categories created by create_detailed_ofi_categories().
#'
#' @param data A data frame containing the 'Problemomrade_.FMP' column
#' @return A character vector of broad OFI categories
#' @export
create_broad_ofi_categories <- function(data) {
  # Check if data is a data frame
  if (!is.data.frame(data)) {
    stop("Input must be a data frame")
  }

  detailed_categories <- create_detailed_ofi_categories(data)

  # Check if detailed_categories is a character vector
  if (!is.character(detailed_categories)) {
    stop("Detailed categories must be a character vector")
  }

  dplyr::case_when(
    is.na(detailed_categories) ~ NA_character_,
    detailed_categories %in% c("Missed injury", "Tertiary survey", "Communication + missed injury") ~ "Missed diagnosis",
    detailed_categories %in% c("Delay to surgery", "Delay to CT") ~ "Delay in treatment",
    detailed_categories %in% c("Triage in the ED", "Level of care", "Patient management", "Communication", "Patient management + documentation") ~ "Clinical judgement error",
    detailed_categories %in% c("Documentation") ~ "Documentation Issues",
    detailed_categories %in% c("Technical error") ~ "Technical error",
    detailed_categories %in% c("Trauma criteria/guidelines", "Inadequate routine") ~ "Inadequate protocols",
    detailed_categories %in% c("Competence", "Resources", "Logistics/technical") ~ "Inadequate resources",
    detailed_categories %in% c("Other", "Patient management/logistics", "Prehospital management", "Level of care + missed injury", "Neurosurgeon") ~ "Other errors",
    TRUE ~ "Other (not classified)"
  )
}

#' Add detailed OFI categories to data
#'
#' This function adds detailed Opportunities for Improvement (OFI) categories to the input data frame.
#'
#' @param data A data frame containing the 'Problemomrade_.FMP' column
#' @return The input data frame with a new column 'ofi.categories.detailed'
#' @export
add_detailed_ofi_categories <- function(data) {
  data$ofi.categories.detailed <- create_detailed_ofi_categories(data)
  return(data)
}

#' Add broad OFI categories to data
#'
#' This function adds broad Opportunities for Improvement (OFI) categories to the input data frame.
#'
#' @param data A data frame containing the 'Problemomrade_.FMP' column
#' @return The input data frame with a new column 'ofi.categories.broad'
#' @export
add_broad_ofi_categories <- function(data) {
  data$ofi.categories.broad <- create_broad_ofi_categories(data)
  return(data)
}

#' Add OFI categories to data
#'
#' This function adds both detailed and broad Opportunities for Improvement (OFI)
#' categories to the input data frame.
#'
#' @param data A data frame containing the 'Problemomrade_.FMP' column
#' @return The input data frame with two new columns: 'ofi.categories.detailed' and 'ofi.categories.broad'
#' @export
add_ofi_categories <- function(data) {
  data$ofi.categories.detailed <- create_detailed_ofi_categories(data)
  data$ofi.categories.broad <- create_broad_ofi_categories(data)
  return(data)
}
