## ---- include = FALSE---------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## ----setup, include = FALSE---------------------------------------------------
library(rofi)

## ---- eval = FALSE------------------------------------------------------------
#  ## Load packages
#  library(rofi)
#  
#  ## Import data
#  imported.data <- import_data()
#  
#  ## Merge data
#  data <- merge_data(imported.data)
#  
#  ## Prepare data
#  prepared.data <- prepare_data(data)

## ---- eval = FALSE------------------------------------------------------------
#  create_function("Select variables")

## ---- eval = FALSE------------------------------------------------------------
#  #' Select Variables
#  #'
#  #' Write a short description of your function here
#  #' @param x Describe argument x here
#  #' @export
#  select_variables <- function(x) {
#      ## Replace with the contents of your function
#      y <- x + 1
#      return (y)
#  }

## ---- eval = FALSE------------------------------------------------------------
#  #' Select Variables
#  #'
#  #' Select only the variables I need for my analysis
#  #' @param x Describe argument x here
#  #' @export
#  prepare_data <- function(x) {
#      ## Replace with the contents of your function
#      y <- x + 1
#      return (y)
#  }'

## ---- eval = FALSE------------------------------------------------------------
#  #' Select Variables
#  #'
#  #' Select only the variables I need for my analysis
#  #' @param dataset The complete dataset.
#  #' @export
#  prepare_data <- function(dataset) {
#      ## Replace with the contents of your function
#      y <- x + 1
#      return (y)
#  }

## ---- eval = FALSE------------------------------------------------------------
#  #' Select Variables
#  #'
#  #' Select only the variables I need for my analysis
#  #' @param dataset The complete dataset.
#  #' @export
#  prepare_data <- function(dataset) {
#      variables.to.include <- c("pt_Gender", "pt_age_yrs", "ed_sbp_value", "ed_rr_value", "ed_gcs_sum", "NISS")
#      prepared.dataset <- dataset[, variables.to.include]
#      return (prepared.dataset)
#  }

## ---- eval = FALSE------------------------------------------------------------
#  ## Load packages
#  library(rofi)
#  
#  ## Import data
#  imported.data <- import_data()
#  
#  ## Merge data
#  data <- merge_data(imported.data)
#  
#  ## Prepare data
#  prepared.data <- prepare_data(data)
#  
#  ## Select only variables required for the analysis
#  selected.data <- select_variables(prepared.data)

## ---- eval = FALSE------------------------------------------------------------
#  browseVignettes("rofi")

## ---- eval = FALSE------------------------------------------------------------
#  vignette("introduction", "rofi")

