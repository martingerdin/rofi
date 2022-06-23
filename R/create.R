#' Create and Scaffold an Opportunities for Improvement Project
#' 
#' Run this function to initiate an opportunities for improvement project and 
#' scaffold an appropriate folder structure. 
#' @param name Character. The name of the project. No default. 
#' @param description Character. A short, one sentence, description of the 
#' project. This is added to the DESCRIPTION file. No default. 
create <- function(name, description) {
  ## Check arguments
  for (argument in list(name, description)) 
    assertthat::assert_that(is.character(argument) & length(argument) == 1)
  ## Use devtools::create to initiate a R package folder structure
  usethis::create_package(name)
  
}