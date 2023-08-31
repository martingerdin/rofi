#' Prepare Data Function
#'
#' This function takes a data frame and a vector of variable names to
#' be included and performs several steps to prepare the data for
#' analysis. It adds computed variables, subsets the data to include
#' only specified variables, labels the data, and returns the prepared
#' data frame.
#'
#' @param data A data frame containing the raw data to be prepared.
#' @param variables.to.be.included A character vector specifying the
#'     names of variables to be included in the prepared data. Default
#'     is to include the default set of variables returned by
#'     \code{\link{get_default_variables_to_be_included}}.
#' @return A prepared data frame with added variables, subsetted
#'     columns, and labeled data.
#'
#' @examples
#' prepared.data <- prepare_data(data)
#'
#' @export
prepare_data <- function(data, variables.to.be.included = get_default_variables_to_be_included()) {
    ## Check arguments
    assertthat::assert_that(is.data.frame(data))
    assertthat::assert_that(all(variables.to.be.included %in% names(data)))

    ## Add variables
    data$ofi <- create_ofi(data)
    data$intub <- create_intub(data)
    variables.to.be.included <- sort(c(variables.to.be.included, "ofi", "intub"))
        
    ## Subset the data to only include commonly used variables
    data <- data[, variables.to.be.included]

    ## Label data
    data <- label_data(data)
    
    ## Return data
    return(data)
}

