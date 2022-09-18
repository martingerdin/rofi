#' Create and Scaffold an Opportunities for Improvement Project
#'
#' @inheritParams noacsr::create
#' @export
create <- noacsr::hijack_function(noacsr::create, setup.database.access = TRUE)
