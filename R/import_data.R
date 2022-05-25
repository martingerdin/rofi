#' Import Data
#'
#' Function to import data from the local MariaDB database. This
#' assumes that your username and password are available as the
#' environment variables DB_USERNAME and DB_PASSWORD. `rofi` uses the
#' package `dotenv` to access environment variables and load any .env
#' file in the project directory. We therefore recommed storing the
#' username and password in an .env file in the project directory, but
#' make sure this file is ignored by git so that you don't put it in
#' version control by mistake. All this is taken care of by using
#' `rofi::init()`.
#' @param user Character or NULL. The default is NULL in which case
#'     the username is read from the corresponding environment
#'     variable called DB_USERNAME, using Sys.getenv("DB_USERNAME").
#' @param password Character. The default is NULL in which case the
#'     password is read from the corresponding environment variable
#'     called DB_PASSWORD, using Sys.getenv("DB_PASSWORD").
#' @param db.name Character. The name of the database. Defaults to
#'     "opportunities_for_improvement".
#' @param table.names Character. The name(s) of the table(s) in the
#'     database. Defaults to c("swetrau", "fmp", "atgarder",
#'     "problem", "kvalgranskning2014.2017").
#' @param env.file Character. The name of the file defining the
#'     environment variables used to connect to the database. Defaults
#'     to ".env".
#' @param silent Logical. If FALSE a message is displayed with the
#'     names of the imported datasets. Defaults to FALSE.
#' @return A list of data.frames.
#' @export
import_data <- function(user = NULL,
                        password = NULL,
                        db.name = "opportunities_for_improvement",
                        table.names = c("swetrau",
                                        "fmp",
                                        "atgarder",
                                        "problem",
                                        "kvalgranskning2014.2017"),
                        env.file = ".env",
                        silent = FALSE) {
    ## Check arguments
    for (argument in list(user, password, db.name))
        assertthat::assert_that(is.null(argument) || (is.character(argument) & length(argument) == 1))
    assertthat::assert_that(is.character(table.names))
    assertthat::assert_that(is.logical(silent) & length(silent) == 1)
    ## Load user and password from environment variables unless passed in
    if (is.null(user) | is.null(password))
        dotenv::load_dot_env(file = env.file)
    if (is.null(user)) {
        user <- Sys.getenv("DB_USERNAME")
        if (user == "") stop ("No environment variable called DB_USERNAME was found.")
    }
    if (is.null(password)) {
        password <- Sys.getenv("DB_PASSWORD")
        if (password == "") stop ("No environment variable called DB_PASSWORD was found.")
    }
    ## Create connection to the database
    conn <- DBI::dbConnect(drv = RMariaDB::MariaDB(),
                       user = user,
                       password = password,
                       db = db.name)
    datasets <- lapply(setNames(nm = table.names), function(table.name) DBI::dbReadTable(conn = conn, name = table.name))
    DBI::dbDisconnect(conn)
    if (!silent)
        message(paste0("The datasets ", paste0(names(datasets), collapse = ", "), " have been imported."))
    return (datasets)
}
