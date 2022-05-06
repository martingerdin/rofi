#' Import Data
#'
#' Function to import data from the local MariaDB database. This assumes that your username and password are available as the environment variables DB_USERNAME and DB_PASSWORD. `rofi` uses the package `dotenv` to access environment variables and load any .env file in the project directory. We therefore recommed storing the username and password in an .env file in the  project directory, but make sure this file is ignored by git so that you don't put it in version control by mistake. All this is taken care of by using `rofi::init()`. 
#' @param user Character. Your username to connect to the database. Defaults to Sys.getenv("DB_USERNAME").
#' @param password Character. Your password to connect to the database. Defaults to Sys.getenv("DB_PASSWORD"). 
#' @param db.name Character. The name of the database. Defaults to "opportunities_for_improvement".
#' @param table.names Character. The name(s) of the table(s) in the database. Defaults to c("swetrau", "fmp", "atgarder", "problem").
#' @return A list of data.frames.
import_data <- function(user = Sys.getenv("DB_USERNAME"),
                        password = Sys.getenv("DB_PASSWORD"),
                        db.name = "opportunities_for_improvement",
                        table.names = c("swetrau", "fmp", "atgarder", "problem")) {
    ## Create connection to the database
    conn <- DBI::dbConnect(drv = RMariaDB::MariaDB(),
                       user = user,
                       password = password,
                       db = db.name)
    datasets <- lapply(setNames(nm = table.names), function(table.name) DBI::dbReadTable(conn = conn, name = table.name))
    DBI::dbDisconnect(conn)
    return (datasets)
}
