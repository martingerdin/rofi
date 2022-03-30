#' Import Data
#'
#' Function to import data from the local MariaDB database.
#' @param user Character. Your username to connect to the database. No default.
#' @param db.name Character. The name of the database. Defaults to "opportunities_for_improvement".
#' @param table.names Character. The name(s) of the table(s) in the database. Defaults to c("swetrau", "fmp", "atgarder", "problem").
#' @param password Character. Your password to connect to the database. Defaults to keyring::key_get(db.name, user).
import_data <- function(user,
                        db.name = "opportunities_for_improvement",
                        table.names = c("swetrau", "fmp", "atgarder", "problem"),
                        password = keyring::key_get(db.name, user)) {
    ## Create connection to the database
    conn <- DBI::dbConnect(drv = RMariaDB::MariaDB(),
                       user = user,
                       password = keyring::key_get(db.name, user),
                       db = db.name)
    datasets <- lapply(tables.name, function(table.name) dbReadTable(conn = conn, name = table.name))
    return (datasets)
}
