#' Connect to the DendroPulse PostgreSQL Database
#'
#' This function creates a connection to the *dendro_pulse* PostgreSQL database.
#' By default, it uses the read‑only "pulse" account.
#' If `admin = TRUE`, the function prompts the user to manually enter a username
#' and password via the console (safer for admin access).
#'
#' @param admin Logical.
#'   - `FALSE` (default): Connect using the read‑only credentials.
#'   - `TRUE`: Prompt interactively for an admin username and password.
#'
#' @return
#' A `DBIConnection` object if the connection is successful.
#' Returns `NULL` and prints an error message if the connection fails.
#'
#' @details
#' - Uses the **RPostgres** driver to establish the connection.
#'
#' @examples
#' \dontrun{
#' # Connect with read‑only access
#' con <- get_dendropulse()
#'
#' # Connect with admin credentials
#' con <- get_dendropulse(admin = TRUE)
#' }
#'
#' @export
get_dendropulse<-function(admin=F){
  dsn_database = "dendro_pulse"     # Specify the name of your Database
  dsn_hostname = "gxgfscotty.gxabt.priv"
  dsn_port = "5440"                 # Specify your port number. e.g. 98939
  dsn_uid = "pulse"                 # pulse is a read-only user ... Specify your username. e.g. "admin"
  dsn_pwd = "gfpulse"               # Specify your password. e.g. "xxx"

  if(admin){
    dsn_uid <- readline(prompt = "Enter user name (UID): ")
    dsn_pwd <- readline(prompt = "Enter user password (UID): ")
  }

  ### connect to db
  tryCatch({
    drv <- RPostgres::Postgres()
    con <- DBI::dbConnect(drv,
                     dbname = dsn_database,
                     host = dsn_hostname,
                     port = dsn_port,
                     user = dsn_uid,
                     password = dsn_pwd)
    return(con)
  },
  error=function(cond) {
    print("Unable to connect to Database.")
  })
}
