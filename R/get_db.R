
#' Import All Tables from the Database (optionally save to disk)
#'
#' Retrieves all relevant tables from a database connection using
#' `get_tablelist()` and `get_table()`, returning them in a named list.
#' Optionally saves the resulting list as an `.RData` file when `save = TRUE`.
#'
#' If `con` is missing, a default connection is created via `get_dendropulse()`.
#'
#' @param con A live `DBIConnection`. If missing, a connection is created with
#'   `get_dendropulse()`.
#' @param save Logical; if `TRUE`, the imported database (a list) is saved
#'   to an `.RData` file on disk. Default: `FALSE`.
#' @param dir Character path to an **existing, writable directory** where
#'   the `.RData` file will be saved (required only if `save = TRUE`).
#'
#' @return A named list where each element is a data.frame or `sf` object
#'   corresponding to one table.
#'
#' @details
#' - The list of tables is obtained via `get_tablelist(con)`.
#' - Each table is fetched with `get_table(con, tablename)`.
#' - When `save = TRUE`, the function verifies that `dir` exists and the
#'   process has write permissions; it then saves the list as
#'   `Rdatabase_YYYYmmdd_HHMMSS.RData` using `base::save()`.
#' - The function does **not** disconnect `con` (caller retains control).
#'
#' @examples
#' \dontrun{
#' con <- get_dendropulse()
#' db <- get_db(con)
#'
#' # Save to an existing directory
#' db <- get_db(con, save = TRUE, dir = "/path/to/existing/folder")
#' }
#'
#' @seealso [get_tablelist()], [get_table()], [get_dendropulse()]
#' @export
get_db<-function(con=NA, save = F, dir){

  if(missing(con)){
    con<-get_dendropulse()  #get default connection
  }

  tableList <- get_tablelist(con)
  importedDB <- list()

  for(t in tableList){
    print(paste("Importing table :",t))
    importedDB[[t]]<-get_table(con, t)
  }

  if(save){

    if (missing(dir) || is.null(dir) || is.na(dir) || !nzchar(dir)) {
      warning("`dir` was not specified; the database will not be saved.")
      return(importedDB)
    }


    if (!dir.exists(dir)) {
      warning("`dir` does not exist: ", dir, " : the database will not be saved.")
      return(importedDB)
    }

    filename <- paste0("Rdatabase_",format(Sys.time(),"%Y%m%d%H%H%S"),".RData")
    print(paste0("Saving Rdata file in : ",dir,filename))
    save(importedDB,file=paste0(dir,filename))
  }

  return(importedDB)
}
