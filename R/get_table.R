
#' Retrieve a Table from the DendroPulse Database
#'
#' This function retrieves a complete table from the connected PostgreSQL
#' database. If the table name is `"xy"`, the function reads it as a spatial
#' table using `sf::st_read()`. For all other tables, a standard SQL
#' `SELECT *` query is executed using `DBI::dbGetQuery()`.
#'
#' @param con A live database connection created with
#'   `get_dendropulse()` or any valid `DBIConnection`.
#' @param tablename Character string. Name of the table to retrieve.
#'   Defaults to `"tree"`.
#'
#' @return
#' A data.frame or `sf` object containing the table contents.
#' Returns `NA` if the table does not exist.
#'
#' @details
#' - Checks if the table exists using `getTableList()`.
#' - Applies a simple date conversion if a column named `"date"` exists.
#' - Table `"xy"` is assumed to be spatial and is read using `sf::st_read()`.
#'
#' @examples
#' \dontrun{
#' con <- get_dendropulse()
#' df <- get_table(con, "tree")
#' head(df)
#'
#' xy <- get_table(con, "xy")  # returned as sf object
#' }
#'
#' @importFrom DBI dbGetQuery
#' @importFrom sf st_read
#' @export
get_table<-function(con, tablename = "tree"){

  tableList<-get_tablelist(con)

  if(!tablename %in% tableList){
    print(paste("There is no table with that name :",tablename,"- see getTableList()"))
    return(NA)
  }

  if(tablename == "xy"){
    out = sf::st_read(con,query = paste('select * from',tablename))
  }else{
    out = DBI::dbGetQuery(con,paste('select * from',tablename))
  }

  # date format correction
  if("date" %in% names(out)){
    out$date<-as.Date(out$date)
  }

  return(out)
}
