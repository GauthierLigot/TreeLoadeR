#' List Relevant Tables in the Connected Database
#'
#' This function returns a filtered list of table names from a database
#' connection. It removes internal or system tables whose names begin with
#' common prefixes (e.g., `"street"`, `"geometry"`, `"state"`, `"zip"`, etc.).
#'
#' @param con A valid database connection created with
#'   `get_dendropulse()` or `DBI::dbConnect()`.
#'
#' @return A character vector of table names available to the user.
#'
#'
#' @examples
#' \dontrun{
#' con <- get_dendropulse()
#' tables <- getTableList(con)
#' print(tables)
#' }
#'
#' @importFrom DBI dbListTables
#' @importFrom stringr str_starts
#' @export
get_tablelist <- function(con) {

  # list all tables
  out <- DBI::dbListTables(con)

  # the following table (postgre stuff are not shown)
  out<-out[!str_starts(out,pattern = "street")]
  out<-out[!str_starts(out,pattern = "geography")]
  out<-out[!str_starts(out,pattern = "geometry")]
  out<-out[!str_starts(out,pattern = "spatial")]
  out<-out[!str_starts(out,pattern = "geocode")]
  out<-out[!str_starts(out,pattern = "direction")]
  out<-out[!str_starts(out,pattern = "zip")]
  out<-out[!str_starts(out,pattern = "county")]
  out<-out[!str_starts(out,pattern = "state")]
  out<-out[!str_starts(out,pattern = "place")]
  out<-out[!str_starts(out,pattern = "cousub")]
  out<-out[!str_starts(out,pattern = "edges")]
  out<-out[!str_starts(out,pattern = "add")]
  out<-out[!str_starts(out,pattern = "feat")]
  out<-out[!str_starts(out,pattern = "loader")]
  out<-out[!str_starts(out,pattern = "zcta")]
  out<-out[!str_starts(out,pattern = "tabblock")]
  out<-out[!str_starts(out,pattern = "bg")]
  out<-out[!str_starts(out,pattern = "faces")]
  out<-out[!str_starts(out,pattern = "layer")]
  out<-out[!str_starts(out,pattern = "pagc")]
  out<-out[!str_starts(out,pattern = "secondary")]
  out<-out[!str_starts(out,pattern = "topo")]
  out<-out[!str_starts(out,pattern = "tract")]

  return(out)

  return(out)
}

getTableList<-function(con){

  out<-dbListTables(con)
  # the following table (postgre stuff are not shown)
  out<-out[!str_starts(out,pattern = "street")]
  out<-out[!str_starts(out,pattern = "geography")]
  out<-out[!str_starts(out,pattern = "geometry")]
  out<-out[!str_starts(out,pattern = "spatial")]
  out<-out[!str_starts(out,pattern = "geocode")]
  out<-out[!str_starts(out,pattern = "direction")]
  out<-out[!str_starts(out,pattern = "zip")]
  out<-out[!str_starts(out,pattern = "county")]
  out<-out[!str_starts(out,pattern = "state")]
  out<-out[!str_starts(out,pattern = "place")]
  out<-out[!str_starts(out,pattern = "cousub")]
  out<-out[!str_starts(out,pattern = "edges")]
  out<-out[!str_starts(out,pattern = "add")]
  out<-out[!str_starts(out,pattern = "feat")]
  out<-out[!str_starts(out,pattern = "loader")]
  out<-out[!str_starts(out,pattern = "zcta")]
  out<-out[!str_starts(out,pattern = "tabblock")]
  out<-out[!str_starts(out,pattern = "bg")]
  out<-out[!str_starts(out,pattern = "faces")]
  out<-out[!str_starts(out,pattern = "layer")]
  out<-out[!str_starts(out,pattern = "pagc")]
  out<-out[!str_starts(out,pattern = "secondary")]
  out<-out[!str_starts(out,pattern = "topo")]
  #   out<-out[!str_starts(out,pattern = "zip")]
  out<-out[!str_starts(out,pattern = "tract")]

  return(out)
}
