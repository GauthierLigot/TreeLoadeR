
#' Load the Most Recent Saved Database Snapshot
#'
#' Scans a directory for files and loads the most recent file (by filename sort),
#' assuming it is an `.RData` file created by `get_db(save = TRUE, ...)`
#' that stores an object named `importedDB`. Returns that object.
#'
#' @param dir Character path to an existing directory containing the saved
#'   database file(s).
#'
#' @return The `importedDB` object (a named list of tables) loaded from the
#'   most recent file. Returns `NULL` (invisibly) if the directory is missing
#'   or invalid.
#'
#' @details
#' - This function expects the `.RData` file to contain an object named
#'   `importedDB`, as produced by `get_db(save = TRUE, ...)`.
#' - The "most recent" file is chosen by sorting the file names in **descending**
#'   order; this assumes your filenames include a sortable timestamp (e.g.,
#'   `Rdatabase_YYYYmmdd_HHMMSS.RData`).
#' - No filtering by extension is performed in this basic version.
#'
#' @examples
#' \dontrun{
#' db <- get_saveddb("/path/to/snapshots")
#' names(db)
#' }
#'
#' @export
get_saveddb<-function(dir){

  if (missing(dir) || is.null(dir) || is.na(dir) || !nzchar(dir)) {
    warning("`dir` was not specified; the database cannot be loaded")
    return()
  }

  if (!dir.exists(dir)) {
    warning("`dir` does not exist: ", dir, " - the database cannot be loaded")
    return()
  }

  df <- file.info(list.files(dir, full.names = T))
  filenames<-sort(rownames(df),decreasing = T)
  lastFile<-filenames[1]
  print(paste("Imported database file :",lastFile))
  load(lastFile)

  return(importedDB)
}
