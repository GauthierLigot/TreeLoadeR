
#' Build a Joined Record Table from the Imported Database
#'
#' Joins the `record` table with related metadata tables
#' (`calibration`, `dendrometer`, `tree`, `species`, `site`)
#' to produce a single, enriched data frame with commonly used fields.
#'
#' @param db A named list (e.g., from `get_db()` or `get_saveddb_*()`)
#'   containing data frames (or tibbles) named:
#'   `record`, `calibration`, `dendrometer`, `tree`, `species`, and `site`.
#'
#' @return A data frame (typically a tibble) of records joined with
#'   calibration, dendrometer, tree, species, and site information.
#'
#' @details
#' - Uses `dplyr::left_join()` with `dplyr::join_by()` on the appropriate keys.
#' - Selects a subset of columns from `calibration` (`id_calibration:span0`)
#'   and from `dendrometer` (`id_dendrometer:dendrometer_code`).
#' - Drops the `comment` column from `tree` before joining.
#'
#' @examples
#' \dontrun{
#' db <- get_db(con)               # or get_saveddb_rds("path")
#' records <- getRecordTable(db)
#' dplyr::glimpse(records)
#' }
#'
#' @importFrom dplyr %>% left_join select join_by
#' @export
get_records<-function(db){

  records <- db$record %>%
    left_join(select(db$calibration,id_calibration:span0),
                     by = join_by(id_calibration))%>%
    left_join(select(db$dendrometer,id_dendrometer:dendrometer_code),
                     by = join_by(id_dendrometer))%>%
    left_join(select(db$tree,-comment),
                     by = join_by(id_tree))%>%
    left_join(db$species,
                     by = join_by(id_species))%>%
    left_join(select(db$site,id_site,id_project,management_unit_level_3),
                     by = join_by(id_site))

  return(records)
}



