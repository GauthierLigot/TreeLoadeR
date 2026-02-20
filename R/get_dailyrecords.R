#' Build an Intra-Annual Record Table (Daily Unique by Calibration)
#'
#' Joins record data with related tables and computes a `day` index
#' since a fixed reference date. For each `(id_calibration, day)` pair,
#' the function keeps the first row (to ensure one observation per day per calibration).
#'
#' @param db A named list containing tables `record`, `calibration`,
#'   `dendrometer`, `tree`, `species`, and `site` (e.g., from `get_db()`).
#'
#' @return A data frame/tibble of intra-annual records, with one row per
#'   `(id_calibration, day)` pair and a `day` column (numeric days since the
#'   reference date).
#'
#' @details
#' - The reference date is set in the code as `"2024-01-01"`. Adjust as needed.
#' - The `day` value is computed via `difftime(datetime, "2024-01-01", units = "days")`
#'   and then converted to numeric.
#' - The function groups by `id_calibration` and `day`, and keeps the first row
#'   via `dplyr::slice(1)`, then ungroups.
#'
#' @examples
#' \dontrun{
#' ia <- getIntraAnnualRecord(db)
#' dplyr::count(ia, id_calibration, day)
#' }
#'
#' @importFrom dplyr %>% left_join select join_by mutate group_by slice ungroup
#' @export

get_dailyrecords <- function(db) {

  records <- db$record %>%
    left_join(select(db$calibration,id_calibration:span0),by = join_by(id_calibration))%>%
    left_join(select(db$dendrometer,id_dendrometer:dendrometer_code), by = join_by(id_dendrometer))%>%
    left_join(select(db$tree,-comment), by = join_by(id_tree))%>%
    left_join(db$species, by = join_by(id_species))%>%
    left_join(select(db$site,id_site,id_project,management_unit_level_3), by = join_by(id_site))%>%
    mutate(day = as.numeric(difftime(datetime,"2024-1-1", units = "days")))%>%
    group_by(id_calibration,day)%>%
    slice(1)%>%
    ungroup()

  return(records)
}

