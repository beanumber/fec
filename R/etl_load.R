#' @rdname etl_extract.etl_fec
#' @importFrom DBI dbWriteTable dbListTables
#' @export
#' @examples 
#' \dontrun{
#' if (require(RMySQL)) {
#'   # must have pre-existing database "fec"
#'   # if not, try
#'   system("mysql -e 'CREATE DATABASE IF NOT EXISTS fec;'")
#'   db <- src_mysql_cnf(dbname = "fec")
#' }
#' 
#' fec <- etl("fec", db, dir = "~/dumps/fec")
#' fec %>%
#'   etl_extract() %>%
#'   etl_transform() %>%
#'   etl_init() %>%
#'   etl_load()
#' }
etl_load.etl_fec <- function(obj, years = 2014, ...) {
  
  lcl <- data_frame(
    path = list.files(attr(obj, "load_dir"), full.names = TRUE, 
                    pattern = paste0(years - 2000, "\\.csv"))) %>%
    mutate_(table = ~case_when(
      grepl("ccl", path) ~ "linkages",
      grepl("cm", path) ~ "committees",
      grepl("cn", path) ~ "candidates",
      grepl("house", path) ~ "house_elections",
      grepl("indiv", path) ~ "individuals",
      grepl("oth", path) ~ "com_to_com",
      grepl("oppexp", path) ~ "expenditures",
      TRUE ~ "contributions"
    ))
  
  # write the table directly to the DB
  message("Writing FEC data to the database...")
  mapply(DBI::dbWriteTable, name = lcl$table, value = lcl$path, 
         MoreArgs = list(conn = obj$con, append = TRUE, ... = ...))

  invisible(obj)
}
