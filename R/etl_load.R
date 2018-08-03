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
      grepl("ccl", path) ~ "cand_com_link",
      grepl("cm", path) ~ "committees",
      grepl("cn", path) ~ "candidates",
      grepl("house", path) ~ "house_elections",
      grepl("indiv", path) ~ "contrib_indiv_to_com",
      grepl("oth", path) ~ "contrib_com_to_com",
      grepl("pas2", path) ~ "contrib_com_to_cand",
      grepl("oppexp", path) ~ "expenditures",
      TRUE ~ "null"
    ))
  
  # write the table directly to the DB
  message("Writing FEC data to the database...")
  mapply(DBI::dbWriteTable, name = lcl$table, value = lcl$path, 
         MoreArgs = list(conn = obj$con, append = TRUE, ... = ...))

  invisible(obj)
}

#' @rdname etl_extract.etl_fec
#' @inheritParams etl::etl_init
#' @export

etl_init.etl_fec <- function(obj, script = NULL, 
                                  schema_name = "init", 
                                  pkg = attr(obj, "pkg"),
                                  ext = NULL, ...) {
  NextMethod(ext = "sql")
  
  # add indexes
  
  invisible(obj)
}

