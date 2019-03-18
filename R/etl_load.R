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
#' @importFrom dplyr db_create_index
#' @export

etl_init.etl_fec <- function(obj, script = NULL, 
                                  schema_name = "init", 
                                  pkg = attr(obj, "pkg"),
                                  ext = NULL, ...) {
  NextMethod(ext = "sql")
  
  # add indexes
  dplyr::db_create_index(obj$con, "candidates", "cand_id")
  dplyr::db_create_index(obj$con, "committees", "cmte_id")
  dplyr::db_create_index(obj$con, "committees", "cand_id")
  dplyr::db_create_index(obj$con, "contrib_com_to_com", "tran_id")
  dplyr::db_create_index(obj$con, "contrib_com_to_com", "cmte_id")
  dplyr::db_create_index(obj$con, "contrib_com_to_com", "other_id")
  dplyr::db_create_index(obj$con, "contrib_com_to_cand", "tran_id")
  dplyr::db_create_index(obj$con, "contrib_com_to_cand", "cmte_id")
  dplyr::db_create_index(obj$con, "contrib_com_to_cand", "cand_id")
  dplyr::db_create_index(obj$con, "contrib_com_to_cand", "other_id")
  dplyr::db_create_index(obj$con, "contrib_indiv_to_com", "tran_id")
  dplyr::db_create_index(obj$con, "contrib_indiv_to_com", "cmte_id")
  dplyr::db_create_index(obj$con, "contrib_indiv_to_com", "other_id")
  dplyr::db_create_index(obj$con, "cand_com_link", "cand_id")
  dplyr::db_create_index(obj$con, "cand_com_link", "cmte_id")
  dplyr::db_create_index(obj$con, "expenditures", "tran_id")
  dplyr::db_create_index(obj$con, "expenditures", "cmte_id")
  dplyr::db_create_index(obj$con, "house_elections", "cand_id")
  invisible(obj)
}

