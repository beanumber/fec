#' ETL operations for FEC data
#' 
#' @inheritParams etl::etl_extract
#' @param years a vector of integers representing the years
#' @details If a \code{year} and/or \code{month} is specified, then
#' only flight data from matching months is used.
#' @export
#' @import etl
#' @examples 
#' 
#' \dontrun{
#' fec <- etl("fec", dir = "~/dumps/fec")
#' fec %>%
#'   etl_extract() %>%
#'   etl_transform() %>%
#'   etl_load()
#' }
etl_extract.etl_fec <- function(obj, years = 2012, ...) {
  
  src_root <- "ftp://ftp.fec.gov/FEC/2012/"
  src_files <- c("cn12.zip", "cm12.zip", "pas212.zip", "indiv12.zip")
  src <- paste0(src_root, src_files)
  
  lcl <- paste0(attr(obj, "raw_dir"), "/", basename(src))
  missing <- !file.exists(lcl)
  
  mapply(download.file, src[missing], lcl[missing])
  
  # election results
  src <- "http://www.fec.gov/pubrec/fe2012/federalelections2012.xls"
  lcl <- paste0(attr(obj, "raw_dir"), "/", basename(src))
  if (!file.exists(lcl)) {
    download.file(src, lcl)
  }
  
  invisible(obj)
}

#' @rdname etl_extract.etl_fec
#' @importFrom readr read_delim write_csv
#' @export
etl_transform.etl_fec <- function(obj, years = 2012, ...) {
  
  src <- paste0(attr(obj, "raw_dir"), "/", 
                      c("cn12.zip", "cm12.zip", "pas212.zip", "indiv12.zip"))
  src_headers <- paste0("http://www.fec.gov/finance/disclosure/metadata/", 
                         c("cn_header_file.csv", "cm_header_file.csv", 
                           "pas2_header_file.csv", "indiv_header_file.csv"))
  headers <- lapply(src_headers, readr::read_csv) %>%
    lapply(names)
  files <- mapply(readr::read_delim, file = src, col_names = headers, MoreArgs = list(delim = "|"))
  names(files) <- c("candidates", "committees", "contributions", "individuals")
  
  files <- lapply(files, function(x) { names(x) <- tolower(names(x)); x; })
  
  lcl <- paste0(attr(obj, "load_dir"), "/", names(files), "_2012.csv")
  mapply(readr::write_csv, files, path = lcl, na = "")
  
  # election results
  src <- paste0(attr(obj, "raw_dir"), "/federalelections2012.xls")
  # readxl::excel_sheets(src) 
  elections <- readxl::read_excel(src, sheet = 12)
  names(elections) <- names(elections) %>%
    tolower() %>%
    gsub(" ", "_", x = .) %>%
    gsub("#", "", x = .) %>%
    gsub("%", "pct", x = .)
  house_elections <- elections %>%
    filter(fec_id != "n/a") %>%
    filter(d != "S") %>%
    rename(district = d, incumbent = `(i)`, general_votes = general_votes_) %>%
    select(state_abbreviation, district, fec_id, incumbent, candidate_name, party, 
           primary_votes, runoff_votes, general_votes, ge_winner_indicator) %>%
    mutate(primary_votes = tidyr::extract_numeric(primary_votes)) %>%
    group_by(fec_id) %>%
    summarize(state = max(state_abbreviation), district = max(district),
              incumbent = max(incumbent), name = max(candidate_name), 
              party = ifelse("R" %in% party, "R", ifelse("D" %in% party, "D", max(party))),
#               party = paste0(unique(party), collapse = "/"),
              primary_votes = sum(primary_votes, na.rm = TRUE), 
              runoff_votes = sum(runoff_votes, na.rm = TRUE),
              general_votes = sum(general_votes, na.rm = TRUE),
              ge_winner = max(ge_winner_indicator, na.rm = TRUE))
  readr::write_csv(house_elections, paste0(attr(obj, "load_dir"), "/house_elections_2012.csv"))
  
  invisible(obj)
}

#' @rdname etl_extract.etl_fec
#' @importFrom DBI dbWriteTable dbListTables
#' @export
#' @examples 
#' \dontrun{
#' if (require(RMySQL)) {
#'   # must have pre-existing database "fec"
#'   # if not, try
#'   system("mysql -e 'CREATE DATABASE IF NOT EXISTS fec;'")
#'   db <- src_mysql(default.file = path.expand("~/.my.cnf"), group = "client",
#'                   user = NULL, password = NULL, dbname = "fec")
#' }
#' 
#' fec <- etl("fec", db, dir = "~/dumps/fec")
#' fec %>%
#'   etl_extract() %>%
#'   etl_transform() %>%
#'   etl_load(schema = TRUE)
#' }
etl_load.etl_fec <- function(obj, schema = FALSE, years = 2012, ...) {
  
  if (methods::is(obj$con, "DBIConnection")) {
    if (schema == TRUE & inherits(obj, "src_mysql")) {
      schema <- get_schema(obj, schema_name = "init", pkg = "fec")
    }
    if (!missing(schema)) {
      if (file.exists(as.character(schema))) {
        dbRunScript(obj$con, schema, ...)
      }
    }
  }
  
  # write the table directly to the DB
  message("Writing FEC data to the database...")
  lcl <- list.files(attr(obj, "load_dir"), full.names = TRUE)
  tablenames <- c("candidates", "committees", "contributions", "house_elections", "individuals")
  
  mapply(DBI::dbWriteTable, name = tablenames, value = lcl, 
         MoreArgs = list(conn = obj$con, append = TRUE, ... = ...))

  invisible(obj)
}


