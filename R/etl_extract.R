#' ETL operations for FEC data
#' 
#' @inheritParams etl::etl_extract
#' @param years a vector of integers representing the years
#' @details If a \code{year} and/or \code{month} is specified, then
#' only flight data from matching months is used.
#' @export
#' @import etl
#' @importFrom utils download.file
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
  
  # election results
  src <- append(src, "http://www.fec.gov/pubrec/fe2012/federalelections2012.xls")

  smart_download(obj, src)
  invisible(obj)
}

#' @rdname etl_extract.etl_fec
#' @importFrom readr read_delim write_csv
#' @importFrom readxl read_excel
#' @importFrom tidyr extract_numeric
#' @import dplyr
#' @export
etl_transform.etl_fec <- function(obj, years = 2012, ...) {
  
  filenames <- c("cn12.zip", "cm12.zip", "pas212.zip", "indiv12.zip")
  src <- paste0(attr(obj, "raw_dir"), "/", filenames)
  
  lapply(src, smart_transform, obj = obj)
  
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
    dplyr::filter_(~fec_id != "n/a") %>%
    dplyr::filter_(~d != "S") %>%
    dplyr::rename_(district = ~d, incumbent = ~`(i)`, general_votes = ~general_votes_) %>%
    dplyr::select_(~state_abbreviation, ~district, ~fec_id, ~incumbent, 
                   ~candidate_name, ~party, ~primary_votes, ~runoff_votes, 
                   ~general_votes, ~ge_winner_indicator) %>%
    dplyr::mutate_(primary_votes = ~tidyr::extract_numeric(primary_votes)) %>%
    dplyr::group_by_(~fec_id) %>%
    dplyr::summarize_(state = ~max(state_abbreviation), 
                      district = ~max(district),
                      incumbent = ~max(incumbent), 
                      name = ~max(candidate_name), 
                      party = ~ifelse("R" %in% party, "R", ifelse("D" %in% party, "D", max(party))),
                      #               party = paste0(unique(party), collapse = "/"),
                      primary_votes = ~sum(primary_votes, na.rm = TRUE), 
                      runoff_votes = ~sum(runoff_votes, na.rm = TRUE),
                      general_votes = ~sum(general_votes, na.rm = TRUE),
                      ge_winner = ~max(ge_winner_indicator, na.rm = TRUE))
  readr::write_csv(house_elections, paste0(attr(obj, "load_dir"), "/house_elections_2012.csv"))
  invisible(obj)
}

#' @importFrom readr cols col_character

smart_transform <- function (obj, filename) {
  message(paste("Transforming", filename, "..."))
  src_header <- paste0("http://www.fec.gov/finance/disclosure/metadata/", 
                            gsub("12\\.zip", "_header_file.csv", basename(filename)))
  
  header <- readr::read_csv(src_header) %>%
    names() %>%
    tolower()
  
  # https://github.com/beanumber/fec/issues/3
  col_types <- readr::cols("transaction_tp" = readr::col_character())
  data <- readr::read_delim(filename, col_names = header, 
                            col_types = col_types, delim = "|")
#  data <- read.delim(filename, col.names = header, sep = "|")
 
#  names(files) <- c("candidates", "committees", "contributions", "individuals")
  
  lcl <- paste0(attr(obj, "load_dir"), "/", gsub("\\.zip", "\\.csv", basename(filename)))
  readr::write_csv(data, path = lcl, na = "")
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
    if (schema == TRUE & inherits(obj, c("src_mysql", "src_postgres"))) {
      schema <- get_schema(obj, schema_name = "init", pkg = "fec")
    }
    if (!missing(schema)) {
      if (file.exists(as.character(schema))) {
        dbRunScript(obj$con, schema, ...)
      }
    }
  }
  
  lcl <- list.files(attr(obj, "load_dir"), full.names = TRUE)
  tablenames <- c("committees", "candidates", "house_elections", "individuals", "contributions")
  
  # write the table directly to the DB
  message("Writing FEC data to the database...")
  mapply(DBI::dbWriteTable, name = tablenames, value = lcl, 
         MoreArgs = list(conn = obj$con, append = TRUE, ... = ...))

  invisible(obj)
}


