#' ETL operations for FEC data
#' 
#' @inheritParams etl::etl_extract
#' @param years a vector of integers representing the years
#' @details If a \code{year} and/or \code{month} is specified, then
#' only flight data from matching months is used.
#' @export
#' @import etl
#' @importFrom utils download.file
#' @source \url{http://www.fec.gov/finance/disclosure/ftpdet.shtml}
#' 
#' @examples
#' \dontrun{
#' fec <- etl("fec", dir = "~/dumps/fec")
#' fec %>%
#'   etl_extract() %>%
#'   etl_transform() %>%
#'   etl_init() %>%
#'   etl_load()
#' }
etl_extract.etl_fec <- function(obj, years = 2012, ...) {
  
  src <- lapply(years, get_filenames) %>%
    unlist()
  
  # election results
  src <- append(src, paste0("http://www.fec.gov/pubrec/fe",years,"/federalelections",years,".xls"))

  etl::smart_download(obj, src)
  invisible(obj)
}


get_filenames <- function(year) {
  valid_years <- c(2012, 2014, 2016)
  year <- intersect(year, valid_years)
  gen_files <- c("cn", "cm", "pas2", "indiv")
  if (length(year) > 0) {
    year_end <- substr(year, 3, 4)
    return(paste0("ftp://ftp.fec.gov/FEC/", year, "/", gen_files, year_end, ".zip"))
  } else {
    return(NULL)
  }
}


#' @rdname etl_extract.etl_fec
#' @importFrom readr read_delim write_csv
#' @importFrom readxl read_excel
#' @importFrom tidyr extract_numeric
#' @import dplyr
#' @export
etl_transform.etl_fec <- function(obj, years = 2012, ...) {
  
  src <- lapply(years, get_filenames) %>%
    unlist()
  src <- paste0(attr(obj, "raw_dir"), "/", basename(src))
  
  lapply(src, smart_transform, obj = obj)
  
  # election results
  src <- paste0(attr(obj, "raw_dir"), "/federalelections",years,".xls")
  # readxl::excel_sheets(src) 
  elections <- readxl::read_excel(src, sheet = 12)
  names(elections) <- names(elections) %>%
    tolower() %>%
    gsub(" ", "_", x = .) %>%
    gsub("#", "", x = .) %>%
    gsub("%", "pct", x = .)
  house_elections <- elections %>%
    dplyr::filter_(~fec_id != "n/a", ~d != "S") %>%
    dplyr::rename_(district = ~d, incumbent = ~`(i)`, general_votes = ~general_votes_) %>%
    dplyr::select_(~state_abbreviation, ~district, ~fec_id, ~incumbent, 
                   ~candidate_name, ~party, ~primary_votes, ~runoff_votes, 
                   ~general_votes, ~ge_winner_indicator) %>%
    dplyr::mutate_(primary_votes = ~tidyr::extract_numeric(primary_votes),
                   district = ~trimws(district),
                   is_incumbent = ~incumbent == "(I)") %>%
    dplyr::group_by_(~fec_id) %>%
    dplyr::summarize_(state = ~max(state_abbreviation), 
                      district = ~max(district),
                      incumbent = ~sum(is_incumbent, na.rm = TRUE) > 0, 
                      name = ~max(candidate_name), 
                      party = ~ifelse("R" %in% party, "R", 
                                      ifelse("D" %in% party, "D", max(party))),
                      #               party = paste0(unique(party), collapse = "/"),
                      primary_votes = ~sum(primary_votes, na.rm = TRUE), 
                      runoff_votes = ~sum(runoff_votes, na.rm = TRUE),
                      general_votes = ~sum(general_votes, na.rm = TRUE),
                      ge_winner = ~max(ge_winner_indicator, na.rm = TRUE))
  readr::write_csv(house_elections, paste0(attr(obj, "load_dir"), "/house_elections_",years,".csv"))
  invisible(obj)
}

#' @importFrom readr cols col_character

smart_transform <- function(obj, filename) {
  message(paste("Transforming", filename, "..."))
  src_header <- paste0(
    "http://www.fec.gov/finance/disclosure/metadata/", 
    gsub("\\.zip", "_header_file.csv", basename(filename))
  ) %>%
#    https://github.com/beanumber/fec/issues/9
    gsub("1[0-9]", "", x = .)
    
  
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
#'   db <- src_mysql(default.file = path.expand("~/.my.cnf"), groups = "rs-dbi",
#'                   user = NULL, password = NULL, dbname = "fec")
#' }
#' 
#' fec <- etl("fec", db, dir = "~/dumps/fec")
#' fec %>%
#'   etl_extract() %>%
#'   etl_transform() %>%
#'   etl_init() %>%
#'   etl_load()
#' }
etl_load.etl_fec <- function(obj, years = 2012, ...) {
  
  src <- lapply(years, get_filenames) %>%
    unlist()
  src <- paste0(attr(obj, "load_dir"), "/", basename(src)) # gsub to replace .zip with .csv??
  
  lcl <- list.files(attr(obj, "load_dir"), full.names = TRUE)
  tablenames <- c("committees", "candidates", "house_elections", "individuals", "contributions")
  
  # write the table directly to the DB
  message("Writing FEC data to the database...")
  mapply(DBI::dbWriteTable, name = tablenames, value = lcl, 
         MoreArgs = list(conn = obj$con, append = TRUE, ... = ...))

  invisible(obj)
}

