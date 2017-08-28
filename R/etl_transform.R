if(getRversion() >= "2.15.1")  utils::globalVariables(".")

#' @rdname etl_extract.etl_fec
#' @importFrom readr read_delim write_csv parse_number
#' @importFrom readxl read_excel
#' @importFrom utils head
#' @import dplyr
#' @export
etl_transform.etl_fec <- function(obj, years = 2014, ...) {
  
  src <- lapply(years, get_filenames) %>%
    unlist() %>%
    basename() %>%
    file.path(attr(obj, "raw_dir"), .)
  
  lapply(src, smart_transform, obj = obj)
  
  # election results
  src <- file.path(attr(obj, "raw_dir"), paste0("federalelections", years, ".xls"))
  
  #try catch here - if there is an excel file, return the excel file. If there is no file, return invisible(obj)
  if (file.size(src) < 100000) {
    warning("No valid election results found")
    return(invisible(obj))
  }

  # https://github.com/beanumber/fec/issues/11
  sheets <- readxl::excel_sheets(src)
  house_sheet <- utils::head(grep("House.+Res", x = sheets), 1)
  elections <- readxl::read_excel(src, sheet = house_sheet)
  names(elections) <- names(elections) %>%
    tolower() %>%
    gsub(" ", "_", x = .) %>%
    gsub("#", "", x = .) %>%
    gsub("%", "pct", x = .)
  house_elections <- elections %>%
    dplyr::filter_(~fec_id != "n/a", ~d != "S") %>%
    dplyr::rename_(district = ~d, incumbent = ~`(i)`) %>%
    dplyr::select_(~state_abbreviation, ~district, ~fec_id, ~incumbent, 
                   ~candidate_name, ~party, ~primary_votes, ~runoff_votes, 
                   ~general_votes, ~ge_winner_indicator) %>%
    dplyr::mutate_(primary_votes = ~readr::parse_number(primary_votes),
                   general_votes = ~readr::parse_number(general_votes),
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
  readr::write_csv(house_elections, paste0(attr(obj, "load_dir"), "/house_elections_", years, ".csv"))
  return(invisible(obj))
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
  # add new column for election cycle
  data <- data %>%
    mutate(election_cycle = readr::parse_number(filename) + 2000)
  
#  data <- read.delim(filename, col.names = header, sep = "|")
  
  lcl <- file.path(attr(obj, "load_dir"), gsub("\\.zip", "\\.csv", basename(filename)))
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
      grepl("cm", path) ~ "committees",
      grepl("cn", path) ~ "candidates",
      grepl("house", path) ~ "house_elections",
      grepl("indiv", path) ~ "individuals",
      TRUE ~ "contributions"
    ))
  
  # write the table directly to the DB
  message("Writing FEC data to the database...")
  mapply(DBI::dbWriteTable, name = lcl$table, value = lcl$path, 
         MoreArgs = list(conn = obj$con, append = TRUE, ... = ...))

  invisible(obj)
}
