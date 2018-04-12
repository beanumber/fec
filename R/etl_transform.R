if(getRversion() >= "2.15.1")  utils::globalVariables(".")

#' @rdname etl_extract.etl_fec
#' @importFrom readr read_delim write_csv parse_number
#' @import dplyr
#' @export
etl_transform.etl_fec <- function(obj, years = 2014, ...) {
  
  src <- lapply(years, get_filenames) %>%
    unlist() %>%
    basename() %>%
    file.path(attr(obj, "raw_dir"), .)
  
  lapply(src, smart_transform, obj = obj)
  
  # election results
  valid <- etl::valid_year_month(years, months = 1)
  available <- data_frame(path = list.files(attr(obj, "raw_dir"), pattern = "election", full.names = TRUE)) %>%
    mutate(year = as.integer(readr::parse_number(basename(path))))
  src <- valid %>%
    inner_join(available, by = "year") %>%
    pull(path)
  
  #try catch here - if there is an excel file, return the excel file. If there is no file, return invisible(obj)
  if (file.size(src) < 100000) {
    warning("No valid election results found")
    return(invisible(obj))
  }
  
  house_elections <- sapply(src, transform_elections)
  
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

#' @importFrom readxl read_excel
#' @importFrom utils head
#' @import dplyr

transform_elections <- function(path) {
  # https://github.com/beanumber/fec/issues/11
  sheets <- readxl::excel_sheets(path)
  house_sheet <- utils::head(grep("House.+Res", x = sheets), 1)
  elections <- readxl::read_excel(path, sheet = house_sheet)
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
  
  year <- readr::parse_number(basename(path))
  out_path <- file.path(gsub("raw", "load", dirname(path)), paste0("house_elections_", year, ".csv"))
  readr::write_csv(house_elections, out_path)
}
