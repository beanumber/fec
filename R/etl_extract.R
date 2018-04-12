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
#' @source \url{https://transition.fec.gov/pubrec/electionresults.shtml}
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
etl_extract.etl_fec <- function(obj, years = 2014, ...) {
  
  src <- lapply(years, get_filenames) %>%
    unlist()
  
  # election results
  # https://transition.fec.gov/pubrec/electionresults.shtml
  # https://transition.fec.gov/pubrec/fe2016/federalelections2016.xlsx
  # .xlsx after 2014
  file_ext <- ifelse(years > 2014, ".xlsx", ".xls")
  src <- append(src, 
                paste0("https://transition.fec.gov/pubrec/fe", years, 
                       "/federalelections", years, file_ext))
  
  etl::smart_download(obj, src)
  invisible(obj)
}


get_filenames <- function(year) {
  valid_years <- seq(from = 1982, to = 2018, by = 2)
  year <- intersect(year, valid_years)
  gen_files <- c("ccl", "cn", "cm", "oppexp", "oth", "pas2", "indiv")
  if (length(year) > 0) {
    year_end <- substr(year, 3, 4)
    base_url <- "https://cg-519a459a-0ea3-42c2-b7bc-fa1143481f74.s3-us-gov-west-1.amazonaws.com/bulk-downloads/"
    return(paste0(base_url, year, "/", gen_files, year_end, ".zip"))
  } else {
    return(NULL)
  }
}

