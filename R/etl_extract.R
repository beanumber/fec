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
etl_extract.etl_fec <- function(obj, years = 2014, ...) {
  
  src <- lapply(years, get_filenames) %>%
    unlist()
  
  # election results
  src <- append(src, 
                paste0("http://www.fec.gov/pubrec/fe", years, 
                       "/federalelections", years, ".xls"))
  
  etl::smart_download(obj, src)
  invisible(obj)
}


get_filenames <- function(year) {
  valid_years <- seq(from = 1982, to = 2018, by = 2)
  year <- intersect(year, valid_years)
  gen_files <- c("cn", "cm", "pas2", "indiv")
  if (length(year) > 0) {
    year_end <- substr(year, 3, 4)
    return(paste0("ftp://ftp.fec.gov/FEC/", year, "/", gen_files, year_end, ".zip"))
  } else {
    return(NULL)
  }
}

