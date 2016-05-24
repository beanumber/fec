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
  
  lcl <- paste0(attr(obj, "raw_dir"), "/", src_files)
  missing <- !file.exists(lcl)
  
  mapply(FUN = download.file, src[missing], lcl[missing])
  invisible(obj)
}

#' @rdname etl_extract.etl_fec
#' @importFrom readr read_delim write_csv
#' @export
etl_transform.etl_fec <- function(obj, years = 2012, ...) {
  
  src <- paste0(attr(obj, "raw_dir"), "/", 
                      c("cn12.zip", "cm12.zip", "pas212.zip", "indiv12.zip"))
  headers <- paste0("http://www.fec.gov/finance/disclosure/metadata/", 
                         c("cn_header_file.csv", "cm_header_file.csv", 
                           "pas2_header_file.csv", "indiv_header_file.csv"))
  individuals <- readr::read_delim(src[4], delim = "|", col_names = names(readr::read_csv(headers[4])))
  committees <- readr::read_delim(src[2], delim = "|", col_names = names(readr::read_csv(headers[2])))
  candidates <- readr::read_delim(src[1], delim = "|", col_names = names(readr::read_csv(headers[1])))
  contributions <- readr::read_delim(src[3], delim = "|", col_names = names(readr::read_csv(headers[3])))
  
  readr::write_csv(individuals, paste0(attr(obj, "load_dir"), "/individuals.csv"))
  readr::write_csv(committees, paste0(attr(obj, "load_dir"), "/committees.csv"))
  readr::write_csv(candidates, paste0(attr(obj, "load_dir"), "/candidates.csv"))
  readr::write_csv(contributions, paste0(attr(obj, "load_dir"), "/contributions.csv"))
  
  invisible(obj)
}

#' @rdname etl_extract.etl_fec
#' @importFrom DBI dbWriteTable dbListTables
#' @export
etl_load.etl_fec <- function(obj, schema = FALSE, years = 2012, ...) {
  # write the table directly to the DB
  message("Writing FEC data to the database...")
  DBI::dbWriteTable(obj$con, "contributions", paste0(attr(obj, "load_dir"), "/contributions.csv"), 
                    overwrite = TRUE, ...)
  if (DBI::dbWriteTable(obj$con, "candidates", paste0(attr(obj, "load_dir"), "/candidates.csv"), 
                        overwrite = TRUE, ...)) {
    message("Data was successfully written to database.")
    message(DBI::dbListTables(obj$con))
    invisible(obj)
  }
}


