library(tidyverse)
library(rvest)

url <- "https://classic.fec.gov/finance/disclosure/ftpdet.shtml"

docs <- url %>%
  read_html() %>%
  html_nodes("foia_files")

url_base <- "https://classic.fec.gov/finance/disclosure/metadata/DataDictionary"

pages <- paste0(url_base, 
                c("ContributionstoCandidates.shtml",
                  "CommitteeMaster.shtml", 
                  "CandCmteLinkage.shtml",
                  "CommitteetoCommittee.shtml",
                  "ContributionsbyIndividuals.shtml",
                  "OperatingExpenditures.shtml"
))

get_vars <- function(url) {
  vars <- url %>%
    read_html() %>%
    html_nodes("table") %>%
    html_table(header = TRUE) %>%
    magrittr::extract2(1) %>%
    rename(var_name = `Column Name`,
           data_type = `Data Type`,
           allows_null = `Null?`) %>%
    mutate(table = basename(url), 
           table = gsub("DataDictionary", "", table),
           table = gsub(".shtml", "", table),
           var_name = tolower(var_name),
           precision = readr::parse_number(data_type),
           data_type = gsub("VARCHAR2", "VARCHAR", data_type),
           data_type = ifelse(grepl(" or VARCHAR\\(18\\)", data_type),
                              "VARCHAR(18)", data_type),
           data_type = ifelse(grepl("NUMBER \\([0-9]+\\,[0-9]+\\)", data_type),
                              gsub("NUMBER", "DOUBLE", data_type), data_type),
           data_type = ifelse(grepl("N(UMBER|umber) *\\([0-9]+\\)", data_type),
                              gsub("N(UMBER|umber)", "INT", data_type), data_type),
           data_type = trimws(data_type),
           data_type = ifelse(data_type == "", "TEXT", data_type),
           data_type = ifelse(data_type == "INT" & precision < 5, "SMALLINT", data_type),
           sql = paste0(data_type, 
                        ifelse(allows_null == "Y", "", " NOT NULL"))
           )
  return(vars)
}

fec_vars <- lapply(pages, get_vars) %>%
  bind_rows() %>%
  as_tibble()

save(fec_vars, file = "data/fec_vars.rda", compress = "xz")

library(DBI)

get_sql <- function(data) {
  fields <- data$sql
  names(fields) <- data$var_name
  tbl_name <- data$table[1]
  list(
    paste("/* automatic table generation for", tbl_name, "*/"), 
    paste("DROP TABLE IF EXISTS", tbl_name),
    sqlCreateTable(ANSI(), table = tbl_name, fields = fields)
  )
}

sql <- fec_vars %>%
  group_by(table) %>%
  do(sqls = get_sql(.)) %>%
  pull(sqls) %>%
  lapply(unlist) %>%
  unlist() %>%
  paste0(";\n") %>%
  unlist() %>%
  gsub('"', '`', x = .)

cat(sql, file = "inst/sql/init.sql")
