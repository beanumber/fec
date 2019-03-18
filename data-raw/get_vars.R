library(tidyverse)
library(rvest)

url <- "https://classic.fec.gov/finance/disclosure/ftpdet.shtml"

docs <- url %>%
  read_html() %>%
  html_nodes("foia_files")

url_base <- "https://classic.fec.gov/finance/disclosure/metadata/DataDictionary"

pages <- paste0(url_base, 
                c("ContributionstoCandidates.shtml",
                  "CandidateMaster.shtml",
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
           data_type = gsub("VVARCHAR", "VARCHAR", data_type),
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
                        ifelse(allows_null == "Y", "", " NOT NULL")),
           table = case_when(
             grepl("CandCmteLinkage", table) ~ "cand_com_link",
             grepl("CommitteeMaster", table) ~ "committees",
             grepl("CandidateMaster", table) ~ "candidates",
             grepl("ContributionsbyIndividuals", table) ~ "contrib_indiv_to_com",
             grepl("CommitteetoCommittee", table) ~ "contrib_com_to_com",
             grepl("ContributionstoCandidates", table) ~ "contrib_com_to_cand",
             grepl("OperatingExpenditures", table) ~ "expenditures",
             TRUE ~ "null"
           ))
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
  unlist()


sql <- sql %>%
  append("DROP TABLE IF EXISTS `house_elections`;\n") %>%
  append(sqlCreateTable(ANSI(), "house_elections", fields = c(
    cand_id = "varchar(9) NOT NULL default ''",
    state = "varchar(2) NOT NULL default ''",
    district = "varchar(25) NOT NULL default ''",
    incumbent = "varchar(5) NOT NULL default ''",
    candidate_name = "varchar(255) NOT NULL default ''",
    party = "varchar(1) NOT NULL default ''",
    primary_votes = "int NOT NULL default 0",
    runoff_votes = "int NOT NULL default 0",
    general_votes = "int NOT NULL default 0",
    ge_winner = "varchar(1) NOT NULL default ''",
    election_cycle = "smallint(4) DEFAULT NULL"
  ), row.names = FALSE)) %>%
  append(";") %>%
  gsub('"', '`', x = .)


cat(sql, file = "inst/sql/init.sql")
