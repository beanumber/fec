# Load data
# 

library(fec)

db <- src_mysql_cnf(dbname = "fec")
fec <- etl("fec", db = db, dir = "~/dumps/fec")

fec %>%
  etl_init()

fec %>%
  etl_extract(years = 2016) %>%
  etl_transform(year = 2016) %>%
  etl_load(years = 2016)

