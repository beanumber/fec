# house_elections_2012

db <- src_mysql_cnf(dbname = "fec", host = "127.0.0.1")
fec <- etl("fec", db = db, dir = "~/dumps/fec")
house_elections_2012 <- fec %>% 
  tbl("house_elections") %>% 
  collect()

save(house_elections_2012, file = "data/house_elections_2012", compress = "xz")