context("fec")

## TODO: Rename context
## TODO: Add more tests

test_that("mysql works", {
  if (require(RMySQL) & mysqlHasDefault()) {
    db <- src_mysql(default.file = "~/.my.cnf",
                    groups = "rs-dbi", dbname = "test",
                    user = NULL, password = NULL)
    test_dir <- "~/dumps/fec"
    if (dir.exists(test_dir)) {
      fec <- etl("fec", db = db, dir = test_dir)
      expect_s3_class(fec, c("etl_fec", "etl", "src_mysql", "src_sql"))
      fec %>% etl_update()
      expect_equal(fec %>% tbl("house_elections") %>% collect() %>% nrow(), 2178)
    }
  }
})

#   system("mysql -e 'CREATE DATABASE IF NOT EXISTS fec'")
#   db <- src_mysql(user = NULL, host = "localhost", 
#                      dbname = "fec", password = NULL, default.file = "~/.my.cnf")
#   fec <- etl("fec", db = db, dir = "~/dumps/fec")
#   fec %>%
#     etl_create()
#   expect_equal(fec %>% tbl("individuals") %>% nrow(), 3349043)
#   expect_equal(fec %>% tbl("committees") %>% nrow(), 14454)  
#   expect_equal(fec %>% tbl("candidates") %>% nrow(), 5628)
# })
# 
# test_that("postgres works", {
#   db <- src_postgres(
#     user = "postgres", 
#     host = "localhost", 
#     dbname = "fec", 
#     password = "postgres", 
#     port = 5434
#   )
#   fec <- etl("fec", db = db, dir = "~/dumps/fec")
#   fec %>%
#     etl_create()
#   expect_equal(fec %>% tbl("individuals") %>% nrow(), 3349043)
# })
