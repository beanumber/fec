context("fec")

## TODO: Rename context
## TODO: Add more tests

test_that("mysql works", {
  db <- src_mysql(user = NULL, host = "localhost", 
                     dbname = "fec", password = NULL, default.file = "~/.my.cnf")
  fec <- etl("fec", db = db, dir = "~/dumps/fec")
  fec %>%
    etl_create()
  expect_equal(fec %>% tbl("individuals") %>% nrow(), 3349043)
  expect_equal(fec %>% tbl("committees") %>% nrow(), 14454)  
  expect_equal(fec %>% tbl("candidates") %>% nrow(), 5628)
})

test_that("postgres works", {
  db <- src_postgres(
    user = "postgres", 
    host = "localhost", 
    dbname = "fec", 
    password = "postgres", 
    port = 5434
  )
  fec <- etl("fec", db = db, dir = "~/dumps/fec")
  fec %>%
    etl_create()
  expect_equal(fec %>% tbl("individuals") %>% nrow(), 3349043)
})
