test_that("readJSONFromUrl works", {
  tmp <- readJSONFromUrl("https://api.kolada.se/v2/kpi_groups?title=aaa")
  expect_equal(tmp$count,0)
})

test_that("is_valid_year works", {
  expect_equal(is_valid_year("1999"),TRUE)
  expect_equal(is_valid_year("1999,2000"),TRUE)
  expect_equal(is_valid_year("199"),FALSE)
  expect_equal(is_valid_year("199X"),FALSE)
})

test_that("kolada_municipality_kpi_groups() works", {
  expect_error(kolada_municipality_kpi_groups(search_title = 1))
  
  tmp <- kolada_municipality_kpi_groups(search_title = "a")
  expect_equal(tmp$count,81)
  tmp <- kolada_municipality_kpi_groups(search_title = "aaa")
  expect_equal(tmp$count,0)
})

test_that("kolada_municipality() works", {
  expect_error(kolada_municipality(search_title = 1))
  tmp <- kolada_municipality(search_title = "a")
  expect_equal(tmp$count,161)
  tmp <- kolada_municipality(search_title = "aaaaa")
  expect_equal(tmp$count,0)
})

test_that("kolada_municipality_kpi() works", {
  expect_error(kolada_municipality_kpi(kpi = 1, municipality = "", year=""))
  expect_error(kolada_municipality_kpi(kpi = "", municipality = 1, year=""))
  expect_error(kolada_municipality_kpi(kpi = "", municipality = "", year=1))
  
  tmp <- kolada_municipality_kpi(kpi ="N00914", municipality = "1283", year="2000")
  expect_equal(tmp$count,1)
})