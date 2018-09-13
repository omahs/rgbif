context("throttling: HttpStore internal R6 class")
test_that("HttpStore internal R6 class", {
  expect_is(HttpStore, "R6ClassGenerator")
  expect_is(HttpStore$new(), "HttpStore")

  # before put used
  x <- HttpStore$new()
  expect_type(x$count(), "integer")
  expect_equal(x$count(), 0)
  expect_identical(x$requests, list())
  expect_equal(x$put("Thu, 13 Sep 2018 02:51:08 GMT"), x$requests)
  expect_is(x$requests, "list")

  # wait: no wait here
  expect_lt(system.time(x$wait())[[3]], 1)

  # wait: wait should happen
  x$put(format(Sys.time(), format = "%d %B %Y %H:%M:%S", tz="UTC"))
  expect_gt(system.time(x$wait())[[3]], 0.01)
})

context("throttling: package functions")
test_that("package functions", {
  skip_on_cran()

  keys <- name_lookup(rank = "species")$data$key[1:4]
  expect_gt( 
    system.time(for (i in seq_along(keys)) occ_count(taxonKey = keys[i]))[[3]],
    3
  )
})

context("throttling: failure behavior")
test_that("failure behavior", {
  skip_on_cran()

  x <- HttpStore$new()
  suppressWarnings(x$put(45))
  suppressWarnings(x$put('asdfadfafasdfdffsdf'))
  expect_true(is.na(x$requests[[1]]))
  expect_true(is.na(x$requests[[2]]))

  # runs smoothly 
  expect_null(x$wait())
})

# x$put(format(Sys.time(), format = "%d %B %Y %H:%M:%S", tz="UTC"))
# system.time(x$wait())[[3]]

