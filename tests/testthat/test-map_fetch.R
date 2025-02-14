context("map_fetch")

test_that("map_fetch - png", {
  skip_on_cran()
  # skip_on_ci()
  skip_if_not_installed("png")
  skip_if_not_installed("raster")
  skip_if_not_installed("sf")

  x <- map_fetch(taxonKey = 3118771, year = 2010)
  expect_is(x, "RasterLayer")
})

test_that("map_fetch fails well", {
  skip_on_cran()
  skip_on_ci()
  skip_if_not_installed("png")
  skip_if_not_installed("raster")
  skip_if_not_installed("sf")
  
  expect_error(map_fetch(source = "stuff"), "is not TRUE")
  expect_error(map_fetch(source = 5), "source must be of class character")
  expect_error(map_fetch(format = 'bears'), "is not TRUE")

  # search and id have been removed
  expect_error(map_fetch(search = 'bears'), "have been removed")
  expect_error(map_fetch(id = 3), "have been removed")
})
