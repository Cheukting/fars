
library(testthat)
library(fars)

context("test functions")

filename <- make_filename("2013")
test_that("test make_filename",{
          expect_that(filename,is_a("character"))
})

