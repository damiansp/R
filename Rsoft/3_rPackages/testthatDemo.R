#---------#---------#---------#---------#---------#---------#---------#---------
rm(list=ls())
library(testthat)

expect_that(sqrt(3) * sqrt(3), equals(3))
expect_that(sqrt(3) * sqrt(3), is_identical_to(3))

# Common functions
# equals()
# is_identical_to()
# is_equivalent_to()
# is_a()
# matches()
# prints_text()
# shows_message()
# gives_warning()
# throws_error()
# is_true()

test_that(
  'model fitting', {
    data(airquality)
    fit <- lm(Ozone ~ Wind, data=airquality)
    expect_that(fit, is_a('lm'))
    expect_that(1 + 1, equals(2))
  })
  