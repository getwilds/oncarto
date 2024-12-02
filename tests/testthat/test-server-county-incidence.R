test_that("multiplication works", {
  expect_equal(2 * 2, 4)
})

# https://shiny.posit.co/r/articles/improve/server-function-testing/
# https://r-pkgs.org/testing-advanced.html#test-fixtures
# https://mastering-shiny.org/scaling-testing.html

test_that("server-county-incidence works", {

})


# If the provided "get data" fn and input table name brings up an empty df, then
## print out a warning (not necessarily an error). Then skip rest of tests

# If female cancer, then map should show up only for females
## breast 1, breast 2, cervix, uterus

# If male cancer, then map should show up only for males
## prostate

# If child cancer, then map should show up only for children
## children <15, children <20

# If filters are too stringent, then no map shows up and a message appears

