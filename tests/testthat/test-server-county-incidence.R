test_that("multiplication works", {
  expect_equal(2 * 2, 4)
})

# https://shiny.posit.co/r/articles/improve/server-function-testing/
# https://r-pkgs.org/testing-advanced.html#test-fixtures
# https://mastering-shiny.org/scaling-testing.html

source("./inst/get-data.R")

skip_if_no_data <- function(sample_data) {
  if(nrow(sample_data) == 0){
    skip("No sample data")
  }
}

# Test that map shows up with baseline inputs
testServer("Baseline input works", server_county_incidence,
           args = list("incidence", "get_data", "sample_wa_county_incidence",
                       "WA", "Age_Adjusted_Incidence_Rate", "County"), {

  sample_data <- input_data()

  # If the provided "get data" fn and input table name brings up an empty df, then
  ## print out a warning (not necessarily an error). Then skip rest of tests
  skip_if_no_data(sample_data)

  session$setInputs(cancer_type = "all cancer sites",
                    race = "All Races (includes Hispanic)", sex = "both sexes",
                    age = "all ages", stage = "all stages",
                    year = "latest 5 year average")

  # Check that a map is generated and that the output map message is NULL
  expect_failure(expect_equal(output$choropleth, NULL))
  expect_equal(output$map_message, renderUI({NULL}))

})



testServer(server_county_incidence, args = list("incidence", "get_data",
                                                "sample_wa_county_incidence",
                                                "WA",
                                                "Age_Adjusted_Incidence_Rate",
                                                "County") {

  source("./inst/get-data.R")
  sample_data <- get_data("sample_wa_county_incidence")

  # If the provided "get data" fn and input table name brings up an empty df, then
  ## print out a warning (not necessarily an error). Then skip rest of tests
  if(nrow(sample_data) == 0){
    ## do something
  }

  session$setInputs(cancer_type = "all cancer sites")
  session$setInputs(race = "All Races (includes Hispanic)")
  session$setInputs(sex = "both sexes")
  session$setInputs(age = "all ages")
  session$setInputs(stage = "all stages")
  session$setInputs(year = "latest 5 year average")
  # Check that a map is generated and that the output map message is NULL

  expect_equal(output$map_message, renderUI({NULL}))
  print(str(output$plot))

  # If female cancer, then map should show up only for females
  ## breast 1, breast 2, cervix, uterus
  session$setInputs(cancer_type = "breast (female)")

  # If male cancer, then map should show up only for males
  ## prostate
  session$setInputs(cancer_type = "prostate")

  # If child cancer, then map should show up only for children
  ## children <15
  session$setInputs(cancer_type = "childhood (ages <15, all sites)")

  # If filters are too stringent, then no map shows up and a message appears
})


testServer(server_county_incidence, args = list("incidence", "get_data",
                                                "sample_wa_county_incidence",
                                                "WA",
                                                "Age_Adjusted_Incidence_Rate",
                                                "County") {

  source("./inst/get-data.R")
  sample_data <- get_data("sample_wa_county_incidence")

  # If the provided "get data" fn and input table name brings up an empty df, then
  ## print out a warning (not necessarily an error). Then skip rest of tests
  if(nrow(sample_data) == 0){
    ## do something
  }

  session$setInputs(cancer_type = "all cancer sites")
  session$setInputs(race = "All Races (includes Hispanic)")
  session$setInputs(sex = "both sexes")
  session$setInputs(age = "all ages")
  session$setInputs(stage = "all stages")
  session$setInputs(year = "latest 5 year average")
  # Check that a map is generated and that the output map message is NULL

  expect_equal(output$map_message, renderUI({NULL}))
  print(str(output$plot))

  # If female cancer, then map should show up only for females
  ## breast 1, breast 2, cervix, uterus
  session$setInputs(cancer_type = "breast (female)")

  # If male cancer, then map should show up only for males
  ## prostate
  session$setInputs(cancer_type = "prostate")

  # If child cancer, then map should show up only for children
  ## children <15
  session$setInputs(cancer_type = "childhood (ages <15, all sites)")

  # If filters are too stringent, then no map shows up and a message appears
})


testServer(server_county_incidence, args = list("incidence", "get_data",
                                                "sample_wa_county_incidence",
                                                "WA",
                                                "Age_Adjusted_Incidence_Rate",
                                                "County") {

  source("./inst/get-data.R")
  sample_data <- get_data("sample_wa_county_incidence")

  # If the provided "get data" fn and input table name brings up an empty df, then
  ## print out a warning (not necessarily an error). Then skip rest of tests
  if(nrow(sample_data) == 0){
    ## do something
  }

  session$setInputs(cancer_type = "all cancer sites")
  session$setInputs(race = "All Races (includes Hispanic)")
  session$setInputs(sex = "both sexes")
  session$setInputs(age = "all ages")
  session$setInputs(stage = "all stages")
  session$setInputs(year = "latest 5 year average")
  # Check that a map is generated and that the output map message is NULL

  expect_equal(output$map_message, renderUI({NULL}))
  print(str(output$plot))

  # If female cancer, then map should show up only for females
  ## breast 1, breast 2, cervix, uterus
  session$setInputs(cancer_type = "breast (female)")

  # If male cancer, then map should show up only for males
  ## prostate
  session$setInputs(cancer_type = "prostate")

  # If child cancer, then map should show up only for children
  ## children <15
  session$setInputs(cancer_type = "childhood (ages <15, all sites)")

  # If filters are too stringent, then no map shows up and a message appears
})










