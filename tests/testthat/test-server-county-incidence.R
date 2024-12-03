# This function specifies the server logic for the county incidence tab of the
# app.
#
#' @importFrom shiny testServer

test_that("multiplication works", {
  expect_equal(2 * 2, 4)
})

source("get-data.R")

# https://shiny.posit.co/r/articles/improve/server-function-testing/
# https://r-pkgs.org/testing-advanced.html#test-fixtures
# https://mastering-shiny.org/scaling-testing.html


# Test that map shows up with baseline inputs
shiny::testServer(server_county_incidence,
           args = list(id = "incidence", "get_data", "sample_wa_county_incidence",
                       "WA", "Age_Adjusted_Incidence_Rate", "County"), {

  # Check that a map is generated and that the output map message is NULL
  expect_failure(expect_equal(output$choropleth, NULL))
  expect_equal(output$map_message, NULL)
})

# Test that map shows up with female-specific cancer
shiny::testServer(server_county_incidence,
           args = list(id = "incidence", "get_data", "sample_wa_county_incidence",
                       "WA", "Age_Adjusted_Incidence_Rate", "County"), {

  session$setInputs(cancer_type = "breast (female)", sex = "both sexes")

  # Check that a message is generated and that the map is NULL
  expect_failure(expect_equal(output$map_message, NULL))
  expect_equal(output$choropleth, NULL)

  session$setInputs(sex = "females")

  # Check that a map is generated and that the message is NULL
  expect_failure(expect_equal(output$choropleth, NULL))
  expect_equal(output$map_message, NULL)
 })

# Test that map shows up with male-specific cancer
shiny::testServer(server_county_incidence,
           args = list(id = "incidence", "get_data", "sample_wa_county_incidence",
                       "WA", "Age_Adjusted_Incidence_Rate", "County"), {

  session$setInputs(cancer_type = "prostate", sex = "both sexes")

  # Check that a message is generated and that the map is NULL
  expect_failure(expect_equal(output$map_message, NULL))
  expect_equal(output$choropleth, NULL)

  session$setInputs(sex = "males")

  # Check that a map is generated and that the message is NULL
  expect_failure(expect_equal(output$choropleth, NULL))
  expect_equal(output$map_message, NULL)
})

# Test that map shows up with the right age-specific input
shiny::testServer(server_county_incidence,
           args = list(id = "incidence", "get_data", "sample_wa_county_incidence",
                       "WA", "Age_Adjusted_Incidence_Rate", "County"), {

  session$setInputs(cancer_type = "childhood (ages <15, all sites)",
                    sex = "both sexes")

  # Check that the message appears and the map is NULL
  expect_failure(expect_equal(output$map_message, NULL))
  expect_equal(output$choropleth, NULL)

  session$setInputs(age = "ages <15")

  # Check that a map is generated and that the message is NULL
  expect_failure(expect_equal(output$choropleth, NULL))
  expect_equal(output$map_message, NULL)
})

