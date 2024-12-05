# This function specifies the server logic for the county incidence tab of the
# app.

# Test that map shows up with baseline inputs
test_that("Baseline input works", {
  shiny::testServer(
    server_county_incidence,
    args = list(id = "incidence", test_callback("sample-data.tsv"),
                "WA", "Age_Adjusted_Incidence_Rate", "County"), {

      session$setInputs(
        cancer_type = oncarto:::cancer_types[1],
        race = oncarto:::races[1],
        sex = oncarto:::sexes[1],
        age = oncarto:::ages[1],
        stage = oncarto:::stages[1],
        year = oncarto:::years[1]
      )

      # Check that a map is generated
      expect_true(!is.null(output$choropleth))
  })
})




# Test that map shows up with female-specific cancer
test_that("Female-specific cancer works", {
  shiny::testServer(
    server_county_incidence,
    args = list(id = "incidence", test_callback("sample-data.tsv"),
                "WA", "Age_Adjusted_Incidence_Rate", "County"), {

      session$setInputs(
        cancer_type = "breast (female)",
        race = oncarto:::races[1],
        sex = oncarto:::sexes[1],
        age = oncarto:::ages[1],
        stage = oncarto:::stages[1],
        year = oncarto:::years[1]
      )

      expect_true(!is.null(output$map_message))

      session$setInputs(
        cancer_type = "breast (female)",
        race = oncarto:::races[1],
        sex = "females",
        age = oncarto:::ages[1],
        stage = oncarto:::stages[1],
        year = oncarto:::years[1]
      )

      # Check that a map is generated
      expect_true(!is.null(output$choropleth))
  })
})


# Test that map shows up with male-specific cancer
test_that("Male-specific cancer works", {
  shiny::testServer(
    server_county_incidence,
    args = list(id = "incidence", test_callback("sample-data.tsv"),
                "WA", "Age_Adjusted_Incidence_Rate", "County"), {

      session$setInputs(
        cancer_type = "prostate",
        race = oncarto:::races[1],
        sex = oncarto:::sexes[1],
        age = oncarto:::ages[1],
        stage = oncarto:::stages[1],
        year = oncarto:::years[1]
      )

      expect_true(!is.null(output$map_message))

      session$setInputs(
        cancer_type = "prostate",
        race = oncarto:::races[1],
        sex = "males",
        age = oncarto:::ages[1],
        stage = oncarto:::stages[1],
        year = oncarto:::years[1]
      )

      # Check that a map is generated
      expect_true(!is.null(output$choropleth))
    })
})

# Test that map shows up with age-specific cancer
test_that("Age-specific cancer works", {
  shiny::testServer(
    server_county_incidence,
    args = list(id = "incidence", test_callback("sample-data.tsv"),
                "WA", "Age_Adjusted_Incidence_Rate", "County"), {

      session$setInputs(
        cancer_type = "childhood (ages <15, all sites)",
        race = oncarto:::races[1],
        sex = oncarto:::sexes[1],
        age = oncarto:::ages[1],
        stage = oncarto:::stages[1],
        year = oncarto:::years[1]
      )

      expect_true(!is.null(output$map_message))

      session$setInputs(
        cancer_type = "childhood (ages <15, all sites)",
        race = oncarto:::races[1],
        sex = oncarto:::sexes[1],
        age = "ages <15",
        stage = oncarto:::stages[1],
        year = oncarto:::years[1]
      )

      # Check that a map is generated and that the output map message is NULL
      expect_true(!is.null(output$choropleth))
    })
})

