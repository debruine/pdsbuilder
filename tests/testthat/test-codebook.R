test_that("defaults", {
  data <- faux::sim_design(seed = 8675309)

  cb <- codebook(data)

  compare <- '{
    "@context": "https://schema.org/",
    "@type": "Dataset",
    "name": "data",
    "schemaVersion": "Psych-DS 0.1.0",
    "variableMeasured": [
        {
            "@type": "PropertyValue",
            "name": "id",
            "description": "id",
            "type": "string"
        },
        {
            "@type": "PropertyValue",
            "name": "y",
            "description": "value",
            "type": "float"
        }
    ]
}
'
  class(compare) <- "json"

  expect_equal(cb, compare)
})

test_that("warnings", {
  data <- faux::sim_design(seed = 8675309)
  # all valid properties
  vardesc <- list("description" = c(id = "Subject ID"),
                  "privacy" = c(T, F),
                  "type" = c("string", "float"),
                  "propertyID" = c(id = "ID7"),
                  "minValue" = c(y = -100),
                  "maxValue" = c(y = 100),
                  "levels" = list(id = LETTERS),
                  "ordered" = c(id = TRUE),
                  "na" = c(id = "NA"),
                  "naValues" = c(id = "NOPE"),
                  "alternateName" = c(id = "part_id"),
                  "unitCode" = c(id = "huh?"))
  expect_silent(cb <- codebook(data, "data", vardesc, return = "list"))

  expect_equal(cb$variableMeasured[[1]],
               list(`@type` = "PropertyValue",
                    name = "id",
                    description ="Subject ID",
                    privacy =TRUE,
                    type ="string",
                    propertyID = "ID7",
                    levels = as.list(LETTERS),
                    ordered = TRUE,
                    na = "NA",
                    naValues = "NOPE",
                    alternateName = "part_id",
                    unitCode = "huh?"))

  expect_equal(cb$variableMeasured[[2]],
               list(`@type` = "PropertyValue",
                    name = "y",
                    description= "value",
                    privacy = FALSE,
                    type = "float",
                    minValue = -100,
                    maxValue = 100))

  # add an invalid property
  vardesc$invalid <- c(id = "STOP")
  expect_warning(cb <- codebook(data, "data", vardesc, return = "list"),
                 "The following variable properties are not standard: invalid", fixed = TRUE)
})

test_that("no vardesc", {
  data <- faux::sim_design(2, 2, seed = 8675309, plot = FALSE)
  cb <- codebook(data, return = "list")

  vm <- cb$variableMeasured

  expect_equal(cb[["@type"]], "Dataset")
  expect_equal(cb[["schemaVersion"]], "Psych-DS 0.1.0")
  expect_equal(length(vm), 4)

  expect_equal(vm[[1]], list(`@type` = "PropertyValue",
                             name = "id",
                             description = "id",
                             type = "string"))

  expect_equal(vm[[2]], list(`@type` = "PropertyValue",
                             name = "B",
                             description = "B",
                             levels = list(B1 = "B1", B2 = "B2"),
                             type = "string",
                             ordered = FALSE))

  expect_equal(vm[[3]], list(`@type` = "PropertyValue",
                             name = "A1",
                             description = "A1",
                             type = "float"))

  expect_equal(vm[[4]], list(`@type` = "PropertyValue",
                             name = "A2",
                             description = "A2",
                             type = "float"))
})

test_that("named factor levels", {
  between <- list(
    pet = c(cat = "Has a cat", dog = "Has a dog")
  )
  data <- faux::sim_design(between = between,
                     dv = list(y = "Happiness Score"),
                     id = list(id = "Subject ID"),
                     plot = FALSE)
  expect_message(cb <- codebook(data), "id set to type string")
  expect_message(cb <- codebook(data), "pet set to type string")
  expect_message(cb <- codebook(data, return = "list"), "y set to type float")

  output <- capture_output(print(cb))
  expect_equal(output, "Codebook for data (Psych-DS 0.1.0)\n========================================\n\n===== Dataset Parameters =====\n* name: data\n* schemaVersion: Psych-DS 0.1.0\n===== Column Parameters =====\n* id (string): Subject ID\n* pet (string)\n  * Levels\n    * cat: Has a cat\n    * dog: Has a dog\n  * Ordered: FALSE\n* y (float): Happiness Score")
})

test_that("with vardesc", {
  data <- faux::sim_design(2, 2, seed = 8675309, plot = FALSE)
  vardesc <- list(description = c(id = "Subject ID",
                                  B = "Between-subject factor",
                                  A1 = "Condition 1",
                                  A2 = "Condition 2"),
                  levels = list(B = c(B1 = "First level",
                                      B2 = "Second level")))
  cb <- codebook(data, "My Data", vardesc, return = "list")

  output <- capture_output(print(cb))
  expect_equal(output, "Codebook for My Data (Psych-DS 0.1.0)\n========================================\n\n===== Dataset Parameters =====\n* name: My Data\n* schemaVersion: Psych-DS 0.1.0\n===== Column Parameters =====\n* id (string): Subject ID\n* B (string): Between-subject factor\n  * Levels\n    * B1: First level\n    * B2: Second level\n  * Ordered: FALSE\n* A1 (float): Condition 1\n* A2 (float): Condition 2")

  # unseen levels
  vardesc <- list(description = c(id = "Subject ID",
                                  B = "Between-subject factor",
                                  A1 = "Condition 1",
                                  A2 = "Condition 2"),
                  levels = list(B = c(B1 = "First level",
                                      B2 = "Second level",
                                      B3 = "Third level")))
  cb <- codebook(data, "My Data", vardesc, return = "list")
  # should convert levels to a list
  expect_equal(cb$variableMeasured[[2]]$levels,
               list(B1 = "First level",
                    B2 = "Second level",
                    B3 = "Third level"))
})

test_that("ignores extra vardesc", {
  data <- faux::sim_design(2, 2, seed = 8675309, plot = FALSE)
  vardesc <- list(description = c(id = "Subject ID",
                                  B = "Between-subject factor",
                                  C = "Extra Factor",
                                  A1 = "Condition 1",
                                  A2 = "Condition 2"),
                  levels = list(B = c(B1 = "First level",
                                      B2 = "Second level"),
                                C = c("C1", "C2"))
  )

  # turn off messages to check there are no warnings
  pdsbuilder_options(verbose = FALSE)
  expect_silent(
    cb <- codebook(data, "My Data", vardesc, return = "list")
  )
  pdsbuilder_options(verbose = TRUE)
})

test_that("conversion", {
  data <- data.frame(
    i = as.integer(1:10),
    d = as.double(1:10),
    s = LETTERS[1:10],
    f = rnorm(10),
    b = rep(c(T, F), 5),
    l = rep(0:1, 5)
  )

  # no explicit conversion
  ndata <- codebook(data, return = "data")
  expect_equal(typeof(ndata$i), "integer")
  expect_equal(typeof(ndata$d), "integer")
  expect_equal(typeof(ndata$s), "character")
  expect_equal(typeof(ndata$f), "double")
  expect_equal(typeof(ndata$b), "logical")
  expect_equal(typeof(ndata$l), "integer")

  # convert all to string
  vd <- list(type = rep("s", 6))
  ndata <- codebook(data, vardesc = vd, return = "data")
  expect_equal(typeof(ndata$i), "character")
  expect_equal(typeof(ndata$d), "character")
  expect_equal(typeof(ndata$s), "character")
  expect_equal(typeof(ndata$f), "character")
  expect_equal(typeof(ndata$b), "character")
  expect_equal(typeof(ndata$l), "character")

  # convert all to int
  vd <- list(type = rep("i", 6))
  ndata <- codebook(data, vardesc = vd, return = "data")
  expect_equal(typeof(ndata$i), "integer")
  expect_equal(typeof(ndata$d), "integer")
  expect_equal(typeof(ndata$s), "character")
  expect_equal(typeof(ndata$f), "double")
  expect_equal(typeof(ndata$b), "integer")
  expect_equal(typeof(ndata$l), "integer")

  # convert all to float
  vd <- list(type = rep("f", 6))
  ndata <- codebook(data, vardesc = vd, return = "data")
  expect_equal(typeof(ndata$i), "double")
  expect_equal(typeof(ndata$d), "double")
  expect_equal(typeof(ndata$s), "character")
  expect_equal(typeof(ndata$f), "double")
  expect_equal(typeof(ndata$b), "double")
  expect_equal(typeof(ndata$l), "double")

  # convert all to bool
  vd <- list(type = rep("b", 6))
  ndata <- codebook(data, vardesc = vd, return = "data")
  expect_equal(typeof(ndata$i), "integer")
  expect_equal(typeof(ndata$d), "double")
  expect_equal(typeof(ndata$s), "character")
  expect_equal(typeof(ndata$f), "double")
  expect_equal(typeof(ndata$b), "logical")
  expect_equal(typeof(ndata$l), "logical")
})
