context("test-readline_check")

test_that("error", {
  expect_error(readline_check(), "argument \"prompt\" is missing, with no default", fixed = TRUE)
})

test_that("interactive", capture_output_lines({
  # Thanks https://stackoverflow.com/users/2752888/znk
  # https://stackoverflow.com/questions/41372146/test-interaction-with-users-in-r-package

  f <- file()
  options(faux.connection = f)

  q <- list(
    "1" = "Type a number: ",
    "-1" = "Type a number less than or equal to 0: ",
    "15" = "Type a number from 10 to 20: ",
    "LD" = "Type two characters: ",
    "Lisa DeBruine" = "Type at least 3 characters: ",
    "LDB" = "Type no more than 4 characters: ",
    "D4" = "Type a letter and a number: "
  )
  ans <- paste(names(q), collapse = "\n")
  write(ans, f)

  n <- readline_check(q[[1]], "numeric")
  expect_true(is.numeric(n))
  n0 <- readline_check(q[[2]], "numeric", max = 0)
  expect_true(is.numeric(n0))
  expect_true(n0 <= 0)
  n10 <- readline_check(q[[3]], "numeric", min = 10, max = 20)
  expect_true(is.numeric(n10))
  expect_true(n10 <= 20)
  expect_true(n10 >= 10)
  c2 <- readline_check(q[[4]], "length", min=2, max = 2)
  expect_true(nchar(c2)==2)
  c3 <- readline_check(q[[5]], "length", min =3)
  expect_true(nchar(c3) >= 3)
  c4 <- readline_check(q[[6]], "length", max = 4)
  expect_true(nchar(c4) <= 4)
  ln <- readline_check(q[[7]], "grep",
                       warning = "\033[31mIncorrect.\033[39m Type only a letter and a number: ",
                       pattern = "^[a-zA-Z]\\d$")
  expect_equal(grep("^[a-zA-Z]\\d$", ln), 1)

  options(faux.connection = stdin()) # reset connection
  close(f) # close the file
}))
