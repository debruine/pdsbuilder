test_that("x", {
  x <- list(
    a = list(a1 = "Named", a2 = "List"),
    b = list("Unnamed", "List"),
    c = c(c1 = "Named", c2 = "Vector"),
    d = c("Unnamed", "Vector"),
    e = list(e1 = list("A", "B", "C"),
             e2 = list(a = "A", b = "B"),
             e3 = c("A", "B", "C"),
             e4 = 100),
    f = "not a list or vector",
    g = list(
      list(a = "Nested", b = "Unnamed"),
      list("List", "of", "Lists")
    )
  )
  nl <- nested_list(x)
  comp <- "* a: \n  * a1: Named\n  * a2: List\n* b: \n  * Unnamed\n  * List\n* c: \n  * c1: Named\n  * c2: Vector\n* d: \n  * Unnamed\n  * Vector\n* e: \n  * e1: \n    * A\n    * B\n    * C\n  * e2: \n    * a: A\n    * b: B\n  * e3: \n    * A\n    * B\n    * C\n  * e4: 100\n* f: not a list or vector\n* g: \n  * 1: \n     * a: Nested\n     * b: Unnamed\n  * 2: \n     * List\n     * of\n     * Lists"
  class(comp) <- c("nested_list", "character")

  expect_equal(nl, comp)
})
