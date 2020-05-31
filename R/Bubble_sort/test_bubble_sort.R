library(testthat)

context("When Reverse is set to FALSE")

describe("Can handle small test data set", {
  to_sort <-
    sample(x = 0:9, size = 50, replace = TRUE)

  it("returns the sorted input correctly", {
    expect_equal(
      bubble_sort(to_sort), sort(to_sort)
    )
  })
})

describe("Can handle large test data set", {
  to_sort <-
    sample(x = 0:100, size = 1000, replace = TRUE)

  it("returns the sorted input correctly", {
    expect_equal(
      bubble_sort(to_sort), sort(to_sort)
    )
  })
})

context("When Reverse is set to TRUE")

describe("Can handle small test data set", {
  to_sort <-
    sample(x = 0:9, size = 50, replace = TRUE)

  it("returns the sorted input correctly", {
    expect_equal(
      bubble_sort(to_sort, reverse = TRUE), rev(sort(to_sort))
    )
  })
})
