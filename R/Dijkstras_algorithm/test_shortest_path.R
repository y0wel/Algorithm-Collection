library(testthat)

# Test data -----------------------------------------------------------------------------------

cities <-
  data.frame(
    nodes_from = c(
      "Frankfurt", "Frankfurt", "Frankfurt", "Mannheim",
      "Mannheim", "Würzburg", "Würzburg", "Würzburg",
      "Erfurt", "Stuttgart", "Nürnberg", "Nürnberg",
      "Nürnberg", "Kassel", "Kassel", "Karlsruhe",
      "Karlsruhe", "Augsburg", "Augsburg", "München",
      "München", "München"
    ),
    nodes_to = c(
      "Mannheim", "Würzburg", "Kassel", "Karlsruhe",
      "Frankfurt", "Erfurt", "Nürnberg", "Frankfurt",
      "Würzburg", "Nürnberg", "Stuttgart", "Würzburg",
      "München", "Frankfurt", "München", "Mannheim",
      "Augsburg", "Karlsruhe", "München", "Augsburg",
      "Nürnberg", "Kassel"
    ),
    distances = c(
      85, 217, 173, 80,
      85, 186, 103, 217,
      186, 183, 183, 103,
      167, 173, 502, 80,
      250, 250, 84, 84,
      167, 502
    ),
    stringsAsFactors = FALSE # not needed for R version >= 4.0.0
  )

# Tests ---------------------------------------------------------------------------------------

context("Error handling")

test_data <- cities
  
describe("When start node is blank", {
  start <- ""
  end <- "München"

  it("returns an error", {
    expect_error(
      find_shortest_path(test_data, start, end),
      "Start node is blank!"
    )
  })
})

describe("When end node is blank", {
  start <- "München"
  end <- ""

  it("returns an error", {
    expect_error(
      find_shortest_path(test_data, start, end),
      "End node is blank!"
    )
  })
})

describe("When start node is not in data input", {
  start <- "Tokio"
  end <- "Frankfurt"

  it("returns an error", {
    expect_error(
      find_shortest_path(test_data, start, end),
      "Start node could not be found in data input!"
    )
  })
})

describe("When end node is not in data input", {
  start <- "Frankfurt"
  end <- "Tokio"

  it("returns an error", {
    expect_error(
      find_shortest_path(test_data, start, end),
      "End node could not be found in data input!"
    )
  })
})

context("Finds shortest path correctly")

describe("Shortest Path Frankfurt to München", {
  start <- "Frankfurt"
  end <- "München"

  it("returns the correct distance", {
    expect_equal(
      find_shortest_path(test_data, start, end)[[1]],
      487
    )
  })

  reverse_start <- end
  reverse_end <- start

  it("returns the correct distance", {
    expect_equal(
      find_shortest_path(test_data, reverse_start, reverse_end)[[1]],
      487
    )
  })
})

describe("Shortest Path Mannheim to Stuttgart", {
  start <- "Mannheim"
  end <- "Stuttgart"

  it("returns the correct distance", {
    expect_equal(
      find_shortest_path(test_data, start, end)[[1]], 588
    )
  })
})

describe("Shortest Path Augsburg to Würzburg", {
  start <- "Augsburg"
  end <- "Würzburg"

  it("returns the correct distance", {
    expect_equal(
      find_shortest_path(test_data, start, end)[[1]], 354
    )
  })
})

describe("Shortest Path Nürnberg to Mannheim", {
  start <- "Nürnberg"
  end <- "Mannheim"

  it("returns the correct distance", {
    expect_equal(
      find_shortest_path(test_data, start, end)[[1]], 405
    )
  })
})

describe("When start and end node are direct neighbors", {
  start <- "Karlsruhe"
  end <- "Mannheim"

  it("returns the correct distance", {
    expect_equal(
      find_shortest_path(test_data, start, end)[[1]], 80
    )
  })
})

describe("When start and end node are the same", {
  start <- "Augsburg"
  end <- start

  it("returns 0", {
    expect_equal(
      find_shortest_path(test_data, start, end), 0
    )
  })
})
