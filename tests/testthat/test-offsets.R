test_that("normal usage", {
  expect_equal(offsets(5, 1), c(0L, 1L, 2L, 3L, 4L))
  expect_equal(offsets(10, 2), c(0L, 2L, 4L, 6L, 8L))
  expect_equal(offsets(10, 3), c(0L, 3L, 6L, 9L))
  expect_equal(offsets(10, 10), 0L)
  expect_equal(offsets(10, 11), 0L)
})
