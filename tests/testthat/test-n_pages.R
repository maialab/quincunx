test_that("normal usage", {
  expect_equal(n_pages(count = 5, limit = 1), 5L)
  expect_equal(n_pages(count = 5, limit = 2), 3L)
  expect_equal(n_pages(count = 5, limit = 5), 1L)
  expect_equal(n_pages(count = 5, limit = 6), 1L)
})
