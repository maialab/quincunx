test_that("normal usage", {
  expect_identical(count('{"count":0,"next": "asdas"}'), 0L)
  expect_identical(count('{"count":663,"next": "asdas"}'), 663L)
})

test_that("returns the top level match", {
  expect_identical(count('{"count":42,"next":{"count":43}}'), 42L)
})

test_that("no match returns NA_integer_", {
  expect_true(is.na(count('{"count": null}')))
  expect_true(is.na(count('{"counts":42}')))
  expect_true(is.na(count('')))
})
