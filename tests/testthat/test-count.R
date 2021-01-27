test_that("normal usage", {
  expect_identical(count('{"count":0,"next":'), 0L)
  expect_identical(count('{"count":663,"next":'), 663L)
})

test_that("more than one match return the first match", {
  expect_identical(count('{"count":42,"next":"count":43'), 42L)
})

test_that("no match returns NA_integer_", {
  expect_true(is.na(count('{"count":')))
  expect_true(is.na(count('{"counts":42')))
  expect_true(is.na(count('')))
})
