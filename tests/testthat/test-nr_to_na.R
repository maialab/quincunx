test_that("normal usage", {
  expect_equal(nr_to_na('NR'), NA_character_)
  expect_equal(nr_to_na(c('NR', 'a')), c(NA_character_, 'a'))
})
