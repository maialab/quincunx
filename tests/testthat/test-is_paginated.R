test_that("is not paginated", {
  expect_false(is_paginated(''))
  expect_false(is_paginated('{\n  \"id\": \"PGP000001\"\n }'))
})

test_that("is paginated", {
  txt <- '{
  "count": 6,
  "next": null,
  "previous": null
  }
'
  expect_true(is_paginated(txt))
})
