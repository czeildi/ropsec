context("find matching existing keys")

test_that("no matching keys returned if initial df is empty", {
  df <- data.frame(name = character(0), email = character(0))
  expect_equal(
    nrow(filter_keys_on_name_and_email_if_provided(df, "", "")),
    0L
  )
})

test_that("no matching keys returned if both ids are NULL", {
  df <- data.frame(name = "a", email = "b")
  expect_equal(
    nrow(filter_keys_on_name_and_email_if_provided(df, NULL, NULL)),
    0L
  )
})

test_that("matching keys returned if one id is NULL", {
  df <- data.frame(name = c("a", "a"), email = c("b", "b"))
  expect_equal(
    nrow(filter_keys_on_name_and_email_if_provided(df, "a", NULL)),
    2L
  )
  expect_equal(
    nrow(filter_keys_on_name_and_email_if_provided(df, NULL, "b")),
    2L
  )
})

test_that("matching key returned if both ids provided", {
  df <- data.frame(name = c("a", "a"), email = c("b", "c"))
  expect_equal(
    nrow(filter_keys_on_name_and_email_if_provided(df, "a", "c")),
    1L
  )
})