context("gpg-generate")

test_that("new key generated if no existing key found", {
  mockery::stub(sign_commits_with_key, "get_key_candidates", data.frame(id = c()))
  generate_key_mock <- mockery::mock()
  mockery::stub(
    sign_commits_with_key,
    "generate_key_with_name_and_email",
    generate_key_mock
  )
  mockery::stub(sign_commits_with_key, "set_key_to_sign_commits", "id1")
  sign_commits_with_key()
  mockery::expect_called(generate_key_mock, 1)
})

test_that("safe getPass is called to retrieve password", {
  mockery::stub(generate_key_with_name_and_email, "utils::askYesNo", TRUE)
  getPassMock <- mockery::mock("")
  mockery::stub(generate_key_with_name_and_email, "getPass::getPass", getPassMock)
  mockery::stub(generate_key_with_name_and_email, "generate_key_with_checked_params", "")
  generate_key_with_name_and_email("John Doe", "jd@example.com")
  mockery::expect_called(getPassMock, 1)
})

test_that("throw error if gpg password prompt cancelled", {
  mockery::stub(generate_key_with_name_and_email, "utils::askYesNo", TRUE)
  mockery::stub(generate_key_with_name_and_email, "getPass::getPass", NULL)
  expect_error(
    generate_key_with_name_and_email("John Doe", "jd@example.com"),
    "GPG key generation cancelled by user"
  )
})

test_that("throw error if use? password prompt cancelled", {
  mockery::stub(generate_key_with_name_and_email, "utils::askYesNo", NA)
  expect_error(
    generate_key_with_name_and_email("John Doe", "jd@example.com"),
    "GPG key generation cancelled by user"
  )
})

test_that("generate key: if no password in terminal, use NULL", {
  mockery::stub(generate_key_with_name_and_email, "utils::askYesNo", TRUE)
  mockery::stub(generate_key_with_name_and_email, "getPass::getPass", "")
  keygen_mock <- mockery::mock("id")
  mockery::stub(generate_key_with_name_and_email, "generate_key_with_checked_params", keygen_mock)
  generate_key_with_name_and_email("John Doe", "jd@example.com")
  mockery::expect_args(
    keygen_mock, 1,
    name = "John Doe", email = "jd@example.com", passphrase = NULL
  )
})

test_that("generate key: if no password in Rstudio, use NULL", {
  mockery::stub(generate_key_with_name_and_email, "utils::askYesNo", FALSE)
  keygen_mock <- mockery::mock("id")
  mockery::stub(generate_key_with_name_and_email, "generate_key_with_checked_params", keygen_mock)
  generate_key_with_name_and_email("John Doe", "jd@example.com")
  mockery::expect_args(
    keygen_mock, 1,
    name = "John Doe", email = "jd@example.com", passphrase = NULL
  )
})

test_that("generate key: message used name and email", {
  mockery::stub(generate_key_with_name_and_email, "utils::askYesNo", TRUE)
  mockery::stub(generate_key_with_name_and_email, "getPass::getPass", "")
  mockery::stub(generate_key_with_name_and_email, "generate_key_with_checked_params", "id")
  expect_message(
    generate_key_with_name_and_email("John Doe", "jd@example.com"),
    "`John Doe` \\(as provided\\).*`jd@example\\.com` \\(as provided\\)"
  )
})

test_that("generate key: message based on source of param", {
  mockery::stub(generate_key_with_name_and_email, "utils::askYesNo", TRUE)
  mockery::stub(generate_key_with_name_and_email, "getPass::getPass", "")
  mockery::stub(generate_key_with_name_and_email, "generate_key_with_checked_params", "id")
  name <- "John Doe"
  attr(name, "local") <- TRUE
  email <- "jd@example.com"
  attr(email, "local") <- FALSE
  expect_message(
    generate_key_with_name_and_email(name, email),
    ".*\\(based on local git config\\).*\\(based on global git config\\)"
  )
})

test_that("generate key: success is communicated", {
  mockery::stub(generate_key_with_checked_params, "gpg::gpg_keygen", "id")
  expect_output(
    generate_key_with_checked_params("JD", "jd@jd.com", NULL),
    "successfully generated"
  )
})
