context("gpg sign commits")

test_that("sign_commits_with_key returns key if existing key", {
  mockery::stub(sign_commits_with_key, "set_key_to_sign_commits", "dummy_return")
  expect_equal(sign_commits_with_key(key = "test_key"), "dummy_return")
})

test_that("global arg in sign_commits_with_key passed to set_key_to_sign_commits", {
  sign_mock <- mockery::mock()
  mockery::stub(sign_commits_with_key, "set_key_to_sign_commits", sign_mock)
  sign_commits_with_key(key = "test_key", global = FALSE)
  mockery::expect_args(sign_mock, 1, key = "test_key", global = FALSE)
})

test_that("error in sign_commits_with_key if multiple keys for given name, email pair", {
  mockery::stub(sign_commits_with_key, "get_key_candidates", data.frame(id = 1:3))
  expect_error(sign_commits_with_key())
})

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

test_that("commits are signed with found existing key", {
  sign_mock <- mockery::mock()
  mockery::stub(sign_commits_with_key, "set_key_to_sign_commits", sign_mock)
  mockery::stub(
    sign_commits_with_key,
    "get_key_candidates",
    data.frame(id = "test_id", stringsAsFactors = FALSE)
  )
  sign_commits_with_key()
  mockery::expect_args(sign_mock, 1, key = "test_id", global = TRUE)
})

test_that("if neither key nor name provided, name is extracted from git config", {
  git_mock <- mockery::mock()
  mockery::stub(sign_commits_with_key, "extract_git_option", git_mock)
  mockery::stub(sign_commits_with_key, "generate_key_with_name_and_email", "id1")
  mockery::stub(sign_commits_with_key, "set_key_to_sign_commits", "id1")
  mockery::stub(sign_commits_with_key, "get_key_candidates", data.frame())
  sign_commits_with_key(email = "johndoe@example.com")
  mockery::expect_args(git_mock, 1, name = "user.name")
})

test_that("if neither key nor email provided, email is extracted from git config", {
  git_mock <- mockery::mock()
  mockery::stub(sign_commits_with_key, "extract_git_option", git_mock)
  mockery::stub(sign_commits_with_key, "get_key_candidates", data.frame())
  mockery::stub(sign_commits_with_key, "generate_key_with_name_and_email", "id1")
  mockery::stub(sign_commits_with_key, "set_key_to_sign_commits", "id1")
  sign_commits_with_key(name = "John Doe")
  mockery::expect_args(git_mock, 1, name = "user.email")
})

test_that("if neither key nor email provided, email is extracted from git config and used to generate new key", {
  mockery::stub(sign_commits_with_key, "extract_git_option", "johndoe@example.com")
  mockery::stub(sign_commits_with_key, "get_key_candidates", data.frame())
  generate_key_mock <- mockery::mock()
  mockery::stub(
    sign_commits_with_key,
    "generate_key_with_name_and_email",
    generate_key_mock
  )
  mockery::stub(sign_commits_with_key, "set_key_to_sign_commits", "id1")
  sign_commits_with_key(name = "John Doe")
  mockery::expect_args(generate_key_mock, 1, name = "John Doe", email = "johndoe@example.com")
})

test_that("if one existing key found, it is communicated in message", {
  mockery::stub(sign_commits_with_key, "set_key_to_sign_commits", "test_id")
  mockery::stub(
    sign_commits_with_key,
    "get_key_candidates",
    data.frame(id = "test_id", stringsAsFactors = FALSE)
  )
  expect_message(
    sign_commits_with_key(email = "jd@example.com"),
    "Existing key found: `test_id`.\nCorresponding email: `jd@example.com`"
  )
})

test_that("set_key_to_sign_commits: if user did not confirm, git2r config not called and NULL returned", {
  mockery::stub(set_key_to_sign_commits, "extract_git_option", "gpg")
  mockery::stub(set_key_to_sign_commits, "require_confirmation_from_user", FALSE)
  git_config_mock <- mockery::mock()
  mockery::stub(set_key_to_sign_commits, "git2r::config", git_config_mock)
  expect_null(set_key_to_sign_commits("test_key", global = FALSE))
  mockery::expect_called(git_config_mock, 0)
})

test_that("set_key_to_sign_commits: if user did confirm, git2r config is called with appropriate params and key returned", {
  mockery::stub(set_key_to_sign_commits, "extract_git_option", "gpg")
  mockery::stub(set_key_to_sign_commits, "require_confirmation_from_user", TRUE)
  mockery::stub(set_key_to_sign_commits, "extract_email_for_key", "jd@example.com")
  git_config_mock <- mockery::mock()
  mockery::stub(set_key_to_sign_commits, "git2r::config", git_config_mock)
  expect_equal(set_key_to_sign_commits("test_key", global = FALSE), "test_key")
  mockery::expect_args(
    git_config_mock, 1,
    global = FALSE, user.signingkey = "test_key",
    commit.gpgsign = "true",
    user.email = "jd@example.com"
  )
})

test_that("assemble_confirmation_message: ask for confirmation with meaningful message", {
  mockery::stub(assemble_confirmation_message, "extract_email_for_key", "jd@example.com")
  mockery::stub(assemble_confirmation_message, "extract_git_option", "jd2@company.com")
  expect_equal(
    assemble_confirmation_message("test_id", "global" = FALSE),
    paste0(
      "Do you want to sign all future commits with `test_id` ",
      "in this repository?\n",
      "This will also set your user.email ",
      "from `jd2@company.com` to `jd@example.com`.\n"
    )
  )
})

test_that("find matching existing keys: no matching keys returned if initial df is empty", {
  df <- data.frame(name = character(0), email = character(0))
  expect_equal(
    nrow(filter_keys_on_name_and_email_if_provided(df, "", "")),
    0L
  )
})

test_that("find matching existing keys: no matching keys returned if both ids are NULL", {
  df <- data.frame(name = "a", email = "b")
  expect_equal(
    nrow(filter_keys_on_name_and_email_if_provided(df, NULL, NULL)),
    0L
  )
})

test_that("find matching existing keys: matching keys returned if one id is NULL", {
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

test_that("find matching existing keys: matching key returned if both ids provided", {
  df <- data.frame(name = c("a", "a"), email = c("b", "c"))
  expect_equal(
    nrow(filter_keys_on_name_and_email_if_provided(df, "a", "c")),
    1L
  )
})

test_that("extract email for given key", {
  mockery::stub(
    extract_email_for_key,
    "gpg::gpg_list_keys",
    data.frame(id = 1:2, email = 5:6)
  )
  expect_equal(extract_email_for_key(2), 6)
})

test_that("extract git option: locally available", {
  git_config_mock <- mockery::mock(
    list(
      "local" = list("my_option" = "local_value"),
      "global" = list("my_option" = "global_value")
    )
  )
  mockery::stub(extract_git_option, "git2r::config", git_config_mock)
  expected <- "local_value"
  attr(expected, "local") <- TRUE
  expect_equal(extract_git_option("my_option"), expected)
})

test_that("extract git option: only globally available", {
  git_config_mock <- mockery::mock(
    list(
      "local" = list("my_other_option" = "local_value"),
      "global" = list("my_option" = "global_value")
    )
  )
  mockery::stub(extract_git_option, "git2r::config", git_config_mock)
  expected <- "global_value"
  attr(expected, "local") <- FALSE
  expect_equal(extract_git_option("my_option"), expected)
})

test_that("extract git option: not available", {
  git_config_mock <- mockery::mock(
    list(
      "local" = list("my_other_option" = "local_value"),
      "global" = list("my_option" = "global_value")
    )
  )
  mockery::stub(extract_git_option, "git2r::config", git_config_mock)
  expect_equal(extract_git_option("nonexistent_option"), NULL)
})

test_that("safe getPass is called to retrieve password", {
  getPassMock <- mockery::mock("")
  mockery::stub(generate_key_with_name_and_email, "getPass::getPass", getPassMock)
  mockery::stub(generate_key_with_name_and_email, "gpg::gpg_keygen", "")
  generate_key_with_name_and_email("John Doe", "jd@example.com")
  mockery::expect_called(getPassMock, 1)
})

test_that("throw error if gpg password prompt cancelled", {
  mockery::stub(generate_key_with_name_and_email, "getPass::getPass", NULL)
  expect_error(
    generate_key_with_name_and_email("John Doe", "jd@example.com"),
    "GPG key generation cancelled by user"
  )
})

test_that("generate key: if no password, use NULL", {
  mockery::stub(generate_key_with_name_and_email, "getPass::getPass", "")
  keygen_mock <- mockery::mock()
  mockery::stub(generate_key_with_name_and_email, "gpg::gpg_keygen", keygen_mock)
  generate_key_with_name_and_email("John Doe", "jd@example.com")
  mockery::expect_args(
    keygen_mock, 1,
    name = "John Doe", email = "jd@example.com", passphrase = NULL
  )
})

test_that("generate key: message used name and email", {
  mockery::stub(generate_key_with_name_and_email, "getPass::getPass", "")
  mockery::stub(generate_key_with_name_and_email, "gpg::gpg_keygen", NULL)
  expect_message(
    generate_key_with_name_and_email("John Doe", "jd@example.com"),
    "`John Doe` \\(as provided\\).*`jd@example\\.com` \\(as provided\\)"
  )
})

test_that("generate key: message based on source of param", {
  mockery::stub(generate_key_with_name_and_email, "getPass::getPass", "")
  mockery::stub(generate_key_with_name_and_email, "gpg::gpg_keygen", NULL)
  name <- "John Doe"
  attr(name, "local") <- TRUE
  email <- "jd@example.com"
  attr(email, "local") <- FALSE
  expect_message(
    generate_key_with_name_and_email(name, email),
    ".*\\(based on local git config\\).*\\(based on global git config\\)"
  )
})

test_that("error is thrown if email is neither provided nor available in git config", {
  mockery::stub(sign_commits_with_key, "extract_git_option", NULL)
  mockery::stub(sign_commits_with_key, "get_key_candidates", data.frame())
  expect_error(
    sign_commits_with_key(global = FALSE),
    "Name and email are required"
  )
})

test_that("gpg program is set at first time", {
  mockery::stub(set_key_to_sign_commits, "assemble_confirmation_message", "")
  mockery::stub(set_key_to_sign_commits, "require_confirmation_from_user", FALSE)
  mockery::stub(set_key_to_sign_commits, "extract_git_option", NULL)
  git_config_mock <- mockery::mock()
  mockery::stub(set_key_to_sign_commits, "git2r::config", git_config_mock)
  set_key_to_sign_commits("ABCD", global = FALSE)
  mockery::expect_args(
    git_config_mock, 1, global = TRUE, gpg.program = "gpg"
  )
})

context("gh store key")

test_that("if token is not provided, public key returned with message", {
  mockery::stub(gh_store_key, "gpg::gpg_export", "public_key_block")
  expect_message(
    gh_store_key("ABCD"),
    "Could not add.*token is not provided"
  )
  expect_output(
    gh_store_key("ABCD"),
    "public_key_block"
  )
})

test_that("if key already exists, appropriate message is returned", {
  mockery::stub(gh_store_key, "gpg::gpg_export", "public_key_block")
  gh_error <- ""
  class(gh_error) <- "try-error"
  gh_condition <- ""
  class(gh_condition) <- "http_error_422"
  attr(gh_error, "condition") <- gh_condition
  mockery::stub(gh_store_key, "gh_attempt_key_upload", gh_error)
  expect_message(
    gh_store_key("ABCD", "mytoken"),
    "Public GPG key is already stored on GitHub."
  )
})

test_that("if other error from github, communicate unsuccessful upload", {
  mockery::stub(gh_store_key, "gpg::gpg_export", "public_key_block")
  gh_error <- ""
  class(gh_error) <- "try-error"
  attr(gh_error, "condition") <- ""
  mockery::stub(gh_store_key, "gh_attempt_key_upload", gh_error)
  expect_message(
    gh_store_key("ABCD", "mytoken"),
    "Could not add"
  )
  expect_output(
    gh_store_key("ABCD", "mytoken"),
    "public_key_block"
  )
})

test_that("if upload is successful but key is unverified, communicate it", {
  mockery::stub(gh_store_key, "gpg::gpg_export", "public_key_block")
  gh_answer <- list("emails" = list(list("verified" = FALSE)))
  mockery::stub(gh_store_key, "gh_attempt_key_upload", gh_answer)
  expect_warning(
    gh_store_key("ABCD", "mytoken"),
    "Uploaded key in unverified."
  )
})