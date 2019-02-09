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
  mockery::stub(sign_commits_with_key, "find_git_option", git_mock)
  mockery::stub(sign_commits_with_key, "generate_key_with_name_and_email", "id1")
  mockery::stub(sign_commits_with_key, "set_key_to_sign_commits", "id1")
  mockery::stub(sign_commits_with_key, "get_key_candidates", data.frame())
  sign_commits_with_key(email = "johndoe@example.com")
  mockery::expect_args(git_mock, 1, name = "user.name")
})

test_that("if neither key nor email provided, email is extracted from git config", {
  git_mock <- mockery::mock()
  mockery::stub(sign_commits_with_key, "find_git_option", git_mock)
  mockery::stub(sign_commits_with_key, "get_key_candidates", data.frame())
  mockery::stub(sign_commits_with_key, "generate_key_with_name_and_email", "id1")
  mockery::stub(sign_commits_with_key, "set_key_to_sign_commits", "id1")
  sign_commits_with_key(name = "John Doe")
  mockery::expect_args(git_mock, 1, name = "user.email")
})

test_that("if neither key nor email provided, email is extracted from git config and used to generate new key", {
  mockery::stub(sign_commits_with_key, "find_git_option", "johndoe@example.com")
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

test_that("set_key_to_sign_commits: if everything already set, it is communicated and no further confirmation needed", {
  mockery::stub(set_key_to_sign_commits, "is_commit_signing_already_set", TRUE)
  user_confirmation_mock <- mockery::mock()
  mockery::stub(set_key_to_sign_commits, "require_confirmation_from_user", FALSE)
  expect_message(
    set_key_to_sign_commits("test_key", global = FALSE),
    "already set"
  )
  mockery::expect_called(user_confirmation_mock, 0)
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
  expect_match(
    assemble_confirmation_message("test_id", "global" = FALSE),
    "from `jd2@company\\.com` to `jd@example\\.com`"
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

test_that("error is thrown if email is neither provided nor available in git config", {
  mockery::stub(sign_commits_with_key, "find_git_option", NULL)
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
    git_config_mock, 1, global = FALSE, gpg.program = "gpg"
  )
})
