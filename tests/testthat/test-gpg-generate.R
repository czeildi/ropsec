context("gpg-generate")

describe("sign_commits_with_key", {
  it("generates new key if no existing key found", {
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
})

describe("generate_key_with_name_and_email", {
  it("calls safe getPass to retrieve password", {
    mockery::stub(generate_key_with_name_and_email, "ask_yes_no", TRUE)
    getPassMock <- mockery::mock("pwd")
    mockery::stub(generate_key_with_name_and_email, "getPass::getPass", getPassMock)
    mockery::stub(generate_key_with_name_and_email, "generate_key_with_checked_params", "")
    generate_key_with_name_and_email("John Doe", "jd@example.com")
    mockery::expect_called(getPassMock, 1)
  })
  it("throws error if gpg password prompt cancelled", {
    mockery::stub(generate_key_with_name_and_email, "ask_yes_no", TRUE)
    mockery::stub(generate_key_with_name_and_email, "getPass::getPass", NULL)
    expect_error(
      generate_key_with_name_and_email("John Doe", "jd@example.com"),
      "GPG key generation cancelled by user"
    )
  })
  it("throws error if use? password prompt cancelled", {
    mockery::stub(generate_key_with_name_and_email, "ask_yes_no", NA)
    expect_error(
      generate_key_with_name_and_email("John Doe", "jd@example.com"),
      "GPG key generation cancelled by user"
    )
  })

  it("error if conflicting password preferences", {
    mockery::stub(generate_key_with_name_and_email, "ask_yes_no", TRUE)
    mockery::stub(generate_key_with_name_and_email, "getPass::getPass", "")
    keygen_mock <- mockery::mock("id")
    mockery::stub(generate_key_with_name_and_email, "generate_key_with_checked_params", keygen_mock)
    expect_error(
      generate_key_with_name_and_email("John Doe", "jd@example.com"),
      "Either provide a non-empty password or start again and opt for no password protection."
    )
  })

  it("uses NULL if no password in Rstudio", {
    mockery::stub(generate_key_with_name_and_email, "ask_yes_no", FALSE)
    keygen_mock <- mockery::mock("id")
    mockery::stub(generate_key_with_name_and_email, "generate_key_with_checked_params", keygen_mock)
    generate_key_with_name_and_email("John Doe", "jd@example.com")
    mockery::expect_args(
      keygen_mock, 1,
      name = "John Doe", email = "jd@example.com", passphrase = NULL
    )
  })

  it("messages used name and email", {
    mockery::stub(generate_key_with_name_and_email, "ask_yes_no", TRUE)
    mockery::stub(generate_key_with_name_and_email, "getPass::getPass", "pwd")
    mockery::stub(generate_key_with_name_and_email, "generate_key_with_checked_params", "id")
    expect_message(
      generate_key_with_name_and_email("John Doe", "jd@example.com"),
      "`John Doe` \\(as provided\\).*`jd@example\\.com` \\(as provided\\)"
    )
  })

  it("messages based on source of param", {
    mockery::stub(generate_key_with_name_and_email, "ask_yes_no", TRUE)
    mockery::stub(generate_key_with_name_and_email, "getPass::getPass", "pwd")
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
})

describe("generate_key_with_checked_params", {
  it("communicates success", {
    mockery::stub(generate_key_with_checked_params, "gpg::gpg_keygen", "id")
    expect_output(
      generate_key_with_checked_params("JD", "jd@jd.com", NULL),
      "successfully generated"
    )
  })
})
