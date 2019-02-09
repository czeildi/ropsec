context("gh store key")

describe("gh_store_key", {

  it("if token is not provided, public key returned with message", {
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

  it("if key already exists, appropriate message is returned", {
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

  it("if other error from github, communicate unsuccessful upload", {
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

  it("if upload is successful but key is unverified, communicate it", {
    mockery::stub(gh_store_key, "gpg::gpg_export", "public_key_block")
    gh_answer <- list("emails" = list(list("verified" = FALSE)))
    mockery::stub(gh_store_key, "gh_attempt_key_upload", gh_answer)
    expect_warning(
      gh_store_key("ABCD", "mytoken"),
      "Uploaded key is unverified"
    )
  })

  it("error if user tries to upload non-existent key", {
    mockery::stub(gh_store_key, "gpg::gpg_export", "")
    expect_error(
      gh_store_key("non-existent key"),
      "Key of id `non-existent key` is not found on local system. You can generate one with `sign_commits_with_key`"
    )
  })
})
