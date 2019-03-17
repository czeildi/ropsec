context("store public key")

describe("store_public_key", {
  describe("no service or service independent error", {
    it("error if user tries to upload non-existent key", {
      mockery::stub(store_public_key, "gpg::gpg_export", "")
      expect_error(
        store_public_key("non-existent key"),
        "Key of id `non-existent key` is not found on local system. You can generate one with `sign_commits_with_key`"
      )
    })
    it("if token is not provided, public key returned with message", {
      mockery::stub(store_public_key, "gpg::gpg_export", "public_key_block")
      mockery::stub(store_public_key, "Sys.getenv", "")
      expect_message(
        store_public_key("ABCD", service = "gh"),
        "Could not add.*token is not provided"
      )
      expect_output(
        store_public_key("ABCD", service = "gh"),
        "public_key_block"
      )
    })
  })

  describe("if service is gh (GitHub)", {

    it("if key already exists, appropriate message is returned", {
      mockery::stub(store_public_key, "gpg::gpg_export", "public_key_block")
      gh_error <- ""
      class(gh_error) <- "try-error"
      gh_condition <- ""
      class(gh_condition) <- "http_error_422"
      attr(gh_error, "condition") <- gh_condition
      mockery::stub(store_public_key, "gh_attempt_key_upload", gh_error)
      expect_message(
        store_public_key("ABCD", service = "gh", .token = "mytoken"),
        "Public GPG key is already stored on GitHub."
      )
    })

    it("if other error from github, communicate unsuccessful upload", {
      mockery::stub(store_public_key, "gpg::gpg_export", "public_key_block")
      gh_error <- ""
      class(gh_error) <- "try-error"
      attr(gh_error, "condition") <- ""
      mockery::stub(store_public_key, "gh_attempt_key_upload", gh_error)
      expect_message(
        store_public_key("ABCD", service = "gh", .token = "mytoken"),
        "Could not add"
      )
      expect_output(
        store_public_key("ABCD", service = "gh", .token = "mytoken"),
        "public_key_block"
      )
    })

    it("if upload is successful but key is unverified, communicate it", {
      mockery::stub(store_public_key, "gpg::gpg_export", "public_key_block")
      gh_answer <- list("emails" = list(list("verified" = FALSE)))
      mockery::stub(store_public_key, "gh_attempt_key_upload", gh_answer)
      expect_warning(
        store_public_key("ABCD", service = "gh", .token = "mytoken"),
        "Uploaded key is unverified"
      )
    })
  })

  describe("service is gitlab", {
    it("throws informative error if upload is successful but email address does not match", {

    })
    it("if upload is unauthorized, communicate it", {

    })
    it("if unknown error, communicate it", {

    })
    it("perceived success is communicated", {

    })
  })
})
