context("store public key")

describe("store_public_key", {
  describe("no service or service independent error", {
    it("error if user supplies key in wrong format", {
      expect_error(
        store_public_key(""),
        "key should be the id or fingerprint of an existing gpg key"
      )
    })
    it("error if user tries to upload non-existent key", {
      mockery::stub(store_public_key, "gpg::gpg_export", "")
      expect_error(
        store_public_key("non-existent key"),
        "Key of id `non-existent key` is not found on local system. You can generate one with `sign_commits_with_key`"
      )
    })
    it("if service is not provided, public key returned with message", {
      mockery::stub(store_public_key, "gpg::gpg_export", "public_key_block")
      expect_output(
        store_public_key("ABCD"),
        "public_key_block"
      )
    })
    it("if token is not provided directly or in env var, public key returned with message", {
      mockery::stub(store_public_key, "gpg::gpg_export", "public_key_block")
      mockery::stub(store_public_key, "Sys.getenv", "")
      expect_message(
        store_public_key("ABCD", service = "gh"),
        "Token is not provided."
      )
      expect_output(
        store_public_key("ABCD", service = "gh"),
        "public_key_block"
      )
    })
  })

  describe("if service is gh (GitHub)", {

    it("if key already exists, appropriate message is returned", {
      gh_error <- ""
      class(gh_error) <- "try-error"
      gh_condition <- ""
      class(gh_condition) <- "http_error_422"
      attr(gh_error, "condition") <- gh_condition
      mockery::stub(gh_store_key, "gh_attempt_key_upload", gh_error)
      expect_message(
        gh_store_key("public_key_block", .token = "mytoken", FALSE, "jd@ex.com"),
        "Public GPG key is already stored on GitHub."
      )
    })

    it("if unauthorized request, appropriate message is returned", {
      gh_error <- ""
      class(gh_error) <- "try-error"
      gh_condition <- ""
      class(gh_condition) <- "http_error_401"
      attr(gh_error, "condition") <- gh_condition
      mockery::stub(gh_store_key, "gh_attempt_key_upload", gh_error)
      expect_message(
        gh_store_key("public_key_block", .token = "mytoken", FALSE, "jd@ex.com"),
        "Unauthorized request. Check your token."
      )
      expect_output(
        gh_store_key("public_key_block", .token = "mytoken", FALSE, "jd@ex.com"),
        "public_key_block"
      )
    })

    it("if other error from github, communicate unsuccessful upload", {
      gh_error <- ""
      class(gh_error) <- "try-error"
      attr(gh_error, "condition") <- ""
      mockery::stub(gh_store_key, "gh_attempt_key_upload", gh_error)
      expect_message(
        gh_store_key("public_key_block", .token = "mytoken", FALSE, "jd@ex.com"),
        "Unknown reason"
      )
      expect_output(
        gh_store_key("public_key_block", .token = "mytoken", FALSE, "jd@ex.com"),
        "public_key_block"
      )
    })

    it("if upload is successful but key is unverified, communicate it", {
      gh_answer <- list("emails" = list(list("verified" = FALSE)))
      mockery::stub(gh_store_key, "gh_attempt_key_upload", gh_answer)
      expect_warning(
        gh_store_key("public_key_block", .token = "mytoken", FALSE, "jd@ex.com"),
        "Uploaded key is unverified"
      )
    })
  })

  describe("service is gitlab", {
    it("throws informative error if upload is successful but email address does not match", {
      mockery::stub(gl_store_key, "gl_user_email", "other_email")
      mockery::stub(gl_store_key, "gl_attempt_key_upload", NULL)
      mockery::stub(gl_store_key, "httr::status_code", 201)
      expect_warning(
        gl_store_key("public_key_block", .token = "mytoken", "url", FALSE, "email"),
        "Uploaded key is unverified"
      )
    })
    it("if upload is unauthorized, communicate it", {
      mockery::stub(gl_store_key, "gl_user_email", "email")
      mockery::stub(gl_store_key, "gl_attempt_key_upload", NULL)
      mockery::stub(gl_store_key, "httr::status_code", 401)
      expect_message(
        gl_store_key("public_key_block", .token = "mytoken", "url", FALSE, "email"),
        "Unauthorized request. Check your token."
      )
    })
    it("if key already uploaded", {
      mockery::stub(gl_store_key, "gl_user_email", "email")
      mockery::stub(gl_store_key, "gl_attempt_key_upload", NULL)
      mockery::stub(gl_store_key, "httr::status_code", 400)
      mockery::stub(gl_store_key, "httr::content", list("message" = list("key" = list("has already been taken"))))
      expect_message(
        gl_store_key("public_key_block", .token = "mytoken", "url", FALSE, "email"),
        "Public GPG key is already stored on Gitlab."
      )
    })
    it("if unknown error, communicate it", {
      mockery::stub(gl_store_key, "gl_user_email", "email")
      mockery::stub(gl_store_key, "gl_attempt_key_upload", NULL)
      mockery::stub(gl_store_key, "httr::status_code", 400)
      mockery::stub(gl_store_key, "httr::content", list())
      expect_message(
        gl_store_key("public_key_block", .token = "mytoken", "url", FALSE, "email"),
        "Unknown reason"
      )
    })
    it("outputs public key if gitlab_url not provided", {
      mockery::stub(store_public_key, "gpg::gpg_export", "public_key_block")
      expect_output(
        store_public_key("ABCD", service = "gl", .token = "mytoken"),
        "public_key_block"
      )
    })
  })
})
