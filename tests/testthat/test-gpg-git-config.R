context("gpg-git-config")

describe("extract_email_for_key", {
  it("extracts email for given key", {
    mockery::stub(
      extract_email_for_key,
      "gpg::gpg_list_keys",
      data.frame(id = 1:2, email = 5:6)
    )
    expect_equal(extract_email_for_key(2), 6)
  })
})


describe("find_git_option", {

  it("finds option if it is locally available", {
    git_config_mock <- mockery::mock(
      list(
        "local" = list("my_option" = "local_value"),
        "global" = list("my_option" = "global_value")
      )
    )
    mockery::stub(find_git_option, "git2r::config", git_config_mock)
    expected <- "local_value"
    attr(expected, "local") <- TRUE
    expect_equal(find_git_option("my_option"), expected)
  })

  it("finds git option if only globally available", {
    git_config_mock <- mockery::mock(
      list(
        "local" = list("my_other_option" = "local_value"),
        "global" = list("my_option" = "global_value")
      )
    )
    mockery::stub(find_git_option, "git2r::config", git_config_mock)
    expected <- "global_value"
    attr(expected, "local") <- FALSE
    expect_equal(find_git_option("my_option"), expected)
  })

  it("returns NULL if not available", {
    git_config_mock <- mockery::mock(
      list(
        "local" = list("my_other_option" = "local_value"),
        "global" = list("my_option" = "global_value")
      )
    )
    mockery::stub(find_git_option, "git2r::config", git_config_mock)
    expect_equal(find_git_option("nonexistent_option"), NULL)
  })
})

describe("config conflict", {
  describe("does_local_config_overwrite_global_config", {
    it("identifies existing conflict", {
      m <- mockery::mock(FALSE, FALSE, TRUE, FALSE)
      mockery::stub(does_local_config_overwrite_global_config, "does_local_option_overwrite_global_option", m)
      expect_true(does_local_config_overwrite_global_config())
    })
  })
  describe("does_local_option_overwrite_global_option", {
    it("identifies existing conflict", {
      mockery::stub(does_local_option_overwrite_global_option, "git2r::config", list(
        "global" = list("a" = 1), "local" = list("a" = 2)
      ))
      expect_true(does_local_option_overwrite_global_option("a"))
    })
  })
  describe("does_local_config_conflict_required_global_config", {
    it("it identifies existing conflict", {
      m <- mockery::mock(FALSE, TRUE)
      mockery::stub(does_local_config_conflict_required_global_config, "does_local_option_conflict_required_global_option", m)
      expect_true(does_local_config_conflict_required_global_config(c("a" = 1, "b" = 2)))
    })
  })
  describe("does_local_option_conflict_required_global_option", {
    it("returns TRUE if conflict", {
      mockery::stub(does_local_option_conflict_required_global_option, "git2r::config", list("local" = list("a" = 2)))
      expect_true(does_local_option_conflict_required_global_option("a", 1))
    })
    it("returns FALSE if no conflict", {
      mockery::stub(does_local_option_conflict_required_global_option, "git2r::config", list("local" = list("a" = 2)))
      expect_false(does_local_option_conflict_required_global_option("a", 2))
    })
  })
})


describe("is_commit_signing_already_set", {
  it("returns TRUE if everything is set", {
    mockery::stub(is_commit_signing_already_set, "git2r::config", list(
      "global" = list("gpg.program" = "gpg", "user.email" = "jd@example.com", "commit.gpgsign" = "true", "user.signingkey" = "A")
    ))
    mockery::stub(is_commit_signing_already_set, "extract_email_for_key", "jd@example.com")
    mockery::stub(is_commit_signing_already_set, "does_local_config_overwrite_global_config", FALSE)
    expect_true(is_commit_signing_already_set("A", TRUE))
  })
})