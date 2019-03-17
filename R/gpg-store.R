#' Add a public key to your GitHub or gitlab or other account
#'
#' `store_public_key()` adds the public key associated with a key ID to your
#' GitHub or Gitlab account if token is available. Otherwise it will print the
#' public key for you to copy manually into GitHub/Gitlab or elsewhere.
#'
#' If you do not have a Personal Access Token set up or you want to store your
#' key on other service you can either call this function without a token and
#' then add the printed public key manually or call `cat(`[gpg::gpg_export()]`)`
#' with `newkey` and add the returned public key manually. For more information
#' see [GitHub
#' tokens](https://help.github.com/articles/creating-a-personal-access-token-for-the-command-line/)
#' or [gitlab
#' tokens](https://docs.gitlab.com/ee/user/profile/personal_access_tokens.html).
#'
#' @param key A character string containing the ID of a key to use, use the
#'   return value of [`sign_commits_with_key()`]. If you are not sure, what key
#'   you want to use, you can list your locally available keys with
#'   [gpg::gpg_list_keys()]. The email address corresponding to this key must be
#'   identical to the email address that you use with GitHub/Gitlab etc.
#' @param service NULL currently supported services: `"gh"` for GitHub (access
#'   token is necessary) or `"gl"` for gitlab (url and access token is
#'   necessary). If left NULL, no upload is attempted, public key is returned
#'   for manual upload.
#' @param .token Either GitHub Personal Access Token with at least
#'   `write:gpg_key` scope enabled. You can grant access to tokens
#'   [here](https://github.com/settings/tokens). Supply this with `service =
#'   "gh"`. Or Gitlab personal access token with at least `api` scope enabled.
#'   Supply this with `service = "gl"`. If your PAT is stored in an environment
#'   variable `GITHUB_PAT` (for `service = "gh"`) or `GITLAB_PAT` (for `service
#'   = "gl"`) you do not need to supply the token manually.
#' @param gitlab_url NULL by default, or your gitlab url for `service = "gl"`.
#' @param open_url logical, whether open relevant URLs automatically.
#' @export
#'
#' @examples
#' \dontrun{
#' new_key <- sign_commits_with_key("John Doe", "johndoe@example.com")
#' # if you do not have personal access token for github
#' store_public_key(key = new_key)
#' # if your GitHub Personal Access Token is stored in `.Renviron` as MY_GITHUB_PAT
#' store_public_key(key = new_key, .token = Sys.getenv('MY_GITHUB_PAT'))
#' }
store_public_key <- function(key, service = NULL, .token = NULL, gitlab_url = NULL, open_url = is_interactive()) {
  pubkey <- gpg::gpg_export(key)

  if (pubkey == "") {
    stop(
      "Key of id `", key, "` is not found on local system. ",
      "You can generate one with `sign_commits_with_key`",
      call. = FALSE
    )
  }

  if (is.null(.token)) {
    communicate_pubkey_if_no_token(pubkey, open_url = open_url)
    return(invisible(pubkey))
  }

  gh_attempt <- gh_attempt_key_upload(pubkey, .token)

  if (inherits(gh_attempt, "try-error")) {
    if (inherits(attr(gh_attempt, "condition"), "http_error_422")) {
      message(crayon::green(
        clisymbols::symbol$tick, " ",
        "Public GPG key is already stored on GitHub."
      ))
    } else {
      communicate_pubkey_if_unsuccessful_upload(pubkey, open_url = open_url)
    }
  } else if (!gh_attempt$emails[[1]]$verified) {
    warning(
      crayon::red(clisymbols::symbol$warning), " ",
      "Uploaded key is unverified. ",
      "Is it possible that the email you used to generate the key and ",
      "the email you use with GitHub are different? ",
      "If so, delete the uploaded key by hand from GitHub (", crayon::underline("https://github.com/settings/keys"), ") and try again.",
      call. = FALSE
    )
    if (open_url) {
      Sys.sleep(1)
      utils::browseURL("https://github.com/settings/keys")
    }
  }
  invisible(pubkey)
}

communicate_pubkey_if_no_token <- function(pubkey, open_url) {
  communicate_pubkey_for_manual_addition(
    pubkey,
    reason = "Could not add public key to GitHub as token is not provided."
  )
  new_url <- "https://github.com/settings/gpg/new"
  if (open_url) {
    Sys.sleep(1)
    utils::browseURL(new_url)
  }
  invisible(new_url)
}

communicate_pubkey_if_unsuccessful_upload <- function(pubkey, open_url) {
  communicate_pubkey_for_manual_addition(
    pubkey,
    reason = "Could not add public key to GitHub."
  )
  new_url <- "https://github.com/settings/gpg/new"
  if (open_url) {
    Sys.sleep(1)
    utils::browseURL(new_url)
  }
  invisible(new_url)
}

communicate_pubkey_for_manual_addition <- function(pubkey, reason) {
  message(
    crayon::red(clisymbols::symbol$cross), " ", crayon::silver(reason)
  )
  new_url <- "https://github.com/settings/gpg/new"
  if (is_clipr_available()) {
    clipr::write_clip(pubkey)
    message(crayon::green(
      clisymbols::symbol$tick, "The public key is on your clipboard."
    ))
    message(
      crayon::red(clisymbols::symbol$bullet), " ",
      crayon::silver("Paste it at ", crayon::underline(new_url), ".\n")
    )
  } else {
    message(
      crayon::red(clisymbols::symbol$bullet), " ",
      crayon::silver("Copy the text below and paste it at ", crayon::underline(new_url), ".\n")
    )
    cat(pubkey)
  }
  invisible(pubkey)
}

gh_attempt_key_upload <- function(pubkey, .token) {
  try(
    gh::gh("POST /user/gpg_keys", armored_public_key = pubkey, .token = .token),
    silent = TRUE
  )
}
