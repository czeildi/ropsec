#' Add a public key to your GitHub account
#'
#' `gh_store_key()` adds the public key associated with a key ID to your GitHub
#' account. If you have a GitHub Personal Access Token it will attempt to use
#' it; if it fails, it will print the public key for you to copy manually into
#' GitHub.
#'
#' If you do not have a GitHub Personal Access Token setup or you want to store
#' your key on Gitlab or other service you can either call this function without
#' a token and then add the printed public key manually or call
#' `cat(`[gpg::gpg_export()]`)` with `newkey` and add the returned public key
#' manually. See [this
#' page](https://help.github.com/articles/creating-a-personal-access-token-for-the-command-line/)
#' for more information on tokens.
#'
#' @param key A character string containing the ID of a key to use, use the
#'   return value of `sign_commits_with_key()`. If you are not sure, what key
#'   you want to use, you can list your locally available keys with
#'   [gpg::gpg_list_keys()]. The email address corresponding to this key must be
#'   identical to the email address that you use with GitHub.
#' @param .token GitHub Personal Access Token with at least `write:gpg_key`
#'   scope enabled. You can grant access to tokens
#'   [here](https://github.com/settings/tokens).
#' @param open_url boolean, whether open relevant URLs automatically.
#' @export
#'
#' @examples
#' \dontrun{
#' new_key <- sign_commits_with_key("John Doe", "johndoe@example.com")
#' # if you do not have personal access token for github
#' gh_store_key(key = new_key)
#' # if your GitHub Personal Access Token is stored in `.Renviron` as GITHUB_PAT
#' gh_store_key(key = new_key, .token = Sys.getenv('GITHUB_PAT'))
#' }
gh_store_key <- function(key, .token = NULL, open_url = interactive()) {
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
      "If so, delete the uploaded key by hand from GitHub (https://github.com/settings/keys) and try again.",
      call. = FALSE
    )
    if (open_url) {
      utils::browseURL("https://github.com/settings/keys")
    }
  }
  invisible(pubkey)
}

communicate_pubkey_if_no_token <- function(pubkey, open_url = interactive()) {
  new_url <- "https://github.com/settings/gpg/new"
  message(
    crayon::red(clisymbols::symbol$cross), " ",
    crayon::silver("Could not add public key to GitHub as token is not provided.\n"),
    crayon::red(clisymbols::symbol$bullet), " ",
    crayon::silver(
      "Copy the text below and paste it at ", crayon::underline(new_url)
    ),
    "\n"
  )
  cat(pubkey)
  if (open_url) {
    utils::browseURL(new_url)
  }
  invisible(new_url)
}

communicate_pubkey_if_unsuccessful_upload <- function(pubkey, open_url = interactive()) {
  new_url <- "https://github.com/settings/gpg/new"
  message(
    crayon::red(clisymbols::symbol$cross), " ",
    crayon::silver("Could not add public key to GitHub.\n"),
    crayon::red(clisymbols::symbol$bullet), " ",
    crayon::silver(
      "Copy the text below and paste it at ", crayon::underline(new_url)
    ),
    "\n"
  )
  cat(pubkey)
  if (open_url) {
    utils::browseURL(new_url)
  }
  invisible(new_url)
}

gh_attempt_key_upload <- function(pubkey, .token) {
  try(
    gh::gh("POST /user/gpg_keys", armored_public_key = pubkey, .token = .token),
    silent = TRUE
  )
}
