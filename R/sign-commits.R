#' GPG sign all git commits
#'
#' Configure git to sign all commits using GPG. If no existing key is provided,
#' `sign_commits_with_key()` will create a new key and use it for signing.
#'
#' @param name A character string containing your name. If not provided,
#'   `sign_commits_with_key()` will look in your global git configuration.
#' @param email A character string containing your email address. If not
#'   provided, `sign_commits_with_key()` will look in your global git
#'   configuration.
#' @param passphrase An optional passphrase to protect the keypair.
#' @param key A character string containing the ID of a pre-existing key to use.
#'   If `NULL`, a new key will be created.
#' @return A character string containing the ID of the key that was provided or
#'   generated.
#' @export
#'
#' @examples
#' \dontrun{
#' newkey <- sign_commits_with_key("John Doe", "johndoe@example.com")
#' }
sign_commits_with_key <- function(name, email, passphrase = NULL, key = NULL) {

  if (missing(name)) {
    name <- extract_git_option("user.name")
  }

  if (missing(email)) {
    email <- extract_git_option("user.email")
  }

  if (is.null(name) | is.null(email)) {
    stop(
      "Name and email are required and not provided or available in the user's git config.",
      call. = FALSE
    )
  }

  if (is.null(key)) {
    key <- gpg::gpg_keygen(
      name = name,
      email = email,
      passphrase = passphrase
    )
  }

  git2r::config(global = TRUE, user.signingkey = key, commit.gpgsign = "true")

  return(key)

}

#' Add a public key to your GitHub account
#'
#' `gh_store_key()` add the public key associated with a key ID to your GitHub
#' account. If you have a GitHub Personal Access Token it will attempt to use
#' it; if it fails, it will print the public key for you to copy manually into
#' GitHub.
#'
#' See
#' https://help.github.com/articles/creating-a-personal-access-token-for-the-command-line/
#' for more information on tokens.
#'
#' @param key A character string containing the ID of a key to use. See
#'   [gpg::gpg_list_keys()]; if you haven't created a key, see
#'   [gpg::gpg_keygen()].
#' @param .token GitHub Personal Access Token.
#' @export
#'
#' @examples
#' \dontrun{
#' newkey <- sign_commits_with_key("John Doe", "johndoe@example.com")
#' gh_store_key(newkey)
#' }
gh_store_key <- function(key, .token = NULL) {

  pubkey <- gpg::gpg_export(key)

  gh_attempt <- try(
    gh::gh("POST /user/gpg_keys", armored_public_key = pubkey, .token),
    silent = TRUE
  )

  if (inherits(gh_attempt, "try-error")) {
    message(
      paste(
        "Could not add public key to GitHub.",
        "Copy the text below and paste it at https://github.com/settings/gpg/new",
        pubkey,
        sep = "\n"
      )
    )
  }
}

extract_git_option <- function(name) {
  git_config <- git2r::config()
  if (is.null(git_config[["local"]][[name]])) {
    git_config[["global"]][[name]]
  } else {
    git_config[["local"]][[name]]
  }
}