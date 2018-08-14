#' GPG sign all git commits
#'
#' Configure git to sign all commits using GPG. If no existing key is provided,
#' `sign_commits_with_key()` will create a new key and use it for signing.
#'
#' In case of already existing key(s) for convenience an appropriate key will be
#' identified based on git config, or if git config is not set, it is sufficient
#' to provide one of name or email. This is especially handy if you have
#' multiple email addresses used with git and thus would like to set-up commit
#' signing on a per-repo basis. In this case supply the email and set the global
#' param to `FALSE`. If you accidentally set that all commits should be signed
#' you can revert this by deleting the `commit.gpgsign` and `user.signingkey` git
#' options.
#'
#'
#' @param name A character string containing your name. If not provided,
#'   `sign_commits_with_key()` will look first in your local then in your global
#'   git configuration.
#' @param email A character string containing your email address. If not
#'   provided, `sign_commits_with_key()` will look first in your local then in
#'   your global git configuration.
#' @param key A character string containing the ID of a pre-existing key to use.
#'   If `NULL` and key cannot be found based on name and email unambiguously, a
#'   new key will be created.
#' @param global boolean, set commit signing in global or local git config.
#' @return A character string containing the ID of the key that was provided or
#'   generated.
#' @export
#'
#' @examples
#' \dontrun{
#' newkey <- sign_commits_with_key("John Doe", "johndoe@example.com")
#' }
sign_commits_with_key <- function(name, email, key = NULL, global = TRUE) {
  if (!is.null(key)) {
    git2r::config(global = TRUE, user.signingkey = key, commit.gpgsign = "true")
    return(key)
  }

  if (missing(name)) {
    name <- extract_git_option("user.name")
  }
  if (missing(email)) {
    email <- extract_git_option("user.email")
  }

  key_candidates <- get_key_candidates(name, email)

  if (nrow(key_candidates) == 0L) {
    key <- generate_key_with_name_and_email(name, email)
  } else if (nrow(key_candidates) == 1L) {
    message("Existing key found and will be used to sign commits.")
    key <- key_candidates$id
  } else {
    message(paste0(utils::capture.output(key_candidates), collapse = "\n"))
    stop(
      "There are multiple keys, you must disambiguate with providing the key param.",
      call. = FALSE
    )
  }

  git2r::config(global = global, user.signingkey = key, commit.gpgsign = "true")

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

get_key_candidates <- function(user_name, user_email) {
  existing_keys <- gpg::gpg_list_keys()
  if (is.null(user_name) | is.null(user_email)) {
    subset(
      existing_keys,
      existing_keys$name == user_name | existing_keys$email == user_email
    )
  } else {
    subset(
      existing_keys,
      existing_keys$name == user_name & existing_keys$email == user_email
    )
  }
}

generate_key_with_name_and_email <- function(name, email) {
  if (is.null(name) | is.null(email)) {
    stop(
      "Name and email are required to generate a gpg key and are not provided or available in the user's git config.",
      call. = FALSE
    )
  }
  passphrase <- readline(
    prompt = "Please enter password for new gpg key (can be blank): "
  )
  gpg::gpg_keygen(
    name = name,
    email = email,
    passphrase = passphrase
  )
}
