#' GPG sign all git commits
#'
#' Configure git to sign all commits using GPG. If no existing key is provided
#' or found, `sign_commits_with_key()` will create a new key and use it for
#' signing. This function is suitable only for interactive use, before changing
#' the user's git config it will always ask for confirmation.
#'
#' In case of already existing key(s) for convenience an appropriate key will be
#' identified based on the provided name and/or email, or git config. This is
#' especially handy if you have multiple email addresses used with git and thus
#' would like to set-up commit signing on a per-repository basis. In this case
#' supply the email and set the global param to `FALSE`. If you accidentally set
#' that all commits should be signed you can revert this by deleting the
#' `commit.gpgsign` and `user.signingkey` git options.
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
#' @param global boolean, set commit signing in global or local (repository) git
#'   config.
#' @return A character string containing the ID of the key that was provided or
#'   generated.
#' @export
#'
#' @examples
#' \dontrun{
#' # generate key and use newly generated key
#' newkey <- sign_commits_with_key("John Doe", "johndoe@example.com")
#'
#' # use existing key by explicitly providing it
#' sign_commits_with_key(key = "test_key")
#'
#' # use existing key based on email match
#' sign_commits_with_key(email = "johndoe@example.com")
#'
#' # set key to sign commits only in current repository
#' sign_commits_with_key(key = "test_key", global = FALSE)
#' }
sign_commits_with_key <- function(name, email, key = NULL, global = TRUE) {
  if (!is.null(key)) {
    return(set_key_to_sign_commits(key, global))
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
    key <- key_candidates$id
    message(
      "Existing key found: `", key, "`.\n",
      "Corresponding email: `", email, "`", communicate_source_of_param(email), ".\n"
    )
  } else {
    stop_due_to_multiple_keys(key_candidates)
  }

  set_key_to_sign_commits(key, global)
}

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
#' [gpg::gpg_export()] with `newkey` and add the returned public key manually.
#' See
#' https://help.github.com/articles/creating-a-personal-access-token-for-the-command-line/
#' for more information on tokens.
#'
#' @param key A character string containing the ID of a key to use. See
#'   [gpg::gpg_list_keys()]; if you haven't created a key, see
#'   [sign_commits_with_key()] or [gpg::gpg_keygen()].
#' @param .token GitHub Personal Access Token with at least `write:gpg_key`
#'   scope enabled. You can grant access to tokens
#'   [here](https://github.com/settings/tokens).
#' @export
#'
#' @examples
#' \dontrun{
#' newkey <- sign_commits_with_key("John Doe", "johndoe@example.com")
#' # if you do not have personal access token for github
#' gh_store_key(newkey)
#' # if your GitHub Personal Access Token is stored in `.Renviron` as GITHUB_PAT
#' gh_store_key(newkey, Sys.getenv('GITHUB_PAT'))
#' }
gh_store_key <- function(key, .token = NULL) {
  pubkey <- gpg::gpg_export(key)
  if (is.null(.token)) {
    message(
      paste(
        "Could not add public key to GitHub as token is not provided.",
        "Copy the text below and paste it at https://github.com/settings/gpg/new\n",
        pubkey,
        sep = "\n"
      )
    )
    return(invisible(pubkey))
  }
  gh_attempt <- gh_attempt_key_upload(pubkey, .token)

  if (inherits(gh_attempt, "try-error")) {
    if (inherits(attr(gh_attempt, "condition"), "http_error_422")) {
      message("Public GPG key is already stored on GitHub.")
    } else {
      message(
        paste(
          "Could not add public key to GitHub.",
          "Copy the text below and paste it at https://github.com/settings/gpg/new",
          pubkey,
          sep = "\n"
        )
      )
    }
  } else if (!gh_attempt$emails[[1]]$verified) {
    warning(
      "Uploaded key in unverified. ",
      "Is it possible that the email you used to generate the key and ",
      "the email you use with GitHub are different? ",
      "If so, delete the uploaded key by hand from GitHub (https://github.com/settings/keys) and try again.",
      call. = FALSE
    )
  }
  invisible(pubkey)
}

gh_attempt_key_upload <- function(pubkey, .token) {
  try(
    gh::gh("POST /user/gpg_keys", armored_public_key = pubkey, .token = .token),
    silent = TRUE
  )
}

set_key_to_sign_commits <- function(key, global) {
  if (is.null(extract_git_option("gpg.program"))) {
    git2r::config(
      global = TRUE, gpg.program = "gpg"
    )
  }
  confirmation_message <- assemble_confirmation_message(key, global)
  if (require_confirmation_from_user(confirmation_message)) {
    new_git_user_email <- extract_email_for_key(key)
    git2r::config(
      global = global,
      user.signingkey = key,
      commit.gpgsign = "true",
      user.email = new_git_user_email
    )
    return(key)
  }
  invisible(NULL)
}

extract_email_for_key <- function(key) {
  keys <- gpg::gpg_list_keys()
  subset(keys, keys$id == key)$email
}

extract_git_option <- function(name, allow_global = TRUE) {
  git_config <- git2r::config()
  if (!is.null(git_config[["local"]][[name]])) {
    value <- git_config[["local"]][[name]]
    attr(value, "local") <- TRUE
  } else if (allow_global == TRUE) {
    value <- git_config[["global"]][[name]]
    if (!is.null(value)) {
      attr(value, "local") <- FALSE
    }
  } else {
    value <- NULL
  }
  value
}

get_key_candidates <- function(user_name, user_email) {
  existing_keys <- gpg::gpg_list_keys()
  filter_keys_on_name_and_email_if_provided(
    existing_keys, user_name, user_email
  )
}

filter_keys_on_name_and_email_if_provided <- function(keys, user_name, user_email) {
  if (is.null(user_name)) {
    subset(keys, keys$email == user_email)
  } else if (is.null(user_email)) {
    subset(keys, keys$name == user_name)
  } else {
    subset(keys, keys$name == user_name & keys$email == user_email)
  }
}

generate_key_with_name_and_email <- function(name, email) {
  if (is.null(name) | is.null(email)) {
    stop(
      "Name and email are required to generate a gpg key ",
      "and are neither provided nor available in the user's git config.",
      call. = FALSE
    )
  }
  message(
    "`", name, "`", communicate_source_of_param(name), " and\n",
    "`", email, "`", communicate_source_of_param(email), "\n",
    "will be used to generate a new gpg key."
  )
  passphrase <- getPass::getPass(
    msg = paste(
      "Please enter password for new gpg key",
      "(can only be blank if you are using terminal),",
      "to cancel press `Cancel` in Rstudio on `Ctrl + c` in terminal: "
    )
  )
  if (is.null(passphrase)) {
    stop(
      "GPG key generation cancelled by user, stopping execution.",
      call. = FALSE
    )
  }
  if (passphrase == "") {
    passphrase <- NULL
  }
  gpg::gpg_keygen(
    name = name,
    email = email,
    passphrase = passphrase
  )
}

stop_due_to_multiple_keys <- function(key_candidates) {
  stop(
    paste0(utils::capture.output(key_candidates), collapse = "\n"),
    "There are multiple keys,\n",
    "you must disambiguate with providing the key param or ",
    "deleting the keys you do not want to use.",
    call. = FALSE
  )
}

communicate_source_of_param <- function(param) {
  if (is.null(attr(param, "local", exact = TRUE))) {
    source <- " (as provided)"
  } else {
    if (attr(param, "local", exact = TRUE)) {
      source <- " (based on local git config)"
    } else {
      source <- " (based on global git config)"
    }
  }
  source
}

require_confirmation_from_user <- function(message) {
  if (!interactive()) {
    stop("User input required in non-interactive session.\n", call. = FALSE)
  }
  qs <- c("Yes", "Not now", "Absolutely not")
  rand <- sample(length(qs))
  ret <- rand == 1

  cat(message)
  ret[utils::menu(qs[rand])]
}

assemble_confirmation_message <- function(key, global) {
  new_git_user_email <- extract_email_for_key(key)
  original_git_user_email <- extract_git_option("user.email", allow_global = global)
  if (is.null(original_git_user_email)) {
    original_git_user_email <- ""
  }
  paste0(
    "Do you want to sign all future commits with `", key, "` in ",
    ifelse(global, "all repositories?", "this repository?"), "\n",
    ifelse(
      original_git_user_email != new_git_user_email,
      paste0(
        "This will also set your user.email from `",
        original_git_user_email, "` to `", new_git_user_email, "`.\n"
      ),
      ""
    )
  )
}
