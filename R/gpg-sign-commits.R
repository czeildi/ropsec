#' GPG sign all git commits and/or retrieve ID of existing key
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
#'   your global git configuration. The email address must be identical to the
#'   one you use with the corresponding repository management service (i.e.
#'   GitHub, etc).
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
      crayon::green(clisymbols::symbol$tick), " ",
      crayon::silver(
        "Existing key found: `" %+% key %+% "`.\n" %+%
        "Corresponding email: `" %+% email %+% "`" %+% communicate_source_of_param(email) %+% ".\n"
      )
    )
  } else {
    stop_due_to_multiple_keys(key_candidates)
  }

  set_key_to_sign_commits(key, global)
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
    message(
      crayon::red(clisymbols::symbol$bullet), " ",
      crayon::silver(
        "The next step is uploading the public key",
        "to GitHub or alternative (unless it is already uploaded),",
        " which you can do by passing",
        "the return value (\"" %+% key %+% "\") to `gh_store_key`."
      )
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

stop_due_to_multiple_keys <- function(key_candidates) {
  stop(
    paste0(utils::capture.output(key_candidates), collapse = "\n"), "\n",
    crayon::red(clisymbols::symbol$cross), " ",
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
    "Do you want to sign future commits with `", key, "` in ",
    crayon::bold(ifelse(global, "all repositories?", "this repository?")), "\n",
    ifelse(
      original_git_user_email != new_git_user_email,
      paste0(
        crayon::red(clisymbols::symbol$warning), " ",
        "This will also set your user.email from `",
        original_git_user_email, "` to `", new_git_user_email, "`.\n"
      ),
      ""
    )
  )
}
