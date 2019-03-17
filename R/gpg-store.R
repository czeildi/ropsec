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
store_public_key <- function(key, service = NULL, .token = NULL,
                             gitlab_url = NULL, open_url = is_interactive()) {
  pubkey <- gpg::gpg_export(key)
  if (pubkey == "") communicate_no_key(key)
  email_for_key <- extract_email_for_key(key)

  if (is.null(service)) {
    communicate_pubkey_wo_service(pubkey)
    return(invisible(pubkey))
  }
  stopifnot(service %in% c("gh", "gl"))

  if (is.null(.token)) {
    .token <- Sys.getenv(ifelse(service == "gh", "GITHUB_PAT", "GITLAB_PAT"), unset = NULL)
  }

  if (is.null(.token)) {
    communicate_pubkey_for_manual_addition(
      pubkey, service, gitlab_url, open_url, reason = "Token is not provided."
    )
    return(invisible(pubkey))
  }

  if (service == "gl" && is.null(gitlab_url)) {
    communicate_pubkey_for_manual_addition(
      pubkey, service = "gl", gitlab_url, open_url, reason = "No gitlab url provided."
    )
    return(invisible(pubkey))
  }

  if (service == "gh") {
    gh_store_key(pubkey, .token, open_url, email_for_key)
  } else {
    gl_store_key(pubkey, .token, gitlab_url, open_url, email_for_key)
  }
}

gh_store_key <- function(pubkey, .token, open_url, email_for_key) {
  gh_attempt <- gh_attempt_key_upload(pubkey, .token)

  if (inherits(gh_attempt, "try-error")) {
    if (inherits(attr(gh_attempt, "condition"), "http_error_422")) {
      message(crayon::green(clisymbols::symbol$tick, " ", "Public GPG key is already stored on GitHub."))
    } else if (inherits(attr(gh_attempt, "condition"), "http_error_401")) {
      communicate_pubkey_for_manual_addition(
        pubkey, service = "gh", open_url = open_url, reason = "Unauthorized request. Check your token."
      )
    }
    else {
      communicate_pubkey_for_manual_addition(
        pubkey, service = "gh", open_url = open_url, reason = "Unknown reason."
      )
    }
  } else if (!gh_attempt$emails[[1]]$verified) {
    communicate_unverified_key("gh", email_for_key, open_url)
  }
  invisible(pubkey)
}

gl_store_key <- function(pubkey, .token, gitlab_url, open_url, email_for_key) {
  gl_attempt <- gl_attempt_key_upload(pubkey, .token, gitlab_url)
  status_code <- httr::status_code(gl_attempt)
  if (status_code == 201) {
    gl_email <- gl_user_email(gitlab_url, .token)
    if (isTRUE(gl_email == email_for_key)) {
      message(crayon::green(clisymbols::symbol$tick, " ", "Public GPG key is successfully stored on Gitlab."))
      return(invisible(pubkey))
    } else {
      communicate_unverified_key("gl", email_for_key, open_url, gitlab_url, gl_email)
    }
  } else if (status_code == 401) {
    communicate_pubkey_for_manual_addition(
      pubkey, service = "gl", gitlab_url, open_url = open_url, "Unauthorized request. Check your token."
    )
  } else if (status_code == 400) {
    response_content <- httr::content(gl_attempt)
    if (isTRUE(purrr::pluck(httr::content(gl_attempt), "message", "key", 1) == "has already been taken")) {
      message(crayon::green(clisymbols::symbol$tick, " ", "Public GPG key is already stored on Gitlab."))
      return(invisible(pubkey))
    }
  }
  communicate_pubkey_for_manual_addition(
    pubkey, service = "gl", gitlab_url, open_url = open_url, "Unknown reason."
  )
  invisible(pubkey)
}

communicate_pubkey_for_manual_addition <- function(pubkey, service, gitlab_url = NULL,
                                                   open_url, reason) {
  message(
    crayon::red(clisymbols::symbol$cross), " ", crayon::silver(reason)
  )
  if (service == "gh") {
    new_url <- "https://github.com/settings/gpg/new"
  } else {
    new_url <- paste0(gitlab_url, "/profile/gpg_keys")
  }
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
  if (open_url) {
    Sys.sleep(1)
    utils::browseURL(new_url)
  }
  invisible(pubkey)
}

communicate_pubkey_wo_service <- function(pubkey) {
  message(
    crayon::red(clisymbols::symbol$cross), " ",
    crayon::silver("Could not automatically upload public key as no service is provided. (`'gh'` or `'gl'` is available)")
  )
  if (is_clipr_available()) {
    clipr::write_clip(pubkey)
    message(crayon::green(
      clisymbols::symbol$tick, "The public key is on your clipboard."
    ))
  } else {
    message(
      crayon::red(clisymbols::symbol$bullet), " ",
      crayon::silver("The public key is the text below.\n")
    )
    cat(pubkey)
  }
  invisible(pubkey)
}

communicate_no_key <- function(key) {
  stop(
    "Key of id `", key, "` is not found on local system. ",
    "You can generate one with `sign_commits_with_key`",
    call. = FALSE
  )
}
communicate_unverified_key <- function(service, email_for_key, open_url,
                                       gitlab_url = NULL, gl_email = NULL) {
  if (service == "gh") {
    delete_url <- "https://github.com/settings/keys"
  } else {
    delete_url <- paste0(gitlab_url, "/profile/gpg_keys")
  }
  if (service == "gh") {
    specific_warning_message <- paste0(
      "Is it possible that the email you used to generate the key (`", email_for_key,
      "`) and the email you use with GitHub are different? "
    )
  } else {
    specific_warning_message <- paste0(
      "the email you used to generate the key (`", email_for_key,
      "`) and the email you use with Gitlab (`", gl_email, "`) are different."
    )
  }
  warning(
    crayon::red(clisymbols::symbol$warning), " Uploaded key is unverified. ",
    "Delete the uploaded key by hand (", crayon::underline(delete_url), ") and try again.",
    call. = FALSE
  )
  if (open_url) {
    Sys.sleep(1)
    utils::browseURL(delete_url)
  }
  invisible(delete_url)
}

gh_attempt_key_upload <- function(pubkey, .token) {
  try(
    gh::gh("POST /user/gpg_keys", armored_public_key = pubkey, .token = .token),
    silent = TRUE
  )
}

gl_attempt_key_upload <- function(pubkey, .token, gitlab_url) {
  httr::POST(
    url = paste0(gitlab_url, "/api/v4/user/gpg_keys"),
    config = httr::add_headers(
      "PRIVATE-TOKEN" = .token,
      "Content-Type" = "application/json"
    ),
    body = jsonlite::toJSON(list("key" = pubkey), auto_unbox = TRUE),
    encode = "json"
  )
}

gl_user_email <- function(url, .token) {
  user <- httr::GET(
    url = paste0(url, "/api/v4/user"),
    config = httr::add_headers(
      "PRIVATE-TOKEN" = .token,
      "Content-Type" = "application/json"
    )
  )
  if (httr::status_code(user) == 200) {
    httr::content(user)$email
  } else {
    warning("Could not fetch user email from gitlab API, status code:", httr::status_code(user))
    NA
  }
}
