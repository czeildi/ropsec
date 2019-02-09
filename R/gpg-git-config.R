does_local_config_overwrite_global_config <- function() {
  any(sapply(
    c("gpg.program", "user.email", "commit.gpgsign", "user.signingkey"),
    does_local_option_overwrite_global_option
  ))
}

does_local_option_overwrite_global_option <- function(name) {
  isTRUE(git2r::config()[["global"]][[name]] != git2r::config()[["local"]][[name]])
}

does_local_config_conflict_required_global_config <- function(required_git_config) {
  any(sapply(
    names(required_git_config),
    function(name) does_local_option_conflict_required_global_option(name, required_git_config[[name]])
  ))
}

does_local_option_conflict_required_global_option <- function(name, required_value) {
  local_value <- git2r::config()[["local"]][[name]]
  if (isTRUE(required_value != local_value)) {
    message(crayon::yellow(
      "Your local git config for ",name, "(", local_value, ")",
      "is in conflict with your required global config:", required_value
    ))
    return(TRUE)
  }
  return(FALSE)
}

is_commit_signing_already_set <- function(key, global) {
  existing_config <- git2r::config()[[ifelse(global, "global", "local")]]
  if (is.null(existing_config[["gpg.program"]])) {
    return(FALSE)
  }
  if (!isTRUE(existing_config[["user.email"]] == extract_email_for_key(key))) {
    return(FALSE)
  }
  if (!isTRUE(existing_config[["commit.gpgsign"]] == "true")) {
    return(FALSE)
  }
  if (!isTRUE(existing_config[["user.signingkey"]] == key)) {
    return(FALSE)
  }
  if (global && does_local_config_overwrite_global_config()) {
    return(FALSE)
  }
  TRUE
}

extract_git_option <- function(name, global) {
  git2r::config()[[ifelse(global, "global", "local")]][[name]]
}

extract_email_for_key <- function(key) {
  keys <- gpg::gpg_list_keys()
  subset(keys, keys$id == key)$email
}

find_git_option <- function(name) {
  git_config <- git2r::config()
  if (!is.null(git_config[["local"]][[name]])) {
    value <- git_config[["local"]][[name]]
    attr(value, "local") <- TRUE
  } else {
    value <- git_config[["global"]][[name]]
    if (!is.null(value)) {
      attr(value, "local") <- FALSE
    }
  }
  value
}
