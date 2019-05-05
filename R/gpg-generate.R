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
  password_wanted <- ask_yes_no(
    "Do you want to protect your new gpg key with a password? Password protection is strongly recommended unless you are only testing."
  )
  if (is.na(password_wanted)) {
    stop(
      "GPG key generation cancelled by user, stopping execution.",
      call. = FALSE
    )
  }
  if (password_wanted) {
    passphrase <- getPass::getPass(
      msg = paste(
        "Please enter password for new gpg key,",
        "to cancel press `Cancel` in Rstudio or `Ctrl + c` in terminal: "
      )
    )
    if (!is.null(passphrase) && passphrase == "") {
      stop(paste(
        "You opted for password protection for your GPG key.\n",
        "Either provide a non-empty password or start again and opt for no password protection.",
        call. = FALSE
      ))
    }
  } else {
    passphrase <- ""
  }
  if (is.null(passphrase)) {
    stop(
      "GPG key generation cancelled by user, stopping execution.",
      call. = FALSE
    )
  }
  if (passphrase == "") {
    passphrase <- NULL
  }
  generate_key_with_checked_params(name, email, passphrase)
}

generate_key_with_checked_params <- function(name, email, passphrase) {
  key <- gpg::gpg_keygen(
    name = name,
    email = email,
    passphrase = passphrase
  )
  cat(
    crayon::green(
      clisymbols::symbol$tick,
      "Key with id `" %+% key %+% "` successfully generated.\n"
    )
  )
  key
}


ask_yes_no <- function(question) {
  if (!interactive()) {
    stop("User input required in non-interactive session.\n", call. = FALSE)
  }
  choices <- c("Yes" = TRUE, "no" = FALSE, "cancel" = NA)
  unname(choices[utils::menu(names(choices), title = question)])
}