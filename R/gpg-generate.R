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
  password_wanted <- utils::askYesNo("Do you want to protect your new gpg key with a password?")
  if (is.na(password_wanted)) {
    stop(
      "GPG key generation cancelled by user, stopping execution.",
      call. = FALSE
    )
  }
  if (password_wanted) {
    passphrase <- getPass::getPass(
      msg = paste(
        "Please enter password for new gpg key",
        "(can only be blank if you are using terminal),",
        "to cancel press `Cancel` in Rstudio or `Ctrl + c` in terminal: "
      )
    )
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
