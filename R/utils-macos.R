
get_macos_version <- function() {
  system("sw_vers -productVersion", intern=TRUE)
}


