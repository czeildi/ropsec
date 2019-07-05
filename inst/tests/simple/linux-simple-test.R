# sw_vers -productVersion
context("SSH Configuration - existence")
test_that("ssh directory exists", {
  expect_true(file.exists(path.expand("~/.ssh")))
})

context("SSH Configuration - keys")
test_that("ssh has public/private keys", {
  expect_true(file.exists(path.expand("~/.ssh/id_rsa")))
  expect_true(file.exists(path.expand("~/.ssh/id_rsa.pub")))
})

context("SSH Configuration - key size")
test_that("ssh key size is appropriate", {
  if (file.exists(path.expand("~/.ssh/id_rsa.pub"))) {
    x <- openssl::read_pubkey("~/.ssh/id_rsa.pub")
    expect_true(x$size >= 2048, label = sprintf("Key size of [%s] is below recommended value", x$size))
  }
})

context("GPG Existence")
test_that("gpg is configured", {
  expect_true(length(gpg::gpg_info()) > 0)
})

# context("Firewall is enabled")
# test_that("firewall is on", {
#   x <- system("/usr/libexec/ApplicationFirewall/socketfilterfw --getglobalstate", intern = TRUE)
#   expect_true(any(grepl("Firewall is enabled", x)))
# })
#
# context("Gatekeeper is enabled")
# test_that("gatekeeer is on", {
#   x <- system("spctl --status", intern = TRUE)
#   expect_true(any(grepl("assessments enabled", x)))
# })

context("~/.Rprofile permissions are sane (if ~/.Rprofile exists)")
test_that("~/.Rprofile permissions are sane", {
  if (file.exists(path.expand("~/.Rprofile"))) {
    expect_true(file.info(path.expand("~/.Rprofile"))$mode == structure(416L, class = "octmode"))
  }
})

context("~/.Renviron permissions are sane (if ~/.Renviron exists)")
test_that("~/.Renviron permissions are sane", {
  if (file.exists(path.expand("~/.Renviron"))) {
    expect_true(file.info(path.expand("~/.Renviron"))$mode == structure(416L, class = "octmode"))
  }
})


#https://technology.siprep.org/askforpassword-and-askforpassworddelay-in-macos-10-13-high-sierra/

