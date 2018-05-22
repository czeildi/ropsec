
ropsec
======

Personal Workstation Safety Checks and Utilities

Description
-----------

What's Inside The Tin
---------------------

The following functions are implemented:

-   `full_on_audit()`
-   `sign_commits_with_key()`
-   `gh_store_key()`
-   `summarize_system_checks()`

Installation
------------

``` r
devtools::install_github("ropenscilabs/ropsec")
```

Usage
-----

``` r
library(ropsec)

# current verison
packageVersion("ropsec")
```

    ## [1] '0.1.0'

### Audit

E.g. what ports are used.

``` r
full_audit_results <- full_on_audit()
```

### Sign commits

``` r
key <- sign_commits_with_key("John Doe", "john.doe@gmail.com", "password")
gh_store_key(key)
```

<img src="man/figures/signed_commit.png" align="center"/>

### Lightweight system checks

``` r
ropsec::summarize_system_checks()
```

    ## ✔ | OK F W S | Context
    ## 
    ⠏ |  0       | SSH Configuration - existence
    ⠋ |  1       | SSH Configuration - existence
    ✔ |  1       | SSH Configuration - existence
    ## 
    ⠏ |  0       | SSH Configuration - keys
    ⠋ |  1       | SSH Configuration - keys
    ⠙ |  2       | SSH Configuration - keys
    ✔ |  2       | SSH Configuration - keys
    ## 
    ⠏ |  0       | SSH Configuration - key size
    ⠋ |  1       | SSH Configuration - key size
    ✔ |  1       | SSH Configuration - key size
    ## 
    ⠏ |  0       | GPG Existence
    ⠋ |  1       | GPG Existence
    ✔ |  1       | GPG Existence
    ## 
    ⠏ |  0       | macOS requires password after sleep or screen saver kicks in
    ⠋ |  1       | macOS requires password after sleep or screen saver kicks in
    ✔ |  1       | macOS requires password after sleep or screen saver kicks in [0.1 s]
    ## 
    ⠏ |  0       | Firewall is enabled
    ⠋ |  0 1     | Firewall is enabled
    ✖ |  0 1     | Firewall is enabled
    ## ──────────────────────────────────────────────────────────────────────────────────────────────
    ## macos-simple-test.R:35: failure: firewall is on
    ## any(grepl("Firewall is enabled", x)) isn't true.
    ## ──────────────────────────────────────────────────────────────────────────────────────────────
    ## 
    ⠏ |  0       | Gatekeeper is enabled
    ⠋ |  1       | Gatekeeper is enabled
    ✔ |  1       | Gatekeeper is enabled
    ## 
    ⠏ |  0       | ~/.Rprofile permissions are sane (if ~/.Rprofile exists)
    ⠋ |  0     1 | ~/.Rprofile permissions are sane (if ~/.Rprofile exists)
    ✔ |  0     1 | ~/.Rprofile permissions are sane (if ~/.Rprofile exists)
    ## ──────────────────────────────────────────────────────────────────────────────────────────────
    ## macos-simple-test.R:45: skip: ~/.Rprofile permissions are sane
    ## Empty test
    ## ──────────────────────────────────────────────────────────────────────────────────────────────
    ## 
    ⠏ |  0       | ~/.Renviron permissions are sane (if ~/.Renviron exists)
    ⠋ |  0 1     | ~/.Renviron permissions are sane (if ~/.Renviron exists)
    ✖ |  0 1     | ~/.Renviron permissions are sane (if ~/.Renviron exists)
    ## ──────────────────────────────────────────────────────────────────────────────────────────────
    ## macos-simple-test.R:54: failure: ~/.Renviron permissions are sane
    ## ==... isn't true.
    ## ──────────────────────────────────────────────────────────────────────────────────────────────
    ## 
    ## ══ Results ═══════════════════════════════════════════════════════════════════════════════════
    ## Duration: 0.3 s
    ## 
    ## OK:       7
    ## Failed:   2
    ## Warnings: 0
    ## Skipped:  1

Collaborators
-------------

-   Bob Rudis @hrbrmstr
-   Kara Woo @karawoo
-   Karthik Ram @karthik
-   Ildi Czeller @czeildi
