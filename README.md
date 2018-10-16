
![](./man/figures/Loose_lips_might_sink_ships.jpg)

rOpenSci Unconf 18 Project : ropsec
===================================

[![Travis build status](https://travis-ci.org/ropenscilabs/ropsec.svg?branch=master)](https://travis-ci.org/ropenscilabs/ropsec) [![AppVeyor build status](https://ci.appveyor.com/api/projects/status/55vx8b5jckpa216a?svg=true)](https://ci.appveyor.com/project/czeildi/ropsec-w5fnj) [![Coverage status](https://codecov.io/gh/ropenscilabs/ropsec/branch/master/graph/badge.svg)](https://codecov.io/github/ropenscilabs/ropsec?branch=master)

Personal Workstation Safety Checks and Utilities

What's Inside The Tin
---------------------

The following functions are implemented:

-   [`sign_commits_with_key()`](#sign_commits) [![Lifecycle Status](https://img.shields.io/badge/lifecycle-maturing-blue.svg)](https://www.tidyverse.org/lifecycle/)
-   [`gh_store_key()`](#sign_commits) [![Lifecycle Status](https://img.shields.io/badge/lifecycle-maturing-blue.svg)](https://www.tidyverse.org/lifecycle/)
-   [`summarize_system_checks()`](#system_checks) [![Lifecycle Status](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://www.tidyverse.org/lifecycle/)
-   [`full_on_audit()`](#full_on_audit) [![Lifecycle Status](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://www.tidyverse.org/lifecycle/)

Installation
------------

``` r
devtools::install_github("ropenscilabs/ropsec")
```

To have vignettes available locally:

``` r
devtools::install_github("ropenscilabs/ropsec", build_vignettes = TRUE)
```

Usage
-----

``` r
library(ropsec)

# current verison
packageVersion("ropsec")
```

    ## [1] '0.2.0'

### Sign commits

For details see `vignette("sign-commits", "ropsec")`

``` r
key <- sign_commits_with_key("John Doe", "john.doe@gmail.com")
gh_store_key(key)
```

<img src="man/figures/signed_commit.png" align="center"/>

### Lightweight system checks

``` r
ropsec::summarize_system_checks()
```

    ✔ | OK F W S | Context
    ✔ |  1       | SSH Configuration - existence
    ✔ |  2       | SSH Configuration - keys
    ✔ |  1       | SSH Configuration - key size
    ✔ |  1       | GPG Existence
    ✔ |  1       | macOS requires password after sleep or screen saver kicks in [0.1 s]
    ✖ |  0 1     | Firewall is enabled
    # ...

### Audit

E.g. what ports are used.

``` r
full_audit_results <- full_on_audit()
```

Collaborators
-------------

-   Bob Rudis @hrbrmstr
-   Kara Woo @karawoo
-   Karthik Ram @karthik
-   Ildi Czeller @czeildi
