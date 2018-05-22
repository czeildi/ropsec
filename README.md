
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

``` r
full_audit_results <- full_on_audit()
```

### Sign commits

``` r
key <- sign_commits_with_key("John Doe", "john.doe@gmail.com", "password")
gh_store_key(key)
```

<img src="man/figures/signed_commit.png" align="center"/>

Collaborators
-------------

-   Bob Rudis @hrbrmstr
-   Kara Woo @karawoo
-   Karthik Ram @karthik
-   Ildi Czeller @czeildi
