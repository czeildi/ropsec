ARG R_VERSION

FROM rocker/r-ver:$R_VERSION

RUN apt-get update

RUN apt-get install -y libssl-dev \
  zlib1g-dev \
  libssl-dev \
  libxml2-dev \
  libmariadb2 \
  libmariadb-client-lgpl-dev \
  libpq-dev \
  pandoc \
  pandoc-citeproc \
  qpdf

RUN apt-get install -y libcurl4-openssl-dev

RUN install2.r --error \
  boot \
  cluster \
  class \
  codetools \
  foreign \
  KernSmooth \
  lattice \
  MASS \
  Matrix \
  mgcv \
  nlme \
  nnet \
  rpart \
  spatial \
  survival

# package dev dependencies
RUN R -e 'install.packages("devtools")'
RUN R -e 'install.packages("rcmdcheck")'
RUN R -e 'install.packages("testthat")'
RUN R -e 'install.packages("roxygen2")'
RUN R -e 'install.packages("knitr")'
RUN R -e 'install.packages("pkgdown")'
RUN R -e 'install.packages("styler")'

# package dependencies

RUN apt install -y libgpgme11-dev

RUN R -e 'install.packages("git2r")'
RUN R -e 'install.packages("gpg")'
RUN R -e 'install.packages("stringi")'
RUN R -e 'install.packages("openssl")'
RUN R -e 'install.packages("sys")'
RUN R -e 'install.packages("getPass")'
RUN R -e 'install.packages("crayon")'
RUN R -e 'install.packages("clisymbols")'
RUN R -e 'install.packages("clipr")'

RUN mkdir /rpkg
WORKDIR /rpkg

ADD . /rpkg