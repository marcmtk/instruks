library(assertthat) # Skift til assertive? #https://cran.r-project.org/web/packages/assertive/index.html # https://cran.r-project.org/web/packages/assertive/vignettes/checklists.html
library(testthat)
library(tibble)
library(dplyr)
library(tidyr)
library(glue)
library(forcats)
library(purrr)

`%notin%` <- Negate(`%in%`)
source("R/generate_lookup_tables.R")
