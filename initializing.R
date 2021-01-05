rm(list = ls())

#### Settings ----
options(continue = "  ", digits = 7, max.print = 20, 
        prompt = "R> ", stringsAsFactors = FALSE, warn = 0)

#### load Packages ----
pkgs <- c("compiler", "ChainLadder", "DBI", "doParallel", 
          "dplyr", "foreach", "iterators", "lattice",
          "lubridate", "microbenchmark", "odbc", "parallel", 
          "purrr", "RODBC", "testthat", "tidyverse")

lapply(pkgs, FUN = library, character.only = TRUE)

#### Assigning lubridate functions in order to avoid conflict with data.table package

day <- lubridate::day
month <- lubridate::month
year <- lubridate::year
rename <- dplyr::rename


R.Version()
Sys.info()
devtools::session_info()
