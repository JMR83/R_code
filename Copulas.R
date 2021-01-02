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



rv <- runif(10000)
rho <- 0.3

y <- qnorm(rv) %>% matrix(nrow = length(rv) / 2, ncol = 2) %>% 
    as.data.frame()

x1 <- y[['V1']]
x2 <- rho * y[['V1']] + (1 - rho^2)^0.5 * y[['V2']]

cov(x1, x2)
var(x1)
var(x2)

x <- data.frame(x1, x2) %>% sapply(pnorm)

c1 <- -log(1 - x[, 1])
c2 <- -log(1 - x[, 2])

c <- data.frame(c1, c2)


ggplot2::qplot(x = c1, y = c2)
plot(c1, c2)

ggplot2::qplot(x = x1, y = x2)
plot(x1, x2)


