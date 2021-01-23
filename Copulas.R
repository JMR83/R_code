source(file.path("C:", "Users", "julia", "Documents", 
                 "GitHub", "R_code", "initializing.R"))

rv <- runif(10000)
rho <- 0.8

y <- qnorm(rv) %>% matrix(nrow = length(rv) / 2) %>% 
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

ggplot2::qplot(x = x1, y = x2, col = I("darkblue"))
ggplot2::qplot(x2, geom = 'histogram',
               binwidth = 0.08,
               fill = I("green"), 
               col = I("blue"))
# plot(x1, x2)
# 
# ggplot2::qplot(x = c1, y = c2)
# plot(c1, c2)


# d <- runif(10000) %>% qnorm(d)
# 
# correlation_data <- c(1, 0.5, 0.2, 0.5, 1, 0.4, 0.2, 0.4, 1)
# no_rows <- length(correlation_data) %>% sqrt()
# sigma <- matrix(correlation_data, nrow = no_rows)
# 
# l_11 <- Sigma[1, 1]
# l_21 <- Sigma[2, 1]
# l_22 <- (1 - Sigma[2, 1]^2)^0.5
# l_31 <- Sigma[3, 1]
# l_32 <- (Sigma[3, 2] - l_31 * l_21) / l_22
# l_33 <- (1 - l_31^2 - l_32^2)^0.5
# 
# 
# L <- matrix(0, no_rows, no_rows)
# L[lower.tri(L, diag = T)] <- c(l_11, l_21, l_22, l_31, l_32, l_33)
# 
# L %*% t(L)

