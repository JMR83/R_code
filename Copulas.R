source(file.path("C:", "Users", "julia", "Documents", 
                 "GitHub", "R_code", "initializing.R"))

rv <- runif(10000)
rho <- 0.8

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

ggplot2::qplot(x = x1, y = x2, col = I("darkblue"))
ggplot2::qplot(x2, geom = 'histogram',
               binwidth = 0.08,
               fill = I("green"), 
               col = I("blue"))
# plot(x1, x2)
# 
# ggplot2::qplot(x = c1, y = c2)
# plot(c1, c2)



