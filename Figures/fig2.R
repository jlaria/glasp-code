library(glasp)
set.seed(1)

N = 1000
p = 10
nzc = 5
censProb = 0.3

x = matrix(rnorm(N * p), N, p)
beta = rnorm(nzc)
fx = x[, seq(nzc)] %*% beta/3
hx = exp(fx)
ty = rexp(N, hx)
tcens = rexp(N, hx/2)  # censoring indicator

data <- data.frame(time = pmin(ty, tcens), event = factor((ty < tcens) + 0), x)

model <- linear_classification(event~time + ., data, l1 = 0.03, l2 = 0.01, frob = 0.001, num_comp = 5)
model$beta
model$clusters

N = 2
x = matrix(rnorm(N * p), N, p)
beta = rnorm(nzc)
fx = x[, seq(nzc)] %*% beta/3
hx = exp(fx)
ty = rexp(N, hx)
tcens = rexp(N, hx/2)  # censoring indicator
new_data <- data.frame(time = pmin(ty, tcens), event = factor((ty < tcens) + 0), x)

linear_predictor <- predict(model, new_data[1,], "numeric")
time <- seq(0, 5, 0.1)
S0 <- approx(model$info$blin_S$time, model$info$blin_S$S0, time)
lambda <- hx[1]
S_pred <- S0$y^exp(linear_predictor)
S_true <- exp(-lambda*time)

plot(S_pred)
lines(S_true)
library(ggplot2)
library(reshape2)
library(dplyr)
data.frame(`GLASP estimation` = S_pred, `True function` = S_true, x = S0$x) %>% melt(id.vars = c("x")) %>%
  ggplot() + geom_line(aes(x = x, y = value, group = variable, linetype = variable, color = variable), size = 1) +
  #geom_point(aes(x = x, y = value, group = variable, color = variable)) +
  labs(color = "", linetype = "", y = "Survival function", x = "time") +
  theme_classic() +
  theme(legend.position = "top")
