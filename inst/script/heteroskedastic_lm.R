## hypothesis testing of simle linear regression
## and 95% CI's
## under heteroskedastic data model

## under the null hypothesis
n_sim <- 1000
n <- 100
beta_0 <- 3
beta_1 <- 0
reject <- integer(n_sim)
ci <- integer(n_sim)

set.seed(142273947)
for (i in 1:n_sim) {
  ## generate data
  x <- rnorm(n, 5, 2)
  mu <- beta_0 + beta_1 * x
  y <- rexp(n, rate = 1 / mu)
  
  mod <- lm(y ~ x)
  p_val <- summary(mod)$coef[2,4]
  reject[i] <- ifelse(p_val < 0.05, 1, 0)
  
  lower <- mod$coefficients[2] - summary(mod)$coef[2,2] * 1.96
  upper <- mod$coefficients[2] + summary(mod)$coef[2,2] * 1.96
  ci[i] <- ifelse(beta_1 > lower & beta_1 < upper, 1, 0)
}

mean(reject) # 0.041
mean(ci) # 0.956

## Type 1 error Rate = 0.04
## 95% confidence interval coverage is good




## Under the alternative hypothesis

n_sim <- 1000
n <- 100
beta_0 <- 3
beta_1 <- 4
reject <- integer(n_sim)
ci <- integer(n_sim)

set.seed(142273947)

for (i in 1:n_sim) {
  ## generate data
  x <- rnorm(n, 5, 2)
  mu <- beta_0 + beta_1 * x
  y <- rexp(n, rate = 1 / mu)
  
  mod <- lm(y ~ x)
  p_val <- summary(mod)$coef[2,4]
  reject[i] <- ifelse(p_val < 0.05, 1, 0)
  
  lower <- mod$coefficients[2] - summary(mod)$coef[2,2] * 1.96
  upper <- mod$coefficients[2] + summary(mod)$coef[2,2] * 1.96
  ci[i] <- ifelse(beta_1 > lower & beta_1 < upper, 1, 0)
}

mean(reject) # 0.904
mean(ci) # 0.92

## Type II error rate = 0.097
## 95% CI coverage is too low


