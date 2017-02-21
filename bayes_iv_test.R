# How to estimate an IV model (two-stage least squares) using R and Stan.

# Install required packages
#install.packages("rstan","AER")

# clear memory
rm(list=ls())

# Load necessary packages
library(rstan)
library(AER)

# Generate instrument data
n <- 1000
z <- rnorm(n)

# Specify error terms with for 1st and 2nd stage
e <- rnorm(n, 0.1)
u <- rnorm(n, 0.1)

# Generate 1st and 2nd stage outcomes
x <- 1 + 2 * z + 0.5 * u + e  # Note: x is correlated with u, which makes x endogenous and OLS inconsistent
y <- 3 + 1.5 * x + u

d <- data.frame(z, x, y)

# OLS results (inconsistent)
ols <- lm(y ~ x)
summary(ols)

# Baseline results
iv <- ivreg(y ~ x | z, data = d)
summary(iv)

# Stan model

model <- "
data {
int<lower=0> n;
matrix[n,2] yt;
vector[n] z;
}
parameters {
real alpha1;
real alpha2;
real beta1;
real beta2;
real<lower=0,upper=100> sigma_t;
real<lower=0,upper=100> sigma_y;
real<lower=-1,upper=1> rho_yt;
}
transformed parameters {
cov_matrix[2] Sigma_yt;
matrix[n,2] yt_hat;
Sigma_yt[1,1] = pow(sigma_y,2);
Sigma_yt[2,2] = pow(sigma_t,2);
Sigma_yt[1,2] = rho_yt*sigma_y*sigma_t;
Sigma_yt[2,1] = Sigma_yt[1,2];

// Specify model
for (i in 1:n) {
yt_hat[i,2] = alpha2 + beta2*z[i];
yt_hat[i,1] = alpha1 + beta1*yt[i,2];
}
}
model {
// Priors
sigma_y ~ uniform(0,100);
sigma_t ~ uniform(0,100);
rho_yt ~ uniform(-1,1);
alpha1 ~ normal (0,100);
alpha2 ~ normal (0,100);
beta1 ~ normal (0,100);
beta2 ~ normal (0,100);

// Posterior
for (i in 1:n)
transpose(yt[i]) ~ multi_normal(transpose(yt_hat[i]), Sigma_yt);
}
"

d_list <- list(y1=y, y2=x)
d_matrix <- as.matrix(data.frame(d_list))
data <- list(yt=d_matrix, z=z, n=n)

fit <- stan(model_code = model, data=data, iter = 1000, chains = 3, pars=c("alpha1", "alpha2", "beta1", "beta2", "sigma_t", "sigma_y", "rho_yt"))

print(fit)

d <- as.data.frame(fit)