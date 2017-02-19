#---------------------------------------------------------
# 
# Authors      : Jonathan Sweeney
# Date         : 2/15/2017
# Description  : The following code reproduces the results from Imbens and Rubin (1997) using 
#              : Stan as the 
#
#---------------------------------------------------------

# Clear memory
rm(list=ls())

# Install all packages necessary to run code
#install.packages(c("tidyverse", "rstan"))

# Load libraries
library(tidyverse)
library(rstan)

#---------------------------------------------------------
# Executed statements
#---------------------------------------------------------

# Generate data from Sommer-Zeger in Table 3.
dat <- list(N_c = c(9675, 12094),
            Yobs_C1 = c(9663, 9675),
            Yobs_N1 = c(2385, 2419),
            Yobs_NC0 = c(11514, 11588),
            N = c(11588),
            Yobs_NC = c(rep(0,74), rep(1, 11514)))

# Replicate Figure 1 and 3 of CACE posterior with/out exclusion restriction.
# Stan model
model <- "
data {
int<lower=0> N; // Number of units missing a compliance observation
int<lower=0> N_c[2]; // Cardinal of units of compliers and total observed
int<lower=0> Yobs_C1[2]; // Compliance treatment
int<lower=0> Yobs_N1[2]; // Noncompliance treatment
int<lower=0> Yobs_NC[N]; // Either noncompliance or compliance control
}

parameters {
// (no exlusion)
//real<lower=0,upper=1> omega;
//real<lower=0,upper=1> eta_c0;
//real<lower=0,upper=1> eta_c1;
//real<lower=0,upper=1> eta_n0;
//real<lower=0,upper=1> eta_n1;

// (exlusion)
real<lower=0,upper=1> omega;
real<lower=0,upper=1> eta_c0;
real<lower=0,upper=1> eta_c1;
real<lower=0,upper=1> eta_n; // Note: when exclusion is assumed n=n0=n1
}

transformed parameters {

}

model {
// Priors (no exlusion)
//omega ~ beta(1,1);
//eta_c0 ~ beta(1,1);
//eta_c1 ~ beta(1,1);
//eta_n0 ~ beta(1,1);
//eta_n1 ~ beta(1,1);

// Priors (exlusion)
omega ~ beta(1,1);
eta_c0 ~ beta(1,1);
eta_c1 ~ beta(1,1);
eta_n ~ beta(1,1);

// Likelihood sampling (no exlusion)
//N_c[1] ~ binomial(N_c[2], omega);
//Yobs_C1[1] ~ binomial(Yobs_C1[2], eta_c1);
//Yobs_N1[1] ~ binomial(Yobs_N1[2], eta_n1);

//for (n in 1:N)
//target += log_mix(omega, binomial_lpmf(Yobs_NC[n] | 1, eta_c0), binomial_lpmf(Yobs_NC[n] | 1, eta_n0));

// Likelihood sampling (exlusion)
N_c[1] ~ binomial(N_c[2], omega);
Yobs_C1[1] ~ binomial(Yobs_C1[2], eta_c1);
Yobs_N1[1] ~ binomial(Yobs_N1[2], eta_n);

for (n in 1:N)
target += log_mix(omega, binomial_lpmf(Yobs_NC[n] | 1, eta_c0), binomial_lpmf(Yobs_NC[n] | 1, eta_n));

}

generated quantities{
// Calculate compliance average causal effect (CACE)
real CACE;
CACE = eta_c1-eta_c0;
}
"

fit <- stan(model_code = model, data=dat, iter = 1000, chains = 3)

print(fit)
stan_dens(fit)

