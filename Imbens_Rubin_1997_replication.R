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
sommer_zeger <- tibble(zobs = c(0, 0, 1, 1, 1, 1),
                       dobs = c(0, 0, 0, 0, 1, 1),
                       yobs = c(0, 1, 0, 1, 0, 1),
                       freq = c(75, 11514, 34, 2385, 12, 9663))

sommer_zeger <- sommer_zeger[rep(attr(sommer_zeger, "row.names"), sommer_zeger$freq), 1:3]

# Replicate Figure 1 of CACE posterior without exclusion restriction.
# Stan model
model <- "
data {
int<lower=0> n; // Number of individuals in the sample
vector[n] zobs; // Individual assignment: 0 is not assigned, 1 assigned
vector[n] dobs; // Individual treatment: 0 is not treated, 1 treated
vector[n] yobs; // Individual outcome: 0 is dead, 1 is alive
}

transformed data {
int N_c;
int N_1;
N_c=9663+12;
N_1=1;
}

parameters {
real<lower=-10,upper=10> eta_c0;
real<lower=-10,upper=10> eta_c1;
real<lower=-10,upper=10> eta_n0;
real<lower=-10,upper=10> eta_n1;
real<lower=0,upper=1> omega; // Proportion of sample who are compliers
}

transformed parameters {

}

model {
// Priors
eta_c0 ~ uniform(-10,10);
eta_c1 ~ uniform(-10,10);
eta_n0 ~ uniform(-10,10);
eta_n1 ~ uniform(-10,10);
omega ~ uniform(0,1);

// Postieror distribution (Note: written as the product of 5 distributions)

target += binomial_lpmf(N_c | N_1, omega);

//if()
//target += binomial(yobs[i] | eta_c0)

//else if ()

//else if ()

//else ()


}
"

fit <- stan(model_code = model, data=sommer_zeger, iter = 1000, chains = 3)

print(fit)

