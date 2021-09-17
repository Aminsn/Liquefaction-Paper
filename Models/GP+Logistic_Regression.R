rm(list = ls())
library(readr)
library(R2jags)
library(dplyr)
library(lubridate)
library(tidyverse)
library(MASS)
library(randomForest)

# Running model 1 is the ultimate goal. Others are just for exploration

# Reading and preparing data
df_total_liquefaction <- read_csv("4-All.csv")
df_total_liquefaction$status[which(df_total_liquefaction$status == "NO")] <- "No"
df_total_liquefaction$status <- as.numeric(as.factor(df_total_liquefaction$status))
df_total_liquefaction$status <- df_total_liquefaction$status - 1 # Making classes as 0 and 1

df_total_liquefaction <- read_csv("df_total_liquefaction.csv") #Option 2: includes fines content
df_total_liquefaction$type <- as.numeric(as.factor(df_total_liquefaction$type)) # Perhaps needs to be used as an indicator in JAGS

########################################################################################################
########################### Model 1: All variables + All observations ##################################
########################################################################################################
data_alt <- df_total_liquefaction
data_alt <- data_alt %>% group_by(type) %>% 
  mutate(test_value = (test_value - mean(test_value))/sd(test_value)) %>%
  ungroup(type) %>% 
  mutate(fines_content = replace(fines_content, is.na(fines_content), 0)) %>% 
  mutate(across(c(magnitude, 
                  accelaration,
                  depth,
                  water_depth,
                  total_stress,
                  effective_stress,
                  CSR), ~ . - mean(.)))
# Calculating imbalance difference
x <- length(data_alt$status[which(data_alt$status == 1)]) - length(data_alt$status[which(data_alt$status == 0)])

upsampling <- data_alt %>% filter(status == 0)
samp <- sample(nrow(upsampling), 0.9 * x)

data_alt <- rbind(data_alt, upsampling[samp, ])

model_code <- "
model
{
  # Likelihood
  for (i in 1:N) {
    y[i] ~ dbin(p[i], 1)
    logit(p[i]) <- alpha[i] + beta_5[type[i]] * x_5[i] 
                           
  }
  
    alpha ~ dmnorm.vcov(Mu, Sigma)
    
    # Set up mean and covariance matrix
  for(i in 1:N) {
    Mu[i] <- zeta
    Sigma[i,i] <- pow(sigma, 2) + pow(tau, 2)
    for(j in (i+1):N) {
      Sigma[i,j] <- pow(tau, 2) * exp( - rho *  sum((X[i,] - X[j,])^2)) 
      Sigma[j,i] <- Sigma[i,j]
    }
  }
  
  for (j in 1:J) {
  beta_5[j] ~ dnorm(a,b)
  }
  # Priors
  a ~ dnorm(0,10)
  b ~ dunif(0,10)
  
  zeta ~ dnorm(0, 0.01)
  sigma ~ dunif(0, 10)
  tau ~ dunif(0, 10)
  rho ~ dunif(0.1, 5)

}
"

X <- data_alt %>% dplyr::select(-status, -test_value, -CSR, -fines_content, -type)

# Set up the data
model_data <- list(
  N = nrow(data_alt), 
  y = data_alt$status,
  x_5 = data_alt$test_value,
  type = data_alt$type,
  X = X,
  J = 3
)

# Choose the parameters to watch
model_parameters <- c("alpha", "beta_1", "beta_2", "beta_3", "beta_4", "beta_5", "p")
model_parameters <- c("alpha", "beta_1", "beta_2", "beta_3", "beta_4", "beta_5", "p", "a","b")
model_parameters <- c("alpha")

# Run the model 1
model_run <- jags(
  data = model_data,
  parameters.to.save = model_parameters,
  model.file = textConnection(model_code)
)
