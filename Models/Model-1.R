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

df_total_liquefaction$type <- as.numeric(as.factor(df_total_liquefaction$type)) # Perhaps needed to be used as an indicator in JAGS

########################################################################################################
########################### Model 1: All variables + All observations ##################################
########################################################################################################
data_alt <- df_total_liquefaction

# Calculating imbalance difference
x <- length(data_alt$status[which(data_alt$status == 1)]) - length(data_alt$status[which(data_alt$status == 0)])

upsampling <- data_alt %>% filter(status == 0)
samp <- sample(nrow(upsampling), 0.9 * x)

data_alt <- rbind(data_alt, upsampling[samp, ])

model_code <- "
model
{
  # Likelihood
  for (t in 1:T) {
    y[t] ~ dbin(p[t], K)
    logit(p[t]) <- alpha + beta_1 * x_1[t] + beta_2 * x_2[t] + beta_3 * x_3[t] + beta_4 * x_4[t] +
                                                                                 beta_5[type[t]] * x_5[t]
  }

  for (j in 1:J) {

  beta_5[j] ~ dnorm(a,b)

  }

  # Priors
  a ~ dnorm(0,1)
  b ~ dunif(0,1)

  alpha ~ dnorm(0.0,0.01)
  beta_1 ~ dnorm(0.0,0.01)
  beta_2 ~ dnorm(0.0,0.01)
  beta_3 ~ dnorm(0.0,0.01)
  beta_4 ~ dnorm(0.0,0.01)
}
"

# Set up the data
model_data <- list(
  T = nrow(data_alt), y = data_alt$status,
  x_1 = data_alt$magnitude, x_2 = data_alt$accelaration,
  x_3 = data_alt$depth, x_4 = data_alt$total_stress,
  x_5 = data_alt$test_value, type = data_alt$type,
  K = 1, J = 3
)

# Choose the parameters to watch
model_parameters <- c("alpha", "beta_1", "beta_2", "beta_3", "beta_4", "beta_5", "p")

# Run the model 1
model_run <- jags(
  data = model_data,
  parameters.to.save = model_parameters,
  model.file = textConnection(model_code)
)


plot(model_run)
print(model_run)


p <- model_run$BUGSoutput$mean$p

df_test <- data_alt %>%
  select(status, CSR) %>%
  mutate(prob = p, pred = 1)
df_test$pred[df_test$prob < 0.5] <- 0

tab <- table(df_test$status, df_test$pred)
(tab[1, 1] + tab[2, 2]) / sum(tab) # Calculating performance

########################################################################################################
########################### Model 2: All variables + Only SPT ##################################
########################################################################################################

data_alt <- df_total_liquefaction %>% filter(type == 2)

# Calculating imbalance difference
x <- length(data_alt$status[which(data_alt$status == 1)]) - length(data_alt$status[which(data_alt$status == 0)])

upsampling <- data_alt %>% filter(status == 0)
samp <- sample(nrow(upsampling), 0.9 * x)

data_alt <- rbind(data_alt, upsampling[samp, ])

len <- nrow(data_alt)

model_code <- "
model
{
  # Likelihood
  for (t in 1:T) {
    y[t] ~ dbin(p[t], K)
    logit(p[t]) <- alpha + beta_1 * x_1[t] + beta_2 * x_2[t] + beta_3 * x_3[t] + beta_4 * x_4[t] +
                                                                                 beta_5 * x_5[t]
  }


  # Priors

  alpha ~ dnorm(0.0,0.01)
  beta_1 ~ dnorm(0.0,0.01)
  beta_2 ~ dnorm(0.0,0.01)
  beta_3 ~ dnorm(0.0,0.01)
  beta_4 ~ dnorm(0.0,0.01)
  beta_5 ~ dnorm(0.0,0.01)
}
"

# Set up the data
model_data <- list(
  T = len, y = data_alt$status,
  x_1 = data_alt$magnitude, x_2 = data_alt$accelaration,
  x_3 = data_alt$depth, x_4 = data_alt$total_stress,
  x_5 = data_alt$test_value,
  K = 1
)

# Choose the parameters to watch
model_parameters <- c("alpha", "beta_1", "beta_2", "beta_3", "beta_4", "beta_5", "p")

# Run the model
model_run <- jags(
  data = model_data,
  parameters.to.save = model_parameters,
  model.file = textConnection(model_code)
)


plot(model_run)
print(model_run)


p <- model_run$BUGSoutput$mean$p

df_test <- data_alt %>%
  select(status, CSR) %>%
  mutate(prob = p, pred = 1)
df_test$pred[df_test$prob < 0.5] <- 0

tab <- table(df_test$status, df_test$pred)

(tab[1, 1] + tab[2, 2]) / sum(tab)

########################################################################################################
########################### Model 3: spt,CSR,Mw + Only SPT ##################################
########################################################################################################

data_alt <- df_total_liquefaction %>% filter(type == 2)

# Calculating imbalance difference
x <- length(data_alt$status[which(data_alt$status == 1)]) - length(data_alt$status[which(data_alt$status == 0)])

upsampling <- data_alt %>% filter(status == 0)
samp <- sample(nrow(upsampling), 0 * x)

data_alt <- rbind(data_alt, upsampling[samp, ])

len <- nrow(data_alt)

model_code <- "
model
{
  # Likelihood
  for (t in 1:T) {
    y[t] ~ dbin(p[t], K)
    logit(p[t]) <- alpha + beta_1 * x_1[t] + beta_2 * x_2[t] + beta_3 * x_3[t]
  }


  # Priors

  alpha ~ dnorm(0.0,0.01)
  beta_1 ~ dnorm(0.0,0.01)
  beta_2 ~ dnorm(0.0,0.01)
  beta_3 ~ dnorm(0.0,0.01)

}
"

# Set up the data
model_data <- list(
  T = len, y = data_alt$status,
  x_1 = data_alt$magnitude, x_2 = data_alt$CSR,
  x_3 = data_alt$test_value,
  K = 1
)

# Choose the parameters to watch
model_parameters <- c("alpha", "beta_1", "beta_2", "beta_3", "p")

# Run the model
model_run <- jags(
  data = model_data,
  parameters.to.save = model_parameters,
  model.file = textConnection(model_code)
)


plot(model_run)
print(model_run)


p <- model_run$BUGSoutput$mean$p

df_test <- data_alt %>%
  select(status, CSR) %>%
  mutate(prob = p, pred = 1)
df_test$pred[df_test$prob < 0.5] <- 0

tab <- table(df_test$status, df_test$pred)

(tab[1, 1] + tab[2, 2]) / sum(tab)


# Exploring other methods
formula <- as.formula("status ~ magnitude + accelaration + test_value + water_depth + total_stress")

glm.fit <- glm(formula,
  data = data_alt, family = binomial
)
glm.probs <- predict(glm.fit, type = "response")
check <- data.frame(prob = glm.probs, pred = rep(1, length(glm.probs)))
check$pred[check$prob < 0.5] <- 0
tab <- table(data_alt$status, check$pred)
(tab[1, 1] + tab[2, 2]) / sum(tab)


# qda.fit = qda(formula,data=data_alt)
# qda.class=predict(qda.fit,data_alt)$class
# tab <- table(data_alt$status, qda.class)
#
# (tab[1,1] + tab[2,2])/sum(tab)
data_alt2 <- data_alt %>% mutate(status2 = as.factor(status))
samp <- sample(nrow(data_alt2), 0.7 * nrow(data_alt2))
train <- data_alt2[samp, ]
test <- data_alt2[-samp, ]

rf.fit <- randomForest(status2 ~ . - status - type,
  data = train, mtry = 5
)
pred <- predict(rf.fit, newdata = test)

tab <- table(test$status, pred)
(tab[1, 1] + tab[2, 2]) / sum(tab)
varImpPlot(rf.fit)
########################################################################################################
########################### Model 4: cpt,CSR,Mw + Only CPT ##################################
########################################################################################################

data_alt <- df_total_liquefaction %>% filter(type == 1)

# Calculating imbalance difference
x <- length(data_alt$status[which(data_alt$status == 1)]) - length(data_alt$status[which(data_alt$status == 0)])

upsampling <- data_alt %>% filter(status == 0)
samp <- sample(nrow(upsampling), 0 * x)

data_alt <- rbind(data_alt, upsampling[samp, ])

len <- nrow(data_alt)

model_code <- "
model
{
  # Likelihood
  for (t in 1:T) {
    y[t] ~ dbin(p[t], K)
    logit(p[t]) <- alpha + beta_1 * x_1[t] + beta_2 * x_2[t] + beta_3 * x_3[t]
  }


  # Priors

  alpha ~ dnorm(0.0,0.01)
  beta_1 ~ dnorm(0.0,0.01)
  beta_2 ~ dnorm(0.0,0.01)
  beta_3 ~ dnorm(0.0,0.01)

}
"

# Set up the data
model_data <- list(
  T = len, y = data_alt$status,
  x_1 = data_alt$magnitude, x_2 = data_alt$CSR,
  x_3 = data_alt$test_value,
  K = 1
)

# Choose the parameters to watch
model_parameters <- c("alpha", "beta_1", "beta_2", "beta_3", "p")

# Run the model
model_run <- jags(
  data = model_data,
  parameters.to.save = model_parameters,
  model.file = textConnection(model_code)
)


plot(model_run)
print(model_run)


p <- model_run$BUGSoutput$mean$p

df_test <- data_alt %>%
  dplyr::select(status, CSR) %>%
  mutate(prob = p, pred = 1)
df_test$pred[df_test$prob < 0.5] <- 0

tab <- table(df_test$status, df_test$pred)

(tab[1, 1] + tab[2, 2]) / sum(tab)

formula <- as.formula("status ~ magnitude  + test_value +  CSR")

glm.fit <- glm(formula,
  data = data_alt, family = binomial
)
glm.probs <- predict(glm.fit, type = "response")
check <- data.frame(prob = glm.probs, pred = rep(1, length(glm.probs)))
check$pred[check$prob < 0.5] <- 0
tab <- table(data_alt$status, check$pred)
(tab[1, 1] + tab[2, 2]) / sum(tab)


qda.fit <- qda(formula, data = data_alt)
qda.class <- predict(qda.fit, data_alt)$class
tab <- table(data_alt$status, qda.class)

(tab[1, 1] + tab[2, 2]) / sum(tab)

########################################################################################################
########################### Model 5: All variables + Only CPT+SVW ##################################
########################################################################################################
data_alt <- df_total_liquefaction %>% filter(type == 1 | type == 3)
data_alt$type[which(data_alt$type == 3)] <- 2

# Calculating imbalance difference
x <- length(data_alt$status[which(data_alt$status == 1)]) - length(data_alt$status[which(data_alt$status == 0)])

upsampling <- data_alt %>% filter(status == 0)
samp <- sample(nrow(upsampling), 0.9 * x)

data_alt <- rbind(data_alt, upsampling[samp, ])

model_code <- "
model
{
  # Likelihood
  for (t in 1:T) {
    y[t] ~ dbin(p[t], K)
    logit(p[t]) <- alpha + beta_1 * x_1[t] + beta_2 * x_2[t] + beta_3 * x_3[t] + beta_4 * x_4[t] +
                                                                                 beta_5[type[t]] * x_5[t]
  }

  for (j in 1:J) {

  beta_5[j] ~ dnorm(a,b)

  }

  # Priors
  a ~ dnorm(0,1)
  b ~ dunif(0,1)

  alpha ~ dnorm(0.0,0.01)
  beta_1 ~ dnorm(0.0,0.01)
  beta_2 ~ dnorm(0.0,0.01)
  beta_3 ~ dnorm(0.0,0.01)
  beta_4 ~ dnorm(0.0,0.01)
}
"

# Set up the data
model_data <- list(
  T = nrow(data_alt), y = data_alt$status,
  x_1 = data_alt$magnitude, x_2 = data_alt$CSR,
  x_3 = data_alt$depth, x_4 = data_alt$total_stress,
  x_5 = data_alt$test_value, type = data_alt$type,
  K = 1, J = 2
)

# Choose the parameters to watch
model_parameters <- c("alpha", "beta_1", "beta_2", "beta_3", "beta_4", "beta_5", "p")

# Run the model 1
model_run <- jags(
  data = model_data,
  parameters.to.save = model_parameters,
  model.file = textConnection(model_code)
)


plot(model_run)
print(model_run)


p <- model_run$BUGSoutput$mean$p

df_test <- data_alt %>%
  dplyr::select(status, CSR) %>%
  mutate(prob = p, pred = 1)
df_test$pred[df_test$prob < 0.5] <- 0

tab <- table(df_test$status, df_test$pred)
(tab[1, 1] + tab[2, 2]) / sum(tab)
