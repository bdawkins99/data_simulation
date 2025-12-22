
simulate_survival <- function(num_samples = 1000, num_features = 2, 
                              beta_shift = 1, sd_beta = 0.05,
                              model = c("PH", "AFT"),
                              dist = c("weibull", "exponential"),
                              lambda = 0.01, kappa = 1.5,
                              mu = 2, sigma = 1,
                              censor_rate = 0.5) {
  model <- match.arg(model)
  dist <- match.arg(dist)

  # Beta coefficients vary slightly about mean = 0, with an added shift of either +beta_shift or -beta_shift
  # Why? To allow positive or negative associations with the time-to-event outcome, which are equiprobable 
  # at the moment, but we may add a prob_vec to the sample() function later.
  betas = rnorm(num_features, mean = 0, sd = sd_beta) + beta_shift * sample(c(-1, 1), num_features, replace = TRUE)
  
  # Predictors: standard normal
  X <- matrix(rnorm(num_samples * num_features), nrow = num_samples, ncol = num_features)
  colnames(X) <- paste0("X", 1:num_features)
  
  # Linear predictor
  linpred <- X %*% betas
  
  # Simulate survival times
  U <- runif(num_samples)
  if (model == "PH") {
    if (dist == "weibull") {
      T <- ((-log(U)) / (lambda * exp(linpred)))^(1 / kappa)
    } else if (dist == "exponential") {
      T <- (-log(U)) / (lambda * exp(linpred))
    }
  } else if (model == "AFT") {
    Z <- rnorm(num_samples)
    logT <- mu + linpred + sigma * Z
    T <- exp(logT)
  }
  
  # Censoring
  C <- rexp(num_samples, rate = censor_rate)
  time <- pmin(T, C)
  status <- as.numeric(T <= C)
  
  data <- data.frame(time = time, status = status, X)
  return(data)
}

# Example using Proportional Hazards (PH) model
data_ph <- withr::with_seed(123, 
                simulate_survival(num_samples  = 1000, 
                                  num_features = 2, 
                                  beta_shift   = 1, 
                                  model        = "PH", 
                                  dist         = "weibull")
                )
# Example using Accelerated Failure Time (AFT) model
data_aft <- withr::with_seed(123, 
                simulate_survival(num_samples  = 1000, 
                                  num_features = 2, 
                                  beta_shift   = 1, 
                                  model        = "AFT", 
                                  dist         = "exponential")
                )


library(survival)

# Cox proportional hazards
cox_fit <- coxph(Surv(time, status) ~ X1 + X2, data = data_ph)
summary(cox_fit)

# Accelerated failure time
aft_fit <- survreg(Surv(time, status) ~ X1 + X2, data = data_aft, dist = "exponential")
summary(aft_fit)

aft_fit0 <- survreg(Surv(time, status) ~ 1, data = data_aft, dist = "exponential")
summary(aft_fit0)



