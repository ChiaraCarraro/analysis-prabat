library(tidyverse)
library(brms)

# set current working directory if following gives error

data <- read_csv("../data/data_4IRT.csv")%>%
  mutate(age_group = factor(substr(age, 1,1)))

data

irt_dat <- data%>%
  select(subjID, task, score, sex)

## 2PL Model
prior_va_2pl <- 
  prior("normal(0, 2)", class = "b", nlpar = "eta") + # ability-difficulty
  prior("normal(0, 1)", class = "b", nlpar = "logalpha") + # log discriminaiton
  prior("normal(0, 1)", class = "sd", group = "subjID", nlpar = "eta") + # person variability in ability
  prior("normal(0, 3)", class = "sd", group = "task", nlpar = "eta") + # item variability in difficulty
  prior("normal(0, 1)", class = "sd", group = "task", nlpar = "logalpha") # item variability in discrimination 

irt2PL_guess_fit_sel <- brm(
  data = irt_dat,
  family = brmsfamily("bernoulli", "identity"),
  bf(
    score ~ 0.33 + 0.67 * inv_logit(exp(logalpha) * eta),
    eta ~ 1 + (1 |i| task) + (1 | subjID),
    logalpha ~ 1 + (1 |i| task),
    nl = TRUE
  ),
  prior = prior_va_2pl,
  control = list(adapt_delta = 0.99, max_treedepth = 20),
  cores = 6,
  init = 0, # all parameters start at 0 
  chains = 6,
  iter = 6000,
  refresh = 1
  # threads = threading(8), #to speed things up, comment out if not on a cluster 
  # backend = "cmdstanr" #to speed things up, comment out if not on a cluster
)


saveRDS(irt2PL_guess_fit_sel, "../saves/irt2PL_guess_fit_sel.rds")

irt2PL_guess_fit_sel <- add_criterion(
  irt2PL_guess_fit_sel,
  criterion = c("loo","waic"),
  cores = 2,
  ndraws = 2000
)

saveRDS(irt2PL_guess_fit_sel, "../saves/irt2PL_guess_fit_sel.rds")
