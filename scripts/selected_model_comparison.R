library(brms)
library(tidyverse)
library(dplyr)
library(tibble)
library(purrr)
library(brms)
library(loo)

## load data and models

data <- read_csv("./data_4IRT.csv")%>%
  mutate(age_group = factor(substr(age, 1,1)))

# data <- data%>%
#   select(subjID, task, score, sex)

irt1PL <- readRDS("../saves/irt1PL1.rds")

rasch_fit_mode <- readRDS("../saves/rasch_fit_mode.rds")

irt2PL <- readRDS("../saves/irt2PL_guess_fit_sel.rds")

## make indices

items <- data %>%
  distinct(task) %>%
  arrange(task) %>%
  pull(task)

easiness_1PL <- ranef(irt1PL)$task %>%
  as_tibble(rownames = "task") %>%
  arrange(task) %>%
  pull(Estimate.eta_Intercept)

infit_fit_sel <- rasch_fit_mode %>% 
  filter(fit_index == "infit") %>%
  arrange(task)%>% 
  pull(mode)

outfit_fit_sel <- rasch_fit_mode %>% 
  filter(fit_index == "outfit") %>%
  arrange(task)%>% 
  pull(mode)

disc_2PL <- coef(irt2PL)$task[, , "logalpha_Intercept"]%>%
  as_tibble(rownames = "task")%>%
  arrange(task) %>%
  pull(Estimate)

source("./irt-analysis/simulated_annealing.R") # üü

## compare models and save output

ks <- c(18, 19, 20, 21, 22, 23)
n_runs <- 1
model_comparison <- tibble()

for (k in ks) {

  for (item_set_name in names(items_exclusion)) {
    
    item_set <- items_exclusion[[item_set_name]]

    sim <- simulated_annealing_rasch(item_set, k)
    keep <- as.logical(unlist(sim$best_subset))

    sel <- item_set[keep]

    # temporary diagnostics
    stopifnot(
      length(keep) == length(item_set),
      length(sel) == sum(keep, na.rm = TRUE),
      !any(is.na(sel))
    )

    sub_dat <- data %>% filter(task %in% sel)

    # --- refit models + LOO (NO moment_match) ---
    m1PL <- update(
      irt1PL, newdata = sub_dat,
      chains = 6, cores = 6, threads = threading(2),
      backend = "cmdstanr",
      control = list(adapt_delta = 0.99, max_treedepth = 15)
    ) %>% add_criterion("loo", cores = 1, ndraws = 2000)

    m2PL <- update(
      irt2PL, newdata = sub_dat,
      chains = 6, cores = 6, threads = threading(2),
      backend = "cmdstanr",
      control = list(adapt_delta = 0.99, max_treedepth = 15)
    ) %>% add_criterion("loo", cores = 1, ndraws = 2000)

    # pull loo objects from brms fits
    loo1 <- m1PL$criteria$loo
    loo2 <- m2PL$criteria$loo

    # 1) per-model absolute LOO
    loo_tbl <- bind_rows(
      tibble(model = "1PL",
            elpd_loo = loo1$estimates["elpd_loo","Estimate"],
            se_elpd_loo = loo1$estimates["elpd_loo","SE"],
            looic = loo1$estimates["looic","Estimate"],
            se_looic = loo1$estimates["looic","SE"]),
      tibble(model = "2PL",
            elpd_loo = loo2$estimates["elpd_loo","Estimate"],
            se_elpd_loo = loo2$estimates["elpd_loo","SE"],
            looic = loo2$estimates["looic","Estimate"],
            se_looic = loo2$estimates["looic","SE"])
    )

    # 2) pairwise diffs (named list keeps model labels stable)
    comp_tbl <- loo_compare(list("1PL" = loo1, "2PL" = loo2)) %>%
      as.data.frame() %>%
      tibble::rownames_to_column("model") %>%
      as_tibble() %>%
      select(model, elpd_diff, se_diff)

    # 3) join + add SA scorings + items
    final_tbl <- loo_tbl %>%
      left_join(comp_tbl, by = "model") %>%
      mutate(
        k = k,
        n_items = length(sel),
        item_set = item_set_name,
        items = list(sel),
        best_score   = sim$best_score,
        spacing      = sim$spacing,
        mean_infit   = sim$mean_infit,
        mean_outfit  = sim$mean_outfit,
        var_disc_2PL = sim$var_disc_2PL,
        disc_penalty = sim$disc_penalty
        
      )

    model_comparison <- bind_rows(model_comparison, final_tbl)
    
    saveRDS(model_comparison, "./irt-analysis/saves/model_comparison_18to23.rds")
  }
}