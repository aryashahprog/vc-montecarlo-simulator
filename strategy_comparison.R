# strategy_comparison.R

source("vc_fund_sim_core.R")
library(ggplot2)

set.seed(123)

# helper to run one "strategy"
simulate_strategy <- function(n_deals, label, n_sims = 3000) {
  out <- run_vc_monte_carlo(
    n_sims          = n_sims,
    fund_size       = 50e6,
    n_initial_deals = n_deals,
    reserve_ratio   = 0.4,
    follow_on_policy= "top_quartile",
    dist_mode       = "discrete"
  )$summary
  
  out$strategy <- label
  out
}

# run a few portfolio constructions
res_20  <- simulate_strategy(20,  "20 deals")
res_30  <- simulate_strategy(30,  "30 deals")
res_50  <- simulate_strategy(50,  "50 deals")
res_100 <- simulate_strategy(100, "100 deals")

all_res <- rbind(res_20, res_30, res_50, res_100)

# drop NA IRRs (degenerate cashflows)
all_res_clean <- subset(all_res, !is.na(lp_irr_net))

# -----------------------------
# Summary table (for README)
# -----------------------------
summary_table <- aggregate(
  cbind(lp_moic_net, lp_irr_net) ~ strategy,
  data = all_res_clean,
  FUN = function(x) c(mean = mean(x), median = median(x))
)

print(summary_table)

# Probability of net MOIC >= 3x by strategy
prob_3x <- aggregate(
  lp_moic_net ~ strategy,
  data = all_res_clean,
  FUN = function(x) mean(x >= 3)
)
names(prob_3x)[2] <- "prob_net_moic_ge_3x"
print(prob_3x)

# -----------------------------
# Visuals
# -----------------------------

# 1) MOIC distribution by strategy
ggplot(all_res_clean, aes(x = lp_moic_net, fill = strategy)) +
  geom_density(alpha = 0.35) +
  coord_cartesian(xlim = c(0, 10)) +
  labs(
    title = "Net LP MOIC Distribution by Portfolio Size",
    x = "Net MOIC",
    y = "Density"
  )

# 2) IRR distribution by strategy
ggplot(all_res_clean, aes(x = lp_irr_net, fill = strategy)) +
  geom_density(alpha = 0.35) +
  labs(
    title = "Net LP IRR Distribution by Portfolio Size",
    x = "Net IRR",
    y = "Density"
  )


