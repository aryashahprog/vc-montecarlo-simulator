source("vc_fund_sim_core.R")

# ---- Load packages ----
library(ggplot2)
library(dplyr)

# ---- Run simulations & clean data ----
set.seed(42)

res <- run_vc_monte_carlo(
  n_sims          = 3000,
  fund_size       = 50e6,
  n_initial_deals = 30,
  reserve_ratio   = 0.4,
  follow_on_policy= "top_quartile",
  dist_mode       = "discrete"
)$summary

df <- subset(res, !is.na(lp_irr_net))


# =========================
# 1) Premium MOIC distribution plot
# =========================
ggplot(df, aes(x = lp_moic_net)) +
  geom_histogram(
    bins = 60,
    aes(y = after_stat(density)),
    fill = "#2C7BE5",
    alpha = 0.7
  ) +
  geom_density(
    color = "black",
    linewidth = 1.2
  ) +
  labs(
    title = "Net LP MOIC Distribution",
    subtitle = "3000 Monte Carlo Simulations — 30 Deals, 40% Reserve, Top Quartile Follow-Ons",
    x = "Net MOIC",
    y = "Density"
  ) +
  theme_minimal(base_size = 14)

# =========================
# 2) CDF / survival plot: P(Net MOIC ≥ X)
# =========================
cdf_df <- df %>%
  arrange(lp_moic_net) %>%
  mutate(cdf = ecdf(lp_moic_net)(lp_moic_net))

ggplot(cdf_df, aes(x = lp_moic_net, y = 1 - cdf)) +
  geom_line(color = "#E63946", linewidth = 1.3) +
  labs(
    title = "Probability of Achieving At Least X Net MOIC",
    subtitle = "Survival Function of Net LP MOIC",
    x = "Net MOIC",
    y = "P(Net MOIC ≥ X)"
  ) +
  theme_minimal(base_size = 14)

# =========================
# 3) Tail highlight: P(Net MOIC ≥ 3x)
# =========================
threshold <- 3

ggplot(df, aes(x = lp_moic_net)) +
  geom_histogram(bins = 60, fill = "grey85", color = "white") +
  geom_histogram(
    data = subset(df, lp_moic_net >= threshold),
    bins = 60,
    fill = "#2ECC71",
    color = "white"
  ) +
  geom_vline(xintercept = threshold, color = "red", linewidth = 1.2) +
  labs(
    title = "Right Tail Highlight — Probability of ≥3x Net",
    subtitle = paste0("P(Net MOIC ≥ 3x) = ",
                      round(mean(df$lp_moic_net >= threshold), 3)),
    x = "Net MOIC",
    y = "Frequency"
  ) +
  theme_minimal(base_size = 14)

