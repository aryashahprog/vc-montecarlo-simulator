source("vc_fund_sim_core.R")
library(dplyr)
library(ggplot2)

set.seed(123)

# Run Monte Carlo and keep full sims
res <- run_vc_monte_carlo(
  n_sims          = 1000,
  fund_size       = 50e6,
  n_initial_deals = 30,
  reserve_ratio   = 0.4,
  follow_on_policy= "top_quartile",
  dist_mode       = "discrete"
)

sims <- res$sims

# Build long data frame of yearly LP net cash flows for each simulation
paths <- do.call(rbind, lapply(seq_along(sims), function(i) {
  sim <- sims[[i]]
  data.frame(
    sim_id    = i,
    year      = sim$years,
    lp_cf_net = sim$lp_cf_net
  )
}))

# Cumulative LP net cash flows over time (J-curve)
paths <- paths %>%
  group_by(sim_id) %>%
  arrange(year, .by_group = TRUE) %>%
  mutate(cum_cf = cumsum(lp_cf_net)) %>%
  ungroup()

# Summary across simulations: median + quantile bands
jcurve_summary <- paths %>%
  group_by(year) %>%
  summarise(
    median_cum = median(cum_cf),
    p25_cum    = quantile(cum_cf, 0.25),
    p75_cum    = quantile(cum_cf, 0.75),
    p10_cum    = quantile(cum_cf, 0.10),
    p90_cum    = quantile(cum_cf, 0.90),
    .groups = "drop"
  )

# Plot J-curve fan chart (note the print())
p_jcurve <- ggplot() +
  geom_ribbon(data = jcurve_summary,
              aes(x = year, ymin = p10_cum, ymax = p90_cum),
              fill = "#D0E2FF", alpha = 0.5) +
  geom_ribbon(data = jcurve_summary,
              aes(x = year, ymin = p25_cum, ymax = p75_cum),
              fill = "#6FA8FF", alpha = 0.6) +
  geom_line(data = jcurve_summary,
            aes(x = year, y = median_cum),
            color = "black", linewidth = 1.2) +
  labs(
    title = "VC Fund J-Curve (LP Net Cash Flows)",
    subtitle = "Median and quantile bands across 1,000 simulated funds",
    x = "Year",
    y = "Cumulative net cash flow to LPs (USD)"
  ) +
  theme_minimal(base_size = 14)

print(p_jcurve)

