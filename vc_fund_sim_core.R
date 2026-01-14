# vc_fund_sim_core.R
# Core engine for VC fund Monte Carlo simulations
# Uses benchmark-calibrated exits from Rdata_calibration.R

# Utility: IRR calculation
irr_from_cashflows <- function(cf) {
  npv <- function(r) sum(cf / (1 + r)^(0:(length(cf) - 1)))
  if (all(cf >= 0) || all(cf <= 0)) return(NA_real_)
  tryCatch(
    uniroot(npv, lower = -0.99, upper = 5)$root,
    error = function(e) NA_real_
  )
}

# Core fund simulation
simulate_vc_fund <- function(
    fund_size        = 50e6,
    n_initial_deals  = 30,
    reserve_ratio    = 0.4,
    fund_life_years  = 10,
    invest_period    = 3,
    mgmt_fee_rate    = 0.02,
    carry_rate       = 0.20,
    hurdle_rate      = 0.08,
    seed             = NULL
) {
  if (!is.null(seed)) set.seed(seed)
  
  years   <- 0:fund_life_years
  n_years <- length(years)
  
  # 1) Capital allocation
  initial_pool <- fund_size * (1 - reserve_ratio)
  reserve_pool <- fund_size * reserve_ratio
  
  initial_check <- initial_pool / n_initial_deals
  invest_years  <- sample(1:invest_period, n_initial_deals, replace = TRUE)
  
  # 2) Benchmark-calibrated exits
  base_multiples <- numeric(n_initial_deals)
  exit_years     <- integer(n_initial_deals)
  follow_flags   <- logical(n_initial_deals)
  
  for (i in seq_len(n_initial_deals)) {
    inv <- simulate_investment()
    
    base_multiples[i] <- inv$exit_multiple
    exit_years[i] <- min(
      fund_life_years,
      max(1, round(inv$exit_time_years))
    )
    follow_flags[i] <- inv$follow_on
  }
  
  # 3) Follow-on allocation
  follow_on_deals <- rep(0, n_initial_deals)
  follow_on_years <- rep(NA_integer_, n_initial_deals)
  remaining_reserve <- reserve_pool
  
  follow_idx <- which(follow_flags)
  
  for (i in follow_idx) {
    if (remaining_reserve <= 0) break
    fo_year  <- min(invest_period + 1, exit_years[i] - 1)
    if (fo_year < 1) next
    
    fo_check <- min(initial_check, remaining_reserve / 2)
    follow_on_deals[i] <- fo_check
    follow_on_years[i] <- fo_year
    remaining_reserve  <- remaining_reserve - fo_check
  }
  
  # 4) LP cash flows
  lp_cf <- rep(0, n_years)
  
  # Initial investments
  for (i in seq_len(n_initial_deals)) {
    idx <- which(years == invest_years[i])
    lp_cf[idx] <- lp_cf[idx] - initial_check
  }
  
  # Follow-ons
  for (i in seq_len(n_initial_deals)) {
    if (!is.na(follow_on_years[i]) && follow_on_deals[i] > 0) {
      idx <- which(years == follow_on_years[i])
      lp_cf[idx] <- lp_cf[idx] - follow_on_deals[i]
    }
  }
  
  # Management fees
  for (t in seq_along(years)) {
    y <- years[t]
    if (y > 0 && y <= fund_life_years) {
      lp_cf[t] <- lp_cf[t] - mgmt_fee_rate * fund_size
    }
  }
  
  # Exit distributions
  gross_proceeds <- (initial_check + follow_on_deals) * base_multiples
  
  for (i in seq_len(n_initial_deals)) {
    idx <- which(years == exit_years[i])
    lp_cf[idx] <- lp_cf[idx] + gross_proceeds[i]
  }
  
  # 5) Carry & waterfall
  total_invested <- -sum(lp_cf[lp_cf < 0])
  total_dist     <- sum(lp_cf[lp_cf > 0])
  gross_profit   <- total_dist - total_invested
  
  target_lp_min <- total_invested * (1 + hurdle_rate)^fund_life_years
  gp_carry <- 0
  lp_cf_net <- lp_cf
  
  if (gross_profit > 0 && total_dist > target_lp_min) {
    excess_profit <- total_dist - target_lp_min
    gp_carry <- carry_rate * excess_profit
    lp_cf_net[length(lp_cf_net)] <- lp_cf_net[length(lp_cf_net)] - gp_carry
  }
  
  gp_cf <- rep(0, n_years)
  gp_cf[length(gp_cf)] <- gp_carry
  

  # 6) Metrics
  list(
    years         = years,
    lp_cf_gross   = lp_cf,
    lp_cf_net     = lp_cf_net,
    gp_cf         = gp_cf,
    total_invested= total_invested,
    total_dist    = total_dist,
    gp_carry      = gp_carry,
    lp_irr_gross  = irr_from_cashflows(lp_cf),
    lp_irr_net    = irr_from_cashflows(lp_cf_net),
    lp_moic_gross = total_dist / total_invested,
    lp_moic_net   = (total_dist - gp_carry) / total_invested
  )
}


# Monte Carlo wrapper
run_vc_monte_carlo <- function(n_sims = 2000, ...) {
  sims <- vector("list", n_sims)
  for (i in seq_len(n_sims)) sims[[i]] <- simulate_vc_fund(...)
  data.frame(
    lp_irr_gross  = sapply(sims, `[[`, "lp_irr_gross"),
    lp_irr_net    = sapply(sims, `[[`, "lp_irr_net"),
    lp_moic_gross = sapply(sims, `[[`, "lp_moic_gross"),
    lp_moic_net   = sapply(sims, `[[`, "lp_moic_net")
  )
}


