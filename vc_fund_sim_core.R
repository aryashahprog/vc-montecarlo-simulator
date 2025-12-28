# vc_fund_sim_core.R
# Core engine for VC fund Monte Carlo simulations

# -----------------------------
# Utility: IRR calculation
# -----------------------------
irr_from_cashflows <- function(cf, guess = 0.15) {
  npv <- function(r) sum(cf / (1 + r)^(0:(length(cf) - 1)))
  f <- function(r) npv(r)
  
  # Handle degenerate cases
  if (all(cf >= 0) || all(cf <= 0)) return(NA_real_)
  
  out <- tryCatch(
    uniroot(f, lower = -0.99, upper = 5)$root,
    error = function(e) NA_real_
  )
  out
}

# -----------------------------
# Outcome distributions
# -----------------------------

# Discrete bucket distribution (simple power-law-ish)
draw_multiples_discrete <- function(n) {
  multiples <- c(0, 1, 3, 10, 50)
  probs     <- c(0.55, 0.25, 0.10, 0.07, 0.03)
  sample(multiples, size = n, replace = TRUE, prob = probs)
}

# Lognormal-based distribution (heavy-tailed)
draw_multiples_lognormal <- function(n, meanlog = 0, sdlog = 1.5, cap = 100) {
  x <- rlnorm(n, meanlog = meanlog, sdlog = sdlog)
  pmin(x, cap)
}

# Pareto / power-law distribution for big tails
draw_multiples_pareto <- function(n, xm = 1, alpha = 1.5, cap = 200) {
  # Inverse CDF sampling
  u <- runif(n)
  x <- xm / (u)^(1 / alpha)
  pmin(x, cap)
}

# Wrapper to choose distribution mode
draw_multiples <- function(n,
                           mode = c("discrete", "lognormal", "pareto"),
                           params = list()) {
  mode <- match.arg(mode)
  if (mode == "discrete") {
    draw_multiples_discrete(n)
  } else if (mode == "lognormal") {
    do.call(draw_multiples_lognormal, c(list(n = n), params))
  } else {
    do.call(draw_multiples_pareto, c(list(n = n), params))
  }
}

# -----------------------------
# Core fund simulation
# -----------------------------
# Simulates ONE VC fund path over time from LP & GP perspective

simulate_vc_fund <- function(
    fund_size        = 50e6,     # committed capital
    n_initial_deals  = 30,       # # of initial portfolio companies
    reserve_ratio    = 0.4,      # % of fund reserved for follow-on
    fund_life_years  = 10,
    invest_period    = 3,        # years 1..invest_period = initial deployment
    mgmt_fee_rate    = 0.02,     # 2% mgmt fee
    carry_rate       = 0.20,     # 20% carry
    hurdle_rate      = 0.08,     # 8% preferred return to LPs (simplified)
    follow_on_policy = c("none", "top_quartile"),
    dist_mode        = c("discrete", "lognormal", "pareto"),
    dist_params      = list(),
    seed             = NULL
) {
  if (!is.null(seed)) set.seed(seed)
  
  follow_on_policy <- match.arg(follow_on_policy)
  dist_mode        <- match.arg(dist_mode)
  
  years <- 0:fund_life_years
  n_years <- length(years)
  
  # -------------------------
  # 1) Capital allocation plan
  # -------------------------
  initial_pool <- fund_size * (1 - reserve_ratio)
  reserve_pool <- fund_size * reserve_ratio
  
  initial_check <- initial_pool / n_initial_deals
  
  # Investment years for initial deals (spread over invest_period)
  invest_years <- sample(1:invest_period, size = n_initial_deals, replace = TRUE)
  
  # -------------------------
  # 2) Draw outcome multiples & exit years
  # -------------------------
  base_multiples <- draw_multiples(n_initial_deals, mode = dist_mode, params = dist_params)
  
  # Exit year logic:
  # - Fails: earlier exits or write-offs
  # - Winners: later exits
  exit_years <- sapply(base_multiples, function(m) {
    if (m == 0) {
      sample(2:6, 1)  # write-off early
    } else if (m < 3) {
      sample(4:8, 1)
    } else {
      sample(6:fund_life_years, 1)
    }
  })
  
  # -------------------------
  # 3) Follow-on logic (uses reserve_pool)
  # -------------------------
  follow_on_deals <- rep(0, n_initial_deals)      # additional capital per deal
  follow_on_years <- rep(NA_integer_, n_initial_deals)
  
  remaining_reserve <- reserve_pool
  
  if (follow_on_policy == "top_quartile") {
    # Heuristic: assume we "identify" better companies from those
    # with higher base_multiples and later exit years
    # Rank deals by a simple score = multiple * (exit_year / fund_life)
    scores <- base_multiples * (exit_years / fund_life_years)
    idx <- order(scores, decreasing = TRUE)
    
    # Pick top 25% for follow-on
    n_follow <- ceiling(0.25 * n_initial_deals)
    follow_idx <- idx[1:n_follow]
    
    # Spread follow-ons over years 2..(invest_period+1)
    for (i in follow_idx) {
      if (remaining_reserve <= 0) break
      fo_year  <- min(invest_period + 1, exit_years[i] - 1)
      fo_check <- min(initial_check, remaining_reserve / 2)  # cap per deal
      
      follow_on_deals[i] <- fo_check
      follow_on_years[i] <- fo_year
      remaining_reserve  <- remaining_reserve - fo_check
    }
  }
  
  # -------------------------
  # 4) Build annual cash flows (LP perspective)
  # -------------------------
  lp_cf <- rep(0, n_years)  # LP cash flows (negative = capital out, positive = distributions)
  
  # Year 0: commitment (not actual cash, but we can track separately)
  # We'll treat capital calls as actual negative cash flows.
  
  # Capital calls for initial checks
  for (i in seq_len(n_initial_deals)) {
    y <- invest_years[i]
    idx <- which(years == y)
    lp_cf[idx] <- lp_cf[idx] - initial_check
  }
  
  # Capital calls for follow-ons
  for (i in seq_len(n_initial_deals)) {
    if (!is.na(follow_on_years[i]) && follow_on_deals[i] > 0) {
      y <- follow_on_years[i]
      idx <- which(years == y)
      lp_cf[idx] <- lp_cf[idx] - follow_on_deals[i]
    }
  }
  
  # Management fees every year on committed capital (simplified)
  for (t in seq_along(years)) {
    y <- years[t]
    if (y > 0 && y <= fund_life_years) {
      lp_cf[t] <- lp_cf[t] - (mgmt_fee_rate * fund_size)
    }
  }
  
  # Distributions from exits
  # Each deal returns: (initial_check + follow_on) * multiple at exit_year
  gross_proceeds_per_deal <- (initial_check + follow_on_deals) * base_multiples
  
  for (i in seq_len(n_initial_deals)) {
    y_exit <- exit_years[i]
    idx    <- which(years == y_exit)
    lp_cf[idx] <- lp_cf[idx] + gross_proceeds_per_deal[i]
  }
  
  # -------------------------
  # 5) Fees & carry allocation (simplified waterfall)
  # -------------------------
  # For clarity, we'll:
  # - treat mgmt fees as already deducted from LP flows (done above)
  # - compute total profit and split carry at the end
  
  total_invested_capital <- -sum(lp_cf[lp_cf < 0])
  total_distributions    <- sum(lp_cf[lp_cf > 0])
  
  gross_profit <- total_distributions - total_invested_capital
  
  # Hurdle: LP entitled to capital + hurdle before carry
  # Very simplified: applied on total invested capital over fund_life_years
  target_lp_min <- total_invested_capital * (1 + hurdle_rate)^fund_life_years
  
  gp_carry     <- 0
  lp_adjust_cf <- lp_cf
  
  if (gross_profit > 0 && total_distributions > target_lp_min) {
    # Profit above hurdle
    excess_profit <- total_distributions - target_lp_min
    gp_carry      <- carry_rate * excess_profit
    
    # Remove carry from LP IRR calculation by subtracting at final year
    final_idx <- length(lp_adjust_cf)
    lp_adjust_cf[final_idx] <- lp_adjust_cf[final_idx] - gp_carry
  }
  
  # Build GP cash flows: they receive carry at the end (ignoring GP commit here)
  gp_cf <- rep(0, n_years)
  gp_cf[length(gp_cf)] <- gp_carry
  
  # -------------------------
  # 6) Metrics
  # -------------------------
  lp_irr_gross <- irr_from_cashflows(lp_cf)
  lp_irr_net   <- irr_from_cashflows(lp_adjust_cf)
  
  lp_moic_gross <- total_distributions / total_invested_capital
  lp_moic_net   <- (total_distributions - gp_carry) / total_invested_capital
  
  list(
    years           = years,
    lp_cf_gross     = lp_cf,
    lp_cf_net       = lp_adjust_cf,
    gp_cf           = gp_cf,
    total_invested  = total_invested_capital,
    total_dist      = total_distributions,
    gp_carry        = gp_carry,
    lp_irr_gross    = lp_irr_gross,
    lp_irr_net      = lp_irr_net,
    lp_moic_gross   = lp_moic_gross,
    lp_moic_net     = lp_moic_net,
    params = list(
      fund_size       = fund_size,
      n_initial_deals = n_initial_deals,
      reserve_ratio   = reserve_ratio,
      fund_life_years = fund_life_years,
      invest_period   = invest_period,
      mgmt_fee_rate   = mgmt_fee_rate,
      carry_rate      = carry_rate,
      hurdle_rate     = hurdle_rate,
      follow_on_policy= follow_on_policy,
      dist_mode       = dist_mode,
      dist_params     = dist_params
    )
  )
}

# -----------------------------
# Monte Carlo wrapper
# -----------------------------
run_vc_monte_carlo <- function(
    n_sims          = 2000,
    verbose         = FALSE,
    ...
) {
  sims <- vector("list", n_sims)
  
  for (i in seq_len(n_sims)) {
    sims[[i]] <- simulate_vc_fund(...)
    if (verbose && i %% 100 == 0) message("Sim ", i, "/", n_sims)
  }
  
  # Extract metrics into data.frame
  df <- data.frame(
    lp_irr_gross  = sapply(sims, `[[`, "lp_irr_gross"),
    lp_irr_net    = sapply(sims, `[[`, "lp_irr_net"),
    lp_moic_gross = sapply(sims, `[[`, "lp_moic_gross"),
    lp_moic_net   = sapply(sims, `[[`, "lp_moic_net"),
    gp_carry      = sapply(sims, `[[`, "gp_carry"),
    total_invested= sapply(sims, `[[`, "total_invested"),
    total_dist    = sapply(sims, `[[`, "total_dist")
  )
  
  list(
    sims     = sims,
    summary  = df
  )
}

