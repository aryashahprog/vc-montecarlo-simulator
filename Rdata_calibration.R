# ============================================================
# Benchmark-Calibrated Assumptions for VC Monte Carlo Simulator
# ============================================================
# This file defines realistic, data-anchored assumptions for
# venture outcomes. These are illustrative, not predictive.
# ============================================================


# ---------------------------
# Exit outcome buckets
# ---------------------------

exit_buckets <- data.frame(
  bucket = c("fail", "small", "medium", "outlier"),
  prob = c(0.60, 0.25, 0.12, 0.03),
  min_multiple = c(0.0, 1.0, 3.0, 10.0),
  max_multiple = c(1.0, 3.0, 10.0, 50.0),
  stringsAsFactors = FALSE
)


# ---------------------------
# Time-to-exit parameters
# ---------------------------

time_to_exit_params <- list(
  meanlog = log(7),   # ~7 year average
  sdlog = 0.6         # right-skewed distribution
)


# ---------------------------
# Follow-on probabilities
# ---------------------------

follow_on_probabilities <- list(
  fail = 0.05,
  small = 0.30,
  medium = 0.60,
  outlier = 0.85
)


# ============================================================
# Sampling Functions
# ============================================================

# ---------------------------
# Sample exit outcome (bucket + multiple)
# ---------------------------

sample_exit_outcome <- function() {
  
  bucket <- sample(
    exit_buckets$bucket,
    size = 1,
    prob = exit_buckets$prob
  )
  
  row <- exit_buckets[exit_buckets$bucket == bucket, ]
  
  multiple <- runif(
    n = 1,
    min = row$min_multiple,
    max = row$max_multiple
  )
  
  list(
    bucket = bucket,
    multiple = multiple
  )
}


# ---------------------------
# Sample time to exit (years)
# ---------------------------

sample_exit_time <- function() {
  rlnorm(
    n = 1,
    meanlog = time_to_exit_params$meanlog,
    sdlog = time_to_exit_params$sdlog
  )
}


# ---------------------------
# Follow-on decision
# ---------------------------

should_follow_on <- function(bucket) {
  runif(1) < follow_on_probabilities[[bucket]]
}


# ============================================================
# Simulate a single investment
# ============================================================

simulate_investment <- function() {
  
  exit <- sample_exit_outcome()
  exit_time <- sample_exit_time()
  follow_on <- should_follow_on(exit$bucket)
  
  list(
    outcome_bucket = exit$bucket,
    exit_multiple = exit$multiple,
    exit_time_years = exit_time,
    follow_on = follow_on
  )
}

