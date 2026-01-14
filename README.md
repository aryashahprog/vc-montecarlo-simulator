# VC Monte Carlo Simulator

[![Made with R](https://img.shields.io/badge/Made%20with-R-276DC3?logo=r&logoColor=white)](https://www.r-project.org/)
[![Shiny App](https://img.shields.io/badge/Live%20App-Shiny-FF3C66?logo=rstudio&logoColor=white)](https://aryashah.shinyapps.io/vcmontecarlosim/)
[![License: MIT](https://img.shields.io/badge/License-MIT-yellow.svg)](LICENSE)
[![Last Commit](https://img.shields.io/github/last-commit/aryashahprog/vc-montecarlo-simulator)](https://github.com/aryashahprog/vc-montecarlo-simulator/commits/main)

A Monte Carlo simulation engine for analyzing venture capital fund performance, including MOIC distributions, IRR outcomes, follow-on behavior, and LP J-curve cash flows, with an interactive Shiny dashboard.

---

##  What This Project Is

This project answers a simple question:

> *“If I ran thousands of VC funds with a given strategy, what would the distribution of outcomes actually look like?”*

It models a venture fund from the perspective of **LPs and the GP**:

- Initial checks across a portfolio of startups  
- Follow-on capital reserved and deployed into “top quartile” deals  
- Exit timing based on outcome size (write-offs vs small wins vs big wins)  
- Gross vs net distributions after fees & carry  
- LP & GP cash flow timelines and the classic **J-curve**

All of this is exposed through a **Shiny app** so you can play with parameters instead of reading a PDF.

---

## Methodology: Benchmark-Calibrated Exits  <!-- NEW -->

Rather than drawing exits from a single synthetic distribution, each portfolio company is simulated **independently** using a benchmark-calibrated process:

- **Power-law exit outcomes** (many failures, few large winners)  
- **Right-skewed time-to-exit** reflecting realistic venture liquidity timing  
- **Conditional follow-on decisions** based on early performance  

Exit assumptions are implemented in a dedicated calibration layer (`Rdata_calibration.R`) and wired directly into the core fund simulator.  
This separation keeps **fund mechanics** and **exit dynamics** modular and transparent.

---

## 📌 Results TL;DR

In a baseline configuration:

- Fund size: **$50M**
- 30 initial portfolio companies  
- 40% of capital reserved for follow-ons  
- Benchmark-calibrated power-law exit dynamics  

The simulator typically shows:

- **Median net LP MOIC** around **~1.8–2.2x**, with a long right tail  
- Significant dispersion across fund outcomes driven by a small number of outliers  
- A clear **J-curve** where LPs are negative for the first few years before distributions dominate  

These are **illustrative only** – not investment advice or a forecast – but they make the trade-offs around **portfolio size, reserves, and follow-ons** much more concrete.

---

## Why This Is Useful

This project is designed to demonstrate:

- **Quantitative intuition for venture**  
  - Power-law outcomes, skewed return distributions, and tail risk  
- **Ability to turn an idea into a working analytical tool**  
  - Clean, modular R code (`vc_fund_sim_core.R`)  
  - Monte Carlo engine + visualization layer + Shiny app  
- **Communication of complex topics**  
  - Visuals that LPs / PMs can understand at a glance  
  - Clear documentation and live demo

If you work in **VC, fintech, or data-driven investing**, this is the kind of tool I’d love to build more of.

---

## Core Features

### 1. VC Fund Engine (`vc_fund_sim_core.R`)

- Simulates **one full VC fund** path:
  - Committed capital, management fees, carry, hurdle
  - Initial checks & follow-ons
  - Exit years and multiples
  - LP & GP cash flows by year
- Pluggable outcome distributions:
  - Discrete “bucketed” distribution (0x, 1x, 3x, 10x, 50x, etc.)
  - Lognormal
  - Pareto (power-law style fat tails)
- Computes:
  - **Gross & net LP IRR**
  - **Gross & net LP MOIC**
  - **Total GP carry**

### 2. Monte Carlo Wrapper (`run_vc_monte_carlo()`)

- Runs **thousands of simulated funds** with the chosen parameters  
- Returns:
  - Raw simulation objects
  - A summary data frame for quick analysis and plotting

---

## Visuals

All charts are generated directly from the simulation outputs.

### Net LP MOIC – Base Histogram

![Net LP MOIC – Base Histogram](images/moic_histogram_base.png)

Shows the raw distribution of fund outcomes (net MOIC). Most funds cluster around moderate outcomes, with a clear right tail.

### Net LP MOIC – Density + Tail Shape

![Net LP MOIC – Density + ggplot](images/moic_density_ggplot.png)

A cleaner, publication-style view with density overlay to visualize skew and tail behavior.

### VC Fund J-Curve (LP Net Cash Flows)

![VC J-Curve](images/jcurve.png)

Median and quantile bands for cumulative **net** cash flows to LPs across many simulated funds – a data-driven view of the classic VC J-curve.

---

## 🖥️ Interactive Shiny App

Try the live app here:

👉 **https://aryashah.shinyapps.io/vcmontecarlosim/**

What you can do in the app:

- Adjust **fund size, portfolio size, reserve ratio**
- Switch **follow-on strategy** and outcome distribution
- See how:
  - The **MOIC distribution** shifts  
  - The **probability of hitting ≥ X net MOIC** changes  
  - The **LP J-curve** steepens or flattens

---

## Tech Stack

- **R**
- **Shiny**
- **ggplot2**
- **dplyr**
- Monte Carlo simulation techniques

---

## ▶️ Run Locally

```r
# Clone
# git clone https://github.com/aryashahprog/vc-montecarlo-simulator.git

# Open project in RStudio

# Install dependencies
install.packages(c("shiny", "ggplot2", "dplyr"))

# Run app
source("shinyapp.R")
shinyApp(ui, server
```
---

## Author
Arya Shah |
Georgia Tech – Business Administration (Finance / FinTech).

Lets connect 👉 [linkedin.com/in/aryashahcy](https://www.linkedin.com/in/aryashahcy/)

---

## License
This project is licensed under the MIT License. See LICENSE for details.
