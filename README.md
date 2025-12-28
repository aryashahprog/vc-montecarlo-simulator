# VC Monte Carlo Simulator

A quantitative simulation tool that models venture capital fund outcomes using Monte Carlo methods.  
This project explores how portfolio construction, reserve strategy, and outcome distributions affect LP returns, MOIC, IRR, and J-curve behavior.

---

## What This Project Does
- Simulates thousands of synthetic VC funds
- Models:
  - Initial investments
  - Follow–on strategy (e.g. top quartile strategy)
  - Fund cash flow timing
  - Management fees
  - Carry
  - LP net returns
- Produces:
  - Net + Gross MOIC distribution
  - Net + Gross IRR distribution
  - Probability of ≥3x outcomes
  - Full VC J-Curve visualization
  - Strategy comparison across different portfolio sizes

---

## Example Outputs
- Net LP MOIC Distribution
- Survival probability curve
- Strategy comparisons
- VC J-Curve fan chart

---

## Why I Built This
I’m interested in Venture Capital and FinTech.  
This project helped me understand:
- Why portfolio size matters
- Why VC returns are power-law driven
- The risk of capital write-offs
- Why J-curves exist
- How follow-on strategy impacts returns

---

## Tech
- R
- ggplot2
- dplyr
- Monte Carlo Simulation
- (coming soon) Shiny App UI

---

## Next Steps
- Deploy as an interactive Shiny App
- Allow user-configurable inputs
- Add lognormal and Pareto return modes
- Benchmark vs real-world VC data

---

If you’d like to collaborate or are interested in VC / FinTech — feel free to reach out!
