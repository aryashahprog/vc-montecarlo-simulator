# VC Monte Carlo Simulator

A Monte Carlo simulation engine for modeling venture capital fund outcomes.  
This tool simulates portfolio construction, reserve strategies, follow-on behavior, and exit distributions to help understand LP net returns, MOIC distributions, and the classic VC J-Curve.

---

## Interactive Shiny App

Try the live simulation dashboard here:  
👉 https://aryashah.shinyapps.io/vcmontecarlosim/

---

## Key Visualizations

### Net LP MOIC Distribution (Base R)
Shows simulated distribution of LP net returns across 3,000 Monte Carlo simulations.

<img src="images/moic_histogram_base.png" width="700">

---

### Net LP MOIC Distribution (Smoothed — ggplot2)
Adds density smoothing to visualize right-tail behavior and multimodal structure.

<img src="images/moic_density_ggplot.png" width="700">

---

### VC Fund J-Curve (LP Net Cash Flows)
Median + quantile confidence bands across 1,000 simulated funds.

<img src="images/jcurve.png" width="700">

---

## What This Project Explores

✔️ How portfolio size affects return distribution  
✔️ The impact of reserve strategy (40% allocated to follow-ons here)  
✔️ Top-quartile follow-on strategy dynamics  
✔️ Skew and right-tail risk in VC  
✔️ LP experience through the J-Curve

---

## Tech Stack

- **R**
- **Shiny**
- **ggplot2 / dplyr**
- **Monte Carlo simulation**

---

## Author

Arya Shah  
Georgia Tech — Business Administration (Finance / FinTech)  
Linkedin: linkedin.com/in/aryashahcy

---

If you’d like to collaborate or are interested in VC / FinTech — feel free to reach out!
