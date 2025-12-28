library(shiny)
library(ggplot2)
library(dplyr)

# Source your core simulator
source("vc_fund_sim_core.R")

ui <- fluidPage(
  titlePanel("VC Portfolio Monte Carlo Lab"),
  
  sidebarLayout(
    sidebarPanel(
      numericInput("fund_size",
                   "Fund size (in $ millions)",
                   value = 50,
                   min   = 5,
                   step  = 5),
      
      numericInput("n_deals",
                   "# of initial deals",
                   value = 30,
                   min   = 5,
                   step  = 5),
      
      sliderInput("reserve_ratio",
                  "Reserve for follow-ons (fraction of fund)",
                  min   = 0,
                  max   = 0.7,
                  value = 0.4,
                  step  = 0.05),
      
      sliderInput("n_sims",
                  "Number of Monte Carlo simulations",
                  min   = 500,
                  max   = 5000,
                  value = 2000,
                  step  = 500),
      
      selectInput("follow_on_policy",
                  "Follow-on policy",
                  choices = c("None" = "none",
                              "Top quartile winners" = "top_quartile"),
                  selected = "top_quartile"),
      
      selectInput("dist_mode",
                  "Outcome distribution",
                  choices = c("Discrete buckets" = "discrete",
                              "Lognormal (heavy tail)" = "lognormal",
                              "Pareto power-law"       = "pareto"),
                  selected = "discrete"),
      
      helpText("Tip: start with discrete, 30 deals, 40% reserve, 2000 sims."),
      
      actionButton("run_btn", "Run simulation")
    ),
    
    mainPanel(
      h4("Summary statistics"),
      verbatimTextOutput("summaryText"),
      
      h4("Probability of exceeding MOIC thresholds"),
      tableOutput("probTable"),
      
      tabsetPanel(
        tabPanel("Net MOIC distribution",
                 plotOutput("moicPlot", height = "350px")),
        tabPanel("Net IRR distribution",
                 plotOutput("irrPlot", height = "350px")),
        tabPanel("J-Curve (LP net cash flows)",
                 plotOutput("jcurvePlot", height = "350px"))
      )
    )
  )
)

server <- function(input, output, session) {
  
  sim_results <- reactiveVal(NULL)
  jcurve_summary <- reactiveVal(NULL)
  
  observeEvent(input$run_btn, {
    fund_size_dollars <- input$fund_size * 1e6
    
    # Run full MC so we get sims & summary
    res <- run_vc_monte_carlo(
      n_sims          = input$n_sims,
      fund_size       = fund_size_dollars,
      n_initial_deals = input$n_deals,
      reserve_ratio   = input$reserve_ratio,
      follow_on_policy= input$follow_on_policy,
      dist_mode       = input$dist_mode
    )
    
    sims <- res$sims
    df   <- res$summary
    
    # Build J-curve summary per year
    paths <- do.call(rbind, lapply(seq_along(sims), function(i) {
      sim <- sims[[i]]
      data.frame(
        sim_id    = i,
        year      = sim$years,
        lp_cf_net = sim$lp_cf_net
      )
    }))
    
    paths <- paths %>%
      group_by(sim_id) %>%
      arrange(year, .by_group = TRUE) %>%
      mutate(cum_cf = cumsum(lp_cf_net)) %>%
      ungroup()
    
    jc <- paths %>%
      group_by(year) %>%
      summarise(
        median_cum = median(cum_cf),
        p25_cum    = quantile(cum_cf, 0.25),
        p75_cum    = quantile(cum_cf, 0.75),
        p10_cum    = quantile(cum_cf, 0.10),
        p90_cum    = quantile(cum_cf, 0.90),
        .groups = "drop"
      )
    
    sim_results(df)
    jcurve_summary(jc)
  })
  
  # ---- Summary text ----
  output$summaryText <- renderPrint({
    df <- sim_results()
    if (is.null(df)) {
      cat("Click 'Run simulation' to generate results.\n")
      return()
    }
    
    df_clean <- subset(df, !is.na(lp_irr_net))
    
    mean_moic <- mean(df_clean$lp_moic_net)
    med_moic  <- median(df_clean$lp_moic_net)
    mean_irr  <- mean(df_clean$lp_irr_net)
    med_irr   <- median(df_clean$lp_irr_net)
    
    cat("Net LP Metrics (across simulations)\n")
    cat("----------------------------------\n")
    cat("Mean net MOIC :", round(mean_moic, 2), "\n")
    cat("Median net MOIC:", round(med_moic, 2), "\n\n")
    cat("Mean net IRR  :", round(mean_irr * 100, 1), "%\n")
    cat("Median net IRR:", round(med_irr * 100, 1), "%\n")
  })
  
  # ---- Probabilities table ----
  output$probTable <- renderTable({
    df <- sim_results()
    if (is.null(df)) return(NULL)
    
    df_clean <- subset(df, !is.na(lp_irr_net))
    thresholds <- c(1.5, 2, 3, 5)
    
    probs <- sapply(thresholds, function(t) mean(df_clean$lp_moic_net >= t))
    
    data.frame(
      MOIC_threshold = thresholds,
      prob_net_MOIC_ge_threshold = round(probs, 3)
    )
  })
  
  # ---- MOIC histogram ----
  output$moicPlot <- renderPlot({
    df <- sim_results()
    if (is.null(df)) return(NULL)
    df_clean <- subset(df, !is.na(lp_irr_net))
    
    ggplot(df_clean, aes(x = lp_moic_net)) +
      geom_histogram(
        bins = 60,
        aes(y = after_stat(density)),
        fill = "#2C7BE5",
        alpha = 0.7
      ) +
      geom_density(color = "black", linewidth = 1.1) +
      labs(
        title = "Net LP MOIC Distribution",
        x = "Net MOIC",
        y = "Density"
      ) +
      theme_minimal(base_size = 14)
  })
  
  # ---- IRR histogram ----
  output$irrPlot <- renderPlot({
    df <- sim_results()
    if (is.null(df)) return(NULL)
    df_clean <- subset(df, !is.na(lp_irr_net))
    
    ggplot(df_clean, aes(x = lp_irr_net)) +
      geom_histogram(bins = 60, fill = "#6FA8FF", alpha = 0.7) +
      labs(
        title = "Net LP IRR Distribution",
        x = "Net IRR",
        y = "Frequency"
      ) +
      theme_minimal(base_size = 14)
  })
  
  # ---- J-curve plot ----
  output$jcurvePlot <- renderPlot({
    jc <- jcurve_summary()
    if (is.null(jc)) return(NULL)
    
    ggplot() +
      geom_ribbon(data = jc,
                  aes(x = year, ymin = p10_cum, ymax = p90_cum),
                  fill = "#D0E2FF", alpha = 0.5) +
      geom_ribbon(data = jc,
                  aes(x = year, ymin = p25_cum, ymax = p75_cum),
                  fill = "#6FA8FF", alpha = 0.6) +
      geom_line(data = jc,
                aes(x = year, y = median_cum),
                color = "black", linewidth = 1.2) +
      labs(
        title = "VC Fund J-Curve (LP Net Cash Flows)",
        x = "Year",
        y = "Cumulative net cash flow to LPs (USD)"
      ) +
      theme_minimal(base_size = 14)
  })
}

shinyApp(ui, server)
