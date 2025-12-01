library(rsconnect)
library(shiny)
library(quantmod)
library(PerformanceAnalytics)
library(tidyverse)
library(xts)

# --- Helper Function for Data Retrieval ---
get_returns <- function(ticker, from = "2022-01-01", to = Sys.Date()) {
  tryCatch({
    # Get data from Yahoo Finance
    data <- getSymbols(ticker, src = "yahoo", from = from, to = to, auto.assign = FALSE)
    
    # Calculate daily returns from closing prices
    daily_returns <- dailyReturn(Cl(data))
    colnames(daily_returns) <- ticker
    return(daily_returns)
  }, error = function(e) {
    
    # Return NULL on error (e.g., invalid ticker)
    return(NULL)
  })
}

# --- Monte Carlo Simulation Function ---
# Simulates future returns based on historical mean and standard deviation
monte_carlo_sim <- function(returns, n_simulations = 100, n_days_forecast = 252) {
  # Estimate historical parameters
  mu <- mean(returns)
  sigma <- sd(returns)
  
  # Get the last cumulative return value to start the forecast from
  # This is the last relative value of the historical portfolio (1 + Historical Cumulative Return)
  initial_value <- as.numeric(last(Return.cumulative(returns, geometric = TRUE))) + 1
  
  # Matrix to store simulation results: rows for days, columns for simulations
  simulated_values <- matrix(NA, nrow = n_days_forecast, ncol = n_simulations)
  
  for (i in 1:n_simulations) {
    # Generate random daily returns using a normal distribution
    daily_returns <- rnorm(n_days_forecast, mean = mu, sd = sigma)
    # Calculate the cumulative value path, starting from the historical final value
    path <- initial_value * cumprod(1 + daily_returns)
    simulated_values[, i] <- path
  }
  
  # Return both the absolute paths and the initial value 
  return(list(simulated_values = simulated_values, initial_value = initial_value))
}


# --- UI Definition ---
ui <- fluidPage(
  titlePanel("Financial Portfolio VS Benchmark (S&P500) Simulator & Monte Carlo Forecast"),
  
  sidebarLayout(
    sidebarPanel(
      textInput("assets_text", "Tickers (Max 5, separated by a comma)", value = "AAPL,MSFT"),
      uiOutput("weights_ui"),
      dateInput("start_date", "Start Date", value = "2022-01-01"),
      numericInput("mc_sims", "Monte Carlo Simulations", value = 100, min = 10, max = 1000, step = 10),
      numericInput("mc_days", "Forecast Days (e.g., 252 for 1 year)", value = 252, min = 1),
      actionButton("calc", "Calculate & Simulate")
    ),
    mainPanel(
      tabsetPanel(
        tabPanel("Historical Performance", 
                 plotOutput("portfolio_plot"),
                 tableOutput("stats")),
        tabPanel("Monte Carlo Forecast", 
                 plotOutput("mc_plot"),
                 p(strong("Monte Carlo Interpretation:")),
                 p("The plot shows 95% confidence intervals for the simulated future portfolio value, normalized to start at 1.0 at the forecast date. The simulation assumes a Geometric Brownian Motion model based on historical volatility and return, and the solid black line represents the median (50th percentile) path.")
        )
      )
    )
  )
)

# --- Server Logic ---
server <- function(input, output, session) {
  
  # Reactive to process and validate selected tickers
  tickers_selected <- reactive({
    req(input$assets_text)
    tickers <- strsplit(input$assets_text, ",")[[1]] |> trimws() |> toupper()
    validate(need(length(tickers) > 0 && length(tickers) <= 5, "Insert 1 to 5 tickers"))
    return(tickers)
  })
  
  # Dynamically render weight inputs based on selected tickers
  output$weights_ui <- renderUI({
    req(tickers_selected())
    # Default weight is equal distribution
    default_weight <- 1 / length(tickers_selected())
    lapply(tickers_selected(), function(asset) {
      numericInput(paste0("weight_", asset), paste("Weight of", asset), 
                   value = default_weight, min = 0, max = 1, step = 0.01)
    })
  })
  
  # Load data, calculate portfolio returns, and run Monte Carlo simulation
  portfolio_data <- eventReactive(input$calc, {
    
    # 1. Data Retrieval and Portfolio Calculation
    tickers <- tickers_selected()
    returns_list <- lapply(tickers, function(ticker) get_returns(ticker, from = input$start_date))
    
    if (any(sapply(returns_list, is.null))) {
      showNotification("One or more tickers not available.", type = "error")
      return(NULL)
    }
    
    # Merge all asset returns
    returns_df <- reduce(returns_list, merge, all = FALSE)
    
    # Get weights and normalize them (ensuring they sum to 1)
    weights <- sapply(tickers, function(asset) input[[paste0("weight_", asset)]])
    weights <- weights / sum(weights)
    
    # Calculate portfolio returns
    portfolio_returns <- Return.portfolio(returns_df, weights = weights)
    
    # Load Benchmark returns
    benchmark_ret <- get_returns("^GSPC", from = input$start_date)
    if (is.null(benchmark_ret)) {
      showNotification("Impossible to load benchmark.", type = "error")
      return(NULL)
    }
    
    # Align dates for portfolio and benchmark
    common_dates <- index(portfolio_returns) %in% index(benchmark_ret)
    portfolio_returns <- portfolio_returns[common_dates]
    benchmark_ret <- benchmark_ret[index(portfolio_returns)]
    
    # 2. Monte Carlo Simulation
    # Run the MC simulation and get both paths and the initial value
    mc_results_list <- monte_carlo_sim(
      returns = portfolio_returns,
      n_simulations = input$mc_sims,
      n_days_forecast = input$mc_days
    )
    
    mc_results_absolute <- mc_results_list$simulated_values
    mc_initial_value <- mc_results_list$initial_value # The relative value at the start of the forecast
    
    mc_results_normalized <- mc_results_absolute / mc_initial_value
    
    # 3. Calculate Simulated Terminal Value (Uses absolute values for correct return calculation)
    mc_terminal_values <- mc_results_absolute[nrow(mc_results_absolute), ]
    mc_final_value <- median(mc_terminal_values)
    
    # Calculate the PURE forecast return (Return over the forecast period only)
    # Formula: ((Final Absolute Value / Initial Absolute Value) - 1) * 100
    mc_pure_forecast_return <- ((mc_final_value / mc_initial_value) - 1) * 100
    
    # Store all results, using the normalized paths for the plot
    return(list(
      portfolio = portfolio_returns, 
      benchmark = benchmark_ret, 
      mc_results = mc_results_normalized, # <- Use normalized results for the plot
      mc_total_return = mc_pure_forecast_return 
    ))
  })
  
  # --- Plot 1: Historical Cumulative Returns ---
  output$portfolio_plot <- renderPlot({
    req(portfolio_data())
    # Merge actual portfolio and benchmark returns
    combined <- merge(portfolio_data()$portfolio, portfolio_data()$benchmark, join = "inner")
    colnames(combined) <- c("Portfolio", "Benchmark (S&P500)")
    # Chart the cumulative returns
    chart.CumReturns(combined, legend.loc = "topleft", 
                     main="Portfolio Return VS Benchmark (S&P500)")
  })
  
  # --- Plot 2: Monte Carlo Simulation Paths ---
  output$mc_plot <- renderPlot({
    req(portfolio_data())
    mc_sims <- portfolio_data()$mc_results
    n_days <- nrow(mc_sims)
    
    # Calculate percentiles for the confidence band
    mc_df <- as.data.frame(mc_sims)
    mc_df$Day <- 1:n_days
    
    # Melt the data for ggplot
    mc_long <- mc_df |> pivot_longer(-Day, names_to = "Simulation", values_to = "Value")
    
    # Calculate median, 2.5%, and 97.5% percentiles for the confidence band
    percentiles <- mc_long |> 
      group_by(Day) |>
      summarise(
        Median = median(Value),
        Lower = quantile(Value, 0.025),
        Upper = quantile(Value, 0.975)
      )
    
    # Plot using ggplot2
    ggplot(percentiles, aes(x = Day)) +
      # Shaded area for 95% confidence interval
      geom_ribbon(aes(ymin = Lower, ymax = Upper), fill = "skyblue", alpha = 0.5) +
      # Median path
      geom_line(aes(y = Median), color = "black", linewidth = 1) +
      labs(
        title = paste("Monte Carlo Forecast: Portfolio Value after", n_days, "Days"),
        y = "Portfolio Value (Relative to Forecast Start)", 
        x = "Trading Day Forecast"
      ) +
      theme_minimal() +
      scale_y_continuous(labels = scales::percent_format(accuracy = 1))
  })
  
  # --- Statistics Table ---
  output$stats <- renderTable({
    req(portfolio_data())
    port <- portfolio_data()$portfolio
    bench <- portfolio_data()$benchmark
    mc_pure_forecast_return <- portfolio_data()$mc_total_return 
    
    n_days <- nrow(port)
    
    # Portfolio Metrics
    port_mean <- mean(port)
    port_sd <- sd(port)
    
    # Total return: (Product of (1+daily return) - 1) * 100
    port_total <- (prod(1 + port) - 1) * 100
    
    # Total Volatility (Annualized Historical Volatility): SD * sqrt(N_Days_in_a_Year) * 100
    port_total_vol <- port_sd * sqrt(252) * 100 
    port_sharpe_daily <- SharpeRatio(port, Rf = 0)[1]
    port_sharpe_annual <- as.numeric(port_sharpe_daily) * sqrt(252)
    
    # Benchmark Metrics
    bench_mean <- mean(bench)
    bench_sd <- sd(bench)
    bench_total <- (prod(1 + bench) - 1) * 100
    bench_total_vol <- bench_sd * sqrt(252) * 100
    bench_sharpe_daily <- SharpeRatio(bench, Rf = 0)[1]
    bench_sharpe_annual <- as.numeric(bench_sharpe_daily) * sqrt(252)
    
    # Risk Metrics
    alpha <- CAPM.alpha(port, bench, Rf = 0)
    beta <- CAPM.beta(port, bench, Rf = 0)
    
    # Create the data frame for the table
    stats <- data.frame(
      `Indicator` = c(
        "Mean Daily Return (%)",
        "Total Return (%)",
        "Annualized Volatility (%)",
        "Daily Sharpe Ratio",
        "Annualized Sharpe Ratio",
        "Alpha (%)",
        "Beta"
      ),
      `Portfolio` = c(
        round(port_mean * 100, 4),
        round(port_total, 2),
        round(port_total_vol, 2),
        round(port_sharpe_daily, 3),
        round(port_sharpe_annual, 3),
        round(as.numeric(alpha) * 100, 3),
        round(as.numeric(beta), 3)
      ),
      `Benchmark` = c(
        round(bench_mean * 100, 4),
        round(bench_total, 2),
        round(bench_total_vol, 2),
        round(bench_sharpe_daily, 3),
        round(bench_sharpe_annual, 3),
        "-",  
        "-"
      ),
      `MonteCarlo MedianForecast (N Days)` = c(
        "-",
        round(mc_pure_forecast_return, 2),
        "-",
        "-",
        "-",
        "-",
        "-"
      )
    )
    
    return(stats)
  }, sanitize.text.function = function(x) x) 
}

shinyApp(ui, server)