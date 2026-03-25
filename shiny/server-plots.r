# we should get arrested for writing code like this

filtered_data <- reactive({
  df <- rawdata
  if(input$gender != "All"){
    gender_code <- ifelse(input$gender == "Male",1,2)
    df <- df[df$gender == gender_code,]
  }
  df <- df[
    df$age_years >= input$age[1] &
      df$age_years <= input$age[2],
  ]
})

selected_vector <- reactive({
  df <- filtered_data()
  vec <- switch(input$dataset,
                "Dominant Hand" = df$RHD,
                "Non Dominant Hand" = df$RHND,
                "Dominant Foot" = df$RFD,
                "Non Dominant Foot" = df$RFND
  )
  vec
})

selected_vector2 <- reactive({
  req(input$dataset2 != "None")
  df <- filtered_data()
  vec <- switch(input$dataset2,
                "Dominant Hand" = df$RHD,
                "Non Dominant Hand" = df$RHND,
                "Dominant Foot" = df$RFD,
                "Non Dominant Foot" = df$RFND
  )
  vec
})

compute_summary <- function(vec, statistic = "Mean", num_samples = 5000, seed = 1){
  vec_clean <- get_clean_numeric(vec)

  if(length(vec_clean) == 0) return(data.frame())

  if(statistic == "Mean"){
    boot_vals <- bootstrap_means(vec_clean, num_samples = num_samples, seed = seed)
  } else {
    boot_vals <- bootstrap_IQR(vec_clean, num_samples = num_samples, seed = seed)
  }

  data.frame(
    Statistic = c("Bootstrap Mean","SD","IQR","Min","Max","95% CI Lower","95% CI Upper"),
    Value = c(
      mean(boot_vals),
      sd(boot_vals),
      IQR(boot_vals),
      min(boot_vals),
      max(boot_vals),
      quantile(boot_vals, 0.025),
      quantile(boot_vals, 0.975)
    )
  )
}

plot_bootstrap_hist <- function(x, statistic="Mean", num_samples=5000, seed=1, colour="burlywood") {
  vec_clean <- get_clean_numeric(x)
  if(statistic=="Mean"){
    boot_vals <- bootstrap_means(vec_clean, num_samples=num_samples, seed=seed)
    true_val <- mean_rmna(vec_clean)
    xlab_text <- "Bootstrap Means (milliseconds)"
  } else {
    boot_vals <- bootstrap_IQR(vec_clean, num_samples=num_samples, seed=seed)
    true_val <- IQR_rmna(vec_clean)
    xlab_text <- "Bootstrap IQR (milliseconds)"
  }

  hist(boot_vals, col=colour, border="white", prob=TRUE,
       main=paste("Bootstrap Distribution of", statistic),
       xlab=xlab_text)
  abline(v=true_val, col="black", lwd=2)
  y_max <- max(hist(boot_vals, plot=FALSE)$density)
  text(x=true_val, y=y_max*0.9, labels=paste("Observed",statistic,"=",round(true_val,1)), pos=4, col="black")
}

output$bootstrap_plot <- renderPlot({
  vec1 <- selected_vector()
  vec2 <- if(input$dataset2 != "None") selected_vector2() else NULL

  colour2 <- if (!is.null(input$colour2)) input$colour2 else "firebrick"

  if(length(vec1) == 0){
    plot.new()
    text(0.5,0.5,"No data available for selected filters")
    return()
  }

  if(!is.null(vec2) && length(vec2) > 0){
    par(mfrow = c(1,2))
    plot_bootstrap_hist(vec1, statistic = input$statistic, num_samples = input$resamples,
                        seed = input$seed, colour = input$colour)
    plot_bootstrap_hist(vec2, statistic = input$statistic, num_samples = input$resamples,
                        seed = input$seed, colour = colour2)
    par(mfrow = c(1,1))
  } else {
    plot_bootstrap_hist(vec1, statistic = input$statistic, num_samples = input$resamples,
                        seed = input$seed, colour = input$colour)
  }
})

output$summary_table <- renderTable({
  vec <- selected_vector()
  if(length(vec) == 0) return(data.frame())
  compute_summary(vec, statistic = input$statistic, num_samples = input$resamples, seed = input$seed)
}, bordered = TRUE, hover = TRUE, digits = 2)
