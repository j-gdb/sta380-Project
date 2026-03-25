# we should get arrested for writing code like this

plot_bootstrap_hist <- function(x, statistic = "Mean", num_samples = 5000, seed = 1, colour = "burlywood") {
  if(statistic == "Mean") {
    boot_vals <- bootstrap_means(x, num_samples = num_samples, seed = seed)
    true_val <- mean_rmna(x)
    xlab_text <- "Bootstrap Means"
  } else {
    boot_vals <- bootstrap_IQR(x, num_samples = num_samples, seed = seed)
    true_val <- IQR_rmna(x)
    xlab_text <- "Bootstrap IQR"
  }

  hist(
    boot_vals,
    main = paste("Bootstrap Distribution of", statistic),
    xlab = xlab_text,
    col = colour,
    prob = TRUE,
    border = "white"
  )

  abline(v = true_val, col = "black", lwd = 2)

  y_max <- max(hist(boot_vals, plot = FALSE)$density)
  text(x = true_val, y = y_max*0.9,
       labels = paste("Observed", statistic, "=", round(true_val,1)),
       col = "black", pos = 4, cex = 0.9)
}

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

output$bootstrap_plot <- renderPlot({
  vec <- selected_vector()
  if(length(vec) == 0){
    plot.new()
    text(0.5,0.5,"No data available for selected filters")
    return()
  }
  plot_bootstrap_hist(
    x = vec,
    statistic = input$statistic,
    num_samples = input$resamples,
    seed = input$seed,
    colour = input$colour
  )
})

output$summary_table <- renderTable({

  df <- filtered_data()

  vec <- switch(input$dataset,
                "Dominant Hand" = df$RHD,
                "Non Dominant Hand" = df$RHND,
                "Dominant Foot" = df$RFD,
                "Non Dominant Foot" = df$RFND)

  vec_clean <- get_clean_numeric(vec)

  if(input$statistic == "Mean"){
    boot_vals <- bootstrap_means(
      vec_clean,
      num_samples = input$resamples,
      seed = input$seed
    )

    data.frame(
      Statistic = c("Bootstrap Mean", "Min", "Max"),
      Value = c(mean(boot_vals), min(boot_vals), max(boot_vals))
    )

  } else {

    boot_vals <- bootstrap_IQR(
      vec_clean,
      num_samples = input$resamples,
      seed = input$seed
    )

    data.frame(
      Statistic = c("Bootstrap IQR", "Min", "Max"),
      Value = c(mean(boot_vals), min(boot_vals), max(boot_vals))
    )
  }

}, bordered = TRUE, hover = TRUE, digits = 2)
