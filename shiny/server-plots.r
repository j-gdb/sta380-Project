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
})

compute_summary <- function(vec, statistic = "Mean", num_samples = 1000, seed = 1){
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

plot_single_hist <- function(vec, statistic, num_samples, seed, colour, label){

  vec_clean <- get_clean_numeric(vec)

  if(statistic == "Mean"){
    boot_vals <- bootstrap_means(vec_clean, num_samples, seed)
    true_val <- mean_rmna(vec_clean)
    word = "Reaction Time (milliseconds)"
  } else {
    boot_vals <- bootstrap_IQR(vec_clean, num_samples, seed)
    true_val <- IQR_rmna(vec_clean)
    word = "of Reaction Times (milliseconds)"
  }

  hist(
    boot_vals,
    col = colour,
    border = "white",
    prob = TRUE,
    main = label,
    xlab = paste(statistic, word)
  )

  abline(v = true_val, col = "black", lwd = 2)

  y_max <- max(hist(boot_vals, plot = FALSE)$density)

  text(
    x = true_val,
    y = y_max * 0.9,
    labels = paste("Observed =", round(true_val, 1)),
    pos = 4,
    cex = 0.9
  )
}

output$bootstrap_plot <- renderPlot({

  vec1 <- selected_vector()
  vec2 <- if(input$dataset2 != "None") selected_vector2() else NULL

  if(length(vec1) == 0){
    plot.new()
    text(0.5,0.5, "No data available for selected filters")
    return()
  }

  colour2 <- if(!is.null(input$colour2)) input$colour2 else "firebrick"

  stat_display <- if(input$statistic == "IQR") "IQR" else "Mean"
  word = if(input$statistic == "IQR") " of Reaction Times for the " else " Reaction Time of "

  if(!is.null(vec2) && length(vec2) > 0){

    par(mfrow = c(1, 2))

    plot_single_hist(
      vec = vec1,
      statistic = input$statistic,
      num_samples = input$resamples,
      seed = input$seed,
      colour = input$colour,
      label = paste("Bootstrap ", stat_display, word, input$dataset)
    )

    plot_single_hist(
      vec = vec2,
      statistic = input$statistic,
      num_samples = input$resamples,
      seed = input$seed,
      colour = colour2,
      label = paste("Bootstrap ", stat_display, word, input$dataset2)
    )

    par(mfrow = c(1, 1))

  } else {

    plot_single_hist(
      vec = vec1,
      statistic = input$statistic,
      num_samples = input$resamples,
      seed = input$seed,
      colour = input$colour,
      label = paste0("Bootstrap ", stat_display, word, input$dataset)
    )

  }

})

summary_data <- reactive({
  vec <- selected_vector()
  if (length(vec) == 0) return(data.frame())
  compute_summary(vec, statistic = input$statistic, num_samples = input$resamples, seed = input$seed)
})

summary_data_2 <- reactive({
  req(input$dataset2 != "None")
  vec <- selected_vector2()
  if (length(vec) == 0) return(data.frame())
  compute_summary(vec,statistic = input$statistic,num_samples = input$resamples,seed = input$seed)
})

output$summary_table <- renderTable({
  summary_data()
}, bordered = TRUE, hover = TRUE, digits = 2)

output$summary_table_2 <- renderTable({
  summary_data_2()
}, bordered = TRUE, hover = TRUE, digits = 2)

output$summary_tables <- renderUI({
  if (input$dataset2 == "None") {
    div(
      class = "card",
      style = "padding: 12px; background-color: var(--bs-secondary-bg); border-radius: 8px; margin-top: 15px; width: 100%;",
      h4("Summary of Filtered Data", style = "text-align: center;"),
      tableOutput("summary_table")
    )
  } else {
    tagList(
      div(
        class = "card",
        style = "padding: 12px; background-color: var(--bs-secondary-bg); border-radius: 8px; margin-top: 15px; width: 100%;",
        h4("Primary Dataset", style = "text-align: center;"),
        tableOutput("summary_table")
      ),
      div(
        class = "card",
        style = "padding: 12px; background-color: var(--bs-secondary-bg); border-radius: 8px; margin-top: 15px; width: 100%;",
        h4("Comparison Dataset", style = "text-align: center;"),
        tableOutput("summary_table_2")
      )
    )
  }
})
