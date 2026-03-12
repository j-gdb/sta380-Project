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

  abline(v = true_val, col = "chocolate", lwd = 2)

  y_max <- max(hist(boot_vals, plot = FALSE)$density)
  text(x = true_val, y = y_max*0.9,
       labels = paste("Observed", statistic, "=", round(true_val,1)),
       col = "chocolate", pos = 4, cex = 0.9)
}
