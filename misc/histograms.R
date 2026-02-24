rawdata = read.csv("rawdata.csv")
x <- rawdata$RFD

boot_means <- bootstrap_means(x, num_samples = 5000)

hist(boot_means,
     main = "Bootstrap Distribution of Means",
     xlab = "Bootstrap Means",
     col = "skyblue",
     prob = TRUE)
abline(v = mean_rmna(x), col = "red", lwd = 2)

boot_IQR <- bootstrap_IQR(x, num_samples = 5000)

hist(boot_IQR,
     main = "Bootstrap Distribution of IQR",
     xlab = "Bootstrap IQR",
     col = "lightgreen",
     prob = TRUE)

abline(v = IQR_rmna(x), col = "red", lwd = 2)

