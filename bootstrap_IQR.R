IQR <- function(x){
  x <- x[!is.na(x)]
  
  Q1 <- quantile(x, 0.25)
  Q3 <- quantile(x, 0.75)
  
  Q3-Q1
}


bootstrap_IQR <- function(x, m = 5000){
  x <- x[!is.na(x)]
  n <- length(x)
  
  boot_IQR <- numeric(m)
  
  for(i in 1:m){
    resample <- sample(x, size = n, replace = TRUE)
    boot_IQR[i] <- IQR(resample)
  }
  
  return(boot_IQR)
}

x <- rawdata$RFD

boot_IQR <- bootstrap_IQR(x, m = 5000)

hist(boot_IQR,
     main = "Bootstrap Distribution of IQR",
     xlab = "Bootstrap IQR",
     col = "lightgreen")

abline(v = IQR(x), col = "red", lwd = 2)
