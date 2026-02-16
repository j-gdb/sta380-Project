mean <- function(x){
  n <- length(x)
  x <- x[!is.na(x)]
  
  sum(x)/n
}


bootstrap_means <- function(x, m = 5000){
  x <- x[!is.na(x)]
  n <- length(x)
  
  boot_means <- numeric(m)
  
  for(i in 1:m){
    resample <- sample(x, size = n, replace = TRUE)
    boot_means[i] <- mean(resample)
  }
  
  return(boot_means)
}

x <- rawdata$B1SD

boot_means <- bootstrap_means(x, m = 5000)

hist(boot_means,
     main = "Bootstrap Distribution of Means",
     xlab = "Bootstrap Means",
     col = "skyblue")

abline(v = mean(x), col = "red", lwd = 2)
