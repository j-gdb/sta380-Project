#' Removes decimal points, strings and NA values from a vector.
#' @description Removes decimal points, characters, strings, and missing values from an inputted vector.
#' @param data the vector to clean.
#' @return A vector containing the cleaned data.
#' @examples
#' x <- c("1,000", "2,500", "", NA, 1)
#' get_clean_numeric(x)
#' @export
get_clean_numeric = function(data){
  data <- gsub(",", "", data) # i hate commas
  data[data == ""] = NA
  data = as.numeric(data)
  data <- data[!is.na(data)]
  return(data)
}

#' Compute the mean of a numeric vector after cleaning.
#' @description Converts input to numeric, removes commas, empty strings, and NAs, then computes the arithmetic mean.
#' @param data the vector to clean and average.
#' @return A numeric scalar, which is the mean of the cleaned vector.
#' @examples
#' x <- c("1,000", "2,500", "", NA, 1)
#' mean_rmna(x)
#' @export
mean_rmna <- function(data){
  data = get_clean_numeric(data)
  n = length(data)
  sum(data)/n
}

#' Compute the IQR of a numeric vector after cleaning.
#' @description Converts input to numeric, removes commas, empty strings, and NAs, then computes the IQR.
#' @param data the vector to clean and compute the IQR of.
#' @return A numeric scalar, which is the IQR of the cleaned vector.
#' @examples
#' x <- c("1,000", "2,500", "", NA, 1)
#' IQR_rmna(x)
#' @export
IQR_rmna <- function(data){
  data = get_clean_numeric(data)
  Q1 <- as.numeric(stats::quantile(data, 0.25))
  Q3 <- as.numeric(stats::quantile(data, 0.75))
  return(Q3 - Q1)
}

#' Bootstrap Sampling of the Mean.
#' @description Cleans a vector, then generates a bootstrap distribution of means by resampling with replacement.
#' @param data the vector containing the dataset we wish to compute the mean of.
#' @param num_samples the number of bootstrap samples to generate (default 5000).
#' @param seed represents the random seed (default 1)
#' @return Numeric vector of length `num_samples`, containing bootstrap means.
#' @examples
#' x <- c("1,000", "2,500", "", NA, "1,200", "1,800")
#' boot_means <- bootstrap_means(x, num_samples = 10, seed = 380)
#' boot_means
#' hist(boot_means, main = "Bootstrap Means", xlab = "Mean Value", col = "skyblue")
#' @export
bootstrap_means <- function(data, num_samples = 5000, seed = 1){
  set.seed(seed)
  data = get_clean_numeric(data)
  n = length(data)
  boot_means <- numeric(num_samples)
  for(i in 1:num_samples){
    resample <- sample(data, size = n, replace = TRUE)
    boot_means[i] <- mean(resample)
  }
  return(boot_means)
}

#' Bootstrap Sampling for IQR.
#' @description Cleans a vector, then generates a bootstrap distribution of the IQR by resampling with replacement.
#' @param data the vector containing the dataset we wish to compute the IQR of.
#' @param num_samples the number of bootstrap samples to generate (default 5000).
#' @param seed represents the random seed (default 1)
#' @return Numeric vector of length `num_samples`, containing bootstrap IQR
#' @examples
#' x <- c("1,000", "2,500", "", NA, "1,200", "1,800")
#' boot_IQR <- bootstrap_IQR(x, num_samples = 10, seed = 380)
#' boot_IQR
#' hist(boot_IQR, main = "Bootstrap IQR", xlab = "IQR Value", col = "skyblue")
#' @export
bootstrap_IQR <- function(data, num_samples = 5000, seed = 1){
  set.seed(seed)
  data = get_clean_numeric(data)
  n = length(data)
  boot_IQR <- numeric(num_samples)

  for(i in 1:num_samples){
    resample <- sample(data, size = n, replace = TRUE)
    boot_IQR[i] <- IQR_rmna(resample)
  }
  return(boot_IQR)
}
