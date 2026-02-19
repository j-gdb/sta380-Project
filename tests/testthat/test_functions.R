library(testthat)

# Test get_clean_numeric

test_that("get_clean_numeric removes commas, blanks, and NAs", {

  x <- c("1,000", "2,500", "", NA, "3")

  cleaned <- get_clean_numeric(x)

  expect_equal(cleaned, c(1000, 2500, 3))
})


# Test mean_rmna

test_that("mean_rmna computes correct mean after cleaning", {

  x <- c("1,000", "2,500", "", NA, "3")

  expect_equal(
    mean_rmna(x),
    mean(c(1000, 2500, 3))
  )
})


# Test IQR_rmna

test_that("IQR_rmna computes correct IQR after cleaning", {

  x <- c("1,000", "2,000", "", NA, "3,000", "4,000")

  expect_equal(
    IQR_rmna(x),
    IQR(c(1000, 2000, 3000, 4000))
  )
})


# Test bootstrap_means

test_that("bootstrap_means returns correct length", {

  set.seed(123)

  x <- c("1,000", "2,500", "1,200", "1,800")

  boot <- bootstrap_means(x, num_samples = 1000)

  expect_length(boot, 1000)
})


test_that("bootstrap_means has mean close to sample mean", {

  set.seed(123)

  x <- c("1,000", "2,500", "1,200", "1,800")

  boot <- bootstrap_means(x, num_samples = 2000)

  expect_equal(
    mean(boot),
    mean_rmna(x),
    tolerance = 0.01
  )
})


test_that("bootstrap_means contains no NA values", {

  set.seed(123)

  x <- c("1,000", "", NA, "2,000", "3,000")

  boot <- bootstrap_means(x, num_samples = 1000)

  expect_false(any(is.na(boot)))
})


# Test bootstrap_IQR

test_that("bootstrap_IQR returns correct length", {

  set.seed(123)

  x <- c("1,000", "2,000", "3,000")

  boot <- bootstrap_IQR(x, num_samples = 1000)

  expect_length(boot, 1000)
})


test_that("bootstrap_IQR has mean close to sample IQR", {

  set.seed(123)

  x <- c("1,000", "2,000", "3,000", "4,000", "5,000")

  boot <- bootstrap_IQR(x, num_samples = 2000)

  expect_equal(
    mean(boot),
    IQR_rmna(x),
    tolerance = 0.5
  )
})


test_that("bootstrap_IQR contains no NA values", {

  set.seed(123)

  x <- c("1,000", "", NA, "2,000", "3,000")

  boot <- bootstrap_IQR(x, num_samples = 1000)

  expect_false(any(is.na(boot)))
})

