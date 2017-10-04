#' Rasch Model Calibration
#' @description
#' Yields estimates of item difficulty parameters and ability parameters
#' under the one-parameter logistic Rasch model by the Birnbaum paradigm.
#' @usage
#' rasch(s, f)
#' @param s a numeric vector representing the column sum for the \code{J} items.
#' @param f a numeric vector representing the frequencies for the scores from
#' 1 to \code{J}-1.
#' @keywords rasch
#' @export
#' @details
#' With data editing command lines, the item response data matrix of
#' \code{N} by \code{J} is to be converted to the two vectors of the column
#' sum \code{s} and the frequencies for the scores \code{f}.
#' The two vectors are the input for the Birnbaum paradigm to calibrate
#' the test.
#' The function contains two other required functions, \code{stage1} and
#' \code{stage2}.
#' After obtaining the item and ability parameter estimates from the Birnbaum
#' paradigm, bias correction methods are applied to the item parameter
#' estimates and then to the ability parameter estimates.
#' The estimates of item difficulty parameters \code{b} are reported in
#' the console window.
#' The estimates of ability parameters \code{theta} are not for individual
#' examinees but for the raw score groups ranged from 1 to \code{J}-1.
#' The function prints out the mean and the standard deviation of the item
#' parameter estimates as well as those of the ability parameter estimates.
#' @references
#' Baker, F. B., & Kim, S.-H. (2017).
#' \emph{The basics of item response theory using R.}
#' New York, NY: Springer.
#' ISBN-13: 978-3-319-54204-1
#' @examples
#' rm(list = ls()) # remove the existing objects in workspace
#' s <- c(13, 8, 8, 5, 10, 7, 7, 6, 7, 3)
#' f <- c(1, 2,  2, 4, 1, 1, 0, 0, 4)
#' rasch(s, f)

rasch <- function(s, f) {
  J <- length(s); G <- length(f); K <- 25; T <- 10
  b <- log((sum(f) - s) / s)
  b <- b - mean(b)
  oldb <- b
  theta <-seq(1, G, 1)
  for (g in 1:G) {theta[g] <- log(g / (J - g)) }
  for (k in 1:K) {
    convabd <- 0.01
    cat("cycle k=", k, "\n")
    b <-  stage1(b, theta, s, f)
    b <- b - mean(b)
    theta <-  stage2(theta, b)
    abd <- abs(b - oldb)
    if (sum(abd) < convabd) { break }
    else { oldb <- b }
  }
  b <- b * ((J - 1) / J)
  for (j in 1:J) {
    cat("b(", j, ")=", b[j], "\n")
  }
  cat("mean(b)=", mean(b), "\n")
  cat("sd(b)=", sd(b), "\n")
  cat("J=", J, "\n")
  theta <- stage2(theta,b)
  theta <- theta * ((J - 2) / (J - 1))
  for (g in 1:G) {
    cat("theta(", g, ")=", theta[g], "\n")
  }
  cat("mean(theta)=", mean(rep(theta, f)), "\n")
  cat("sd(theta)=", sd(rep(theta, f)), "\n")
  cat("N=", sum(f), "\n")
  cat("f=", f, "\n")
}
stage1 <- function(b, theta, s, f) {
  J <- length(b); G <- length(theta); T <- 10
  for (j in 1:J) {
    convb <- 0.01
    for (t in 1:T) {
      sumfp <- 0
      sumfpq <- 0
      for (g in 1:G) {
        p <- 1 / (1 + exp(-(theta[g] - b[j])))
        sumfp <- sumfp + f[g] * p
        sumfpq <- sumfpq + f[g] * p * (1 - p)
      }
      deltab <- (s[j] - sumfp) / sumfpq
      b[j] <- b[j] - deltab
      if (abs(deltab) < convb) { break }
    }
  }
  return(b)
}
stage2 <- function(theta, b){
  G <- length(theta); J <- length(b); T <- 10
  for (g in 1:G) {
    convt <- 0.01
    for (t in 1:T) {
      sump <- 0
      sumpq <- 0
      for (j in 1:J) {
        p <- 1 / (1 + exp(-(theta[g] - b[j])))
        sump <- sump + p
        sumpq <- sumpq - p * (1 - p)
      }
      deltat <- (g - sump) / sumpq
      theta[g] <- theta[g] - deltat
      if (abs(deltat) < convt) { break }
    }
  }
  return(theta)
}

