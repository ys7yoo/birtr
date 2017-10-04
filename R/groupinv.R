#' Group Invariance of Item Parameters
#' @description
#' Plots the item characteristic curve and the two sets of simulated observed
#' proportions of correct response from two groups under the one-, two-, or
#' three-parameter logistic model.
#' @usage
#' groupinv(mdl, t1l, t1u, t2l, t2u)
#' @param mdl 1, 2, or 3 representing the number of the model parameters.
#' @param t1l a number indicating the lower bound of ability for group 1.
#' @param t1u a number indicating the upper bound of ability for group 1.
#' @param t2l a number indicating the lower bound of ability for group 2.
#' @param t2u a number indicating the upper bound of ability for group 2.
#' @keywords groupinv
#' @import stats
#' @import graphics
#' @export
#' @details
#' While the theoretical range of ability is from negative infinity to positive
#' infinity, practical considerations usually limit the range of values
#' from -3 to +3.
#' The default values are \code{t1l = -3}, \code{t1u = -1}, \code{t2l = 1},
#' and \code{t2u = 3}.
#' With the number of item characteristic curve model parameters \code{mdl}
#' the item parameters are randomly sampled from the uniform distributions; for
#' example, under the three-parameter logistic model, \code{b} from the -3 to
#' 3 range, \code{a} from the 0.2 to 2.8 range, and \code{c} from the 0 to
#' .35 range.
#' Each of the 33 ability levels from the -3 to +3 range with .1875 interval,
#' the observed proportion of correct response is generated from the binomial
#' distribution for sample size of 21.
#' The ability levels and the observed proportions of correct response between
#' \code{t1l} and \code{t1u} are used as the group 1 data, and
#' the ability levels and the observed proportions of correct response between
#' \code{t2l} and \code{t2u} are used as the group 2 data.
#' The data from the pooled groups are used to obatin the plot that displays
#' the set of item parameters.
#' @references
#' Baker, F. B., & Kim, S.-H. (2017).
#' \emph{The basics of item response theory using R.}
#' New York, NY: Springer.
#' ISBN-13: 978-3-319-54204-1
#' @examples
#' groupinv(1) # groupinv(1, -3, -1, 1, 3)
#' groupinv(2) # groupinv(2, -3, -1, 1, 3)
#' groupinv(3) # groupinv(3, -3, -1, 1, 3)
#' groupinv(2, -2, 1, -1, 2)

groupinv <- function(mdl, t1l, t1u, t2l, t2u) {
  if (missing(t1l)) t1l <- -3
  if (missing(t1u)) t1u <- -1
  if (missing(t2l)) t2l <- 1
  if (missing(t2u)) t2u <- 3
  theta <- seq(-3, 3, .1875)
  f <- rep(21, length(theta))
  wb <- round(runif(1,-3,3), 2)
  wa <- round(runif(1,0.2,2.8), 2)
  wc <- round(runif(1,0,.35), 2)
  if (mdl == 1 | mdl == 2) { wc <- 0 }
  if (mdl == 1) { wa <- 1 }
  for (g in 1:length(theta)) {
    P <- wc + (1 - wc) / (1 + exp(-wa * (theta - wb)))
  }
  p <- rbinom(length(theta), f, P) / f
  lowerg1 <- 0
  for (g in 1:length(theta)) {
    if (theta[g] <= t1l) { lowerg1 <- lowerg1 + 1 }
  }
  upperg1 <- 0
  for (g in 1:length(theta)) {
    if (theta[g] <= t1u) { upperg1 <- upperg1 + 1 }
  }
  theta1 <- theta[lowerg1:upperg1]
  p1 <- p[lowerg1:upperg1]
  lowerg2 <- 0
  for (g in 1:length(theta)) {
    if (theta[g] <= t2l) { lowerg2 <- lowerg2 + 1 }
  }
  upperg2 <- 0
  for (g in 1:length(theta)) {
    if (theta[g] <= t2u) { upperg2 <- upperg2 + 1 }
  }
  theta2 <- theta[lowerg2:upperg2]
  p2 <- p[lowerg2:upperg2]
  theta12 <- c(theta1, theta2)
  p12 <- c(p1, p2)
  par(lab=c(7,5,3))
  plot(theta12, p12, xlim=c(-3,3), ylim=c(0,1),
       xlab="Ability", ylab="Probability of Correct Response")
  if (mdl == 1) {
    maintext <- paste("Pooled Groups", "\n", "b=", wb)
  }
  if (mdl == 2) {
    maintext <- paste("Pooled Groups","\n","a=",wa,"b=",wb)
  }
  if (mdl == 3) {
    maintext <- paste("Pooled Groups", "\n",
                      "a=", wa, "b=", wb, "c=", wc)
  }
  par(new="T")
  plot(theta, P, xlim=c(-3,3), ylim=c(0,1), type="l",
       xlab="", ylab="", main=maintext)
}


