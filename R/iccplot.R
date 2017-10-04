#' Item Characteristic Curve Plot
#' @description
#' Plots an item characteristic curve under the two-parameter logistic model.
#' @param b a single number representing the value of item difficulty.
#' @param a a single number representing the value of item discrimination.
#' @keywords iccplot
#' @export
#' @details
#' While the theoretical range of ability is from negative infinity to positive
#' infinity, practical considerations usually limit the range of values
#' from -3 to +3.
#' @references
#' Baker, F. B., & Kim, S.-H. (2017).
#' \emph{The basics of item response theory using R.}
#' New York, NY: Springer.
#' ISBN-13: 978-3-319-54204-1
#' @examples
#' iccplot(0, 1)
#' iccplot(a = 1, b = 0)
#' iccplot(0, 1); par(new = TRUE); iccplot(-1.5, 1)

iccplot <- function(b, a) {
  par(lab=c(7,3,3))
  theta <- seq(-3, 3, .1)
  P <- 1 / (1 + exp(-a * (theta - b)))
  plot(theta, P, type="l", xlim=c(-3,3), ylim=c(0,1),
       xlab="Ability", ylab="Probability of Correct Response")
}
