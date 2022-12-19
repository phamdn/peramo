#' Two-Group Permutation Test
#'
#'\code{AB} performs A/B testing for two-group experiments.
#'
#' @param a the measurement of responses of the first group.
#' @param b the measurement of responses of the second group.
#' @param rand an integer, the number of randomization samples. The default value
#'  is 9999.
#' @param seed an integer, the seed for random number generation. Setting a seed
#'  ensures the reproducibility of the result. See \code{\link{set.seed}} for
#'  more details.
#'
#'@return \code{AB} returns an one-row data frame with 6 columns:
#'  \item{\code{nA}}{the sample size of the first group.}
#'  \item{\code{mean.A}}{the mean responses of the first group.}
#'  \item{\code{nB}}{the sample size of the second group.}
#'  \item{\code{mean.B}}{the mean responses of the second group.}
#'  \item{\code{mean.dif}}{the difference between two mean responses.}
#'  \item{\code{pval}}{the p-value.}
#'
#'@references Ernst, M. D. (2004). Permutation Methods: A Basis for Exact
#'  Inference. Statistical Science, 19(4), 676–685.
#'  \doi{10.1214/088342304000000396}.
#'
#' @examples
#' AB(c(19, 22, 25, 26), c (23, 33, 40))
#'
#'@export
AB <- function(a, b, rand = 9999, seed = 1) {

    set.seed(seed)

    A <- a %>% na.omit()
    B <- b %>% na.omit()

    nA <- length(A)
    mean.A <- mean(A)
    nB <- length(B)
    mean.B <- mean(B)

    mean.dif <- mean.A - mean.B

    drand <- rep(0, rand)

    pool <- sort(c(A, B))

    for (i in 1:rand) {
        newlist <- split(sample(pool), rep(c("A", "B"), c(nA, nB)))
        drand[i] <- mean(newlist$A) - mean(newlist$B)
    }

    pval <- (sum(abs(drand) >= abs(mean.dif)) + 1) / (rand + 1)

    data.frame(nA, mean.A, nB, mean.B, mean.dif, pval)
}
