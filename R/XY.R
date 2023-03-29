#'Permutation Test for Correlation Between Paired Samples
#'
#'\code{XY} performs permutation test on correlation coefficients.
#'
#'@param a a numeric vector, the first variable.
#'@param b a numeric vector, the second variable.
#'@param rand an integer, the number of randomization samples. The default value
#'  is 9999.
#'@param seed an integer, the seed for random number generation. Setting a seed
#'  ensures the reproducibility of the result. See \code{\link{set.seed}} for
#'  more details.
#'@param method correlation coefficient, "pearson", "kendall", or "spearman".
#'
#'@return \code{XY} returns an one-row data frame with 2 columns:
#'  \item{\code{cor}}{the correlation coefficient.} \item{\code{pval}}{the
#'  p-value.}
#'
#'@seealso \code{\link{cor.test}}
#'
#' @examples
#' with(subset(Cu, run == "Jan"), XY(sediment, porewater))
#'
#'@export
XY <- function(a, b, rand = 9999, seed = 1, method = c("pearson", "kendall", "spearman")) {

    set.seed(seed)

    cor_obs <- cor(a, b, method = method)

    cor_rand <- rep(0, rand)

    for (i in 1:rand) {
        cor_rand[i] <- cor(sample(a), b, method = method)
    }

    pval <- (sum(abs(cor_rand) >= abs(cor_obs)) + 1) / (rand + 1)

    pval_format <- case_when(pval < 0.001 ~ paste("< 0.001"),
                             pval < 0.01 ~ paste(round(pval, 3)),
                             TRUE ~ paste(round(pval, 2))
    )

    data.frame(cor = round(cor_obs, 2), pval = pval_format)
}
