#' Test Statistics for One-Way Layout Permutation Test
#'
#' \code{owlStat} computes statistics for \code{owl}. This is not meant to be
#' called directly.
#'
#' @param lov a list of vectors, responses by experimental groups.
#' @param env an environment, to access outer scope variables.
#'
#' @return \code{owlStat} returns a list with 5 components: \item{\code{n}}{the
#'   sample sizes.} \item{\code{avg}}{the mean responses.} \item{\code{T}}{the T
#'   statistic for global test.} \item{\code{d.multi}}{the differences in means
#'   for multiple comparisons.} \item{\code{mad}}{the maximum absolute
#'   differences in means.}.
#'
#' @references Ernst, M. D. (2004). Permutation Methods: A Basis for Exact
#'   Inference. Statistical Science, 19(4), 676–685.
#'   \doi{10.1214/088342304000000396}.
#'
#' @export
owlStat <-
    function(lov,  env = parent.frame()) {
        #input a list of vectors

        n <- sapply(lov, length)

        avg <- sapply(lov, mean)

        T <- n  %*% avg ^ 2 %>% as.numeric()

        if (env$type.post == "all")
            d.multi <- outer(avg, avg, "-") #pairwise differences in means

        if (env$type.post == "control")
            d.multi <-
            avg[-1] - avg[1] #differences in means compared to the control

        mad <-
            d.multi %>% abs() %>% max() #max absolute differences in means

        list(
            n = n,
            avg = avg,
            T = T,
            d.multi = d.multi,
            mad = mad
        )
    }
