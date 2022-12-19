#'One-Way Layout Permutation Test
#'
#'\code{owl} performs the global test and multiple comparisons for single factor
#'experiments.
#'
#'@param df a data frame with the name of experimental groups as the first
#'  column and the measurement of responses as the remaining columns.
#'@param rand an integer, the number of randomization samples. The default value
#'  is 9999.
#'@param alpha.post a numeric, the Type I error rate for multiple comparisons.
#'  The default value is 0.05.
#'@param type.post the way of multiple comparisons, "all" for pairwise
#'  comparisons or "control" for only comparisons with the control group.
#'@param seed an integer, the seed for random number generation. Setting a seed
#'  ensures the reproducibility of the result. See \code{\link{set.seed}} for
#'  more details.
#'
#'@details The first name appearing in the first column will determine the
#'  control group. The other names will be treatment groups.
#'
#'@return \code{owl} returns a list with 9 components:
#'  \item{\code{n.obs}}{the sample sizes.} \item{\code{avg.obs}}{the mean
#'  responses.} \item{\code{T.obs}}{the T statistic for global test.}
#'  \item{\code{pval}}{the p-value for global test.}
#'  \item{\code{pval.round}}{the reported form of p-value.}
#'  \item{\code{main.test}}{the strength of evidence against the null
#'  hypothesis.} \item{\code{d.multi.obs}}{the differences in means for multiple
#'  comparisons.} \item{\code{mad.cric}}{the critical value of maximum absolute
#'  differences in means.} \item{\code{post.test}}{\code{TRUE} if the
#'  differences are significant.}
#'
#'@references Ernst, M. D. (2004). Permutation Methods: A Basis for Exact
#'  Inference. Statistical Science, 19(4), 676–685.
#'  \doi{10.1214/088342304000000396}. \cr \cr Muff, S., Nilsen, E. B., O’Hara,
#'  R. B., & Nater, C. R. (2022). Rewriting results sections in the language of
#'  evidence. Trends in Ecology & Evolution, 37(3), 203–210.
#'  \doi{10.1016/j.tree.2021.10.009}.
#'
#' @examples
#' ernst2004 <- data.frame(
#' group = factor(rep(c("style1", "style2", "style3"), each = 5 ),
#' levels = c("style1", "style2", "style3")),
#' speed = c( 135,91,111,87, 122, 175,130,514,283, NA,105,147,159,107,194))
#' owl(ernst2004, type.post = "all")
#'
#'@export
owl <- function(df, rand = 9999, alpha.post = 0.05, type.post = "control", seed = 1){

    set.seed(seed)

    splitdata <- df %>% split(df[,1]) %>% sapply("[",2) %>% lapply(na.omit)

    test.stat <- owlStat(splitdata)

    n.obs <- test.stat$n
    avg.obs <- test.stat$avg
    T.obs <- test.stat$T
    d.multi.obs <- test.stat$d.multi
    mad.obs <- test.stat$mad

    pool <- splitdata %>% unlist %>% sort

    T.rand <- rep(0, rand)
    mad.rand <- rep(0, rand)

    for (i in 1:rand) {
        newlist <- split(sample(pool), rep(names(splitdata), n.obs))
        temp <- owlStat(newlist)
        T.rand[i] <- temp$T
        mad.rand[i] <-  temp$mad
    }

    pval <- (sum(T.rand >= T.obs) + 1)/ (rand + 1)

    pval.round <- case_when(pval < 0.001 ~ paste("p < 0.001"),
                           pval < 0.01 ~ paste("p =", round(pval, 3)),
                           TRUE ~ paste("p =", round(pval, 2))
    )

    main.test <- case_when(pval <= 0.001 ~ "very strong evidence",
                           pval <= 0.01 ~ "strong evidence",
                           pval <= 0.05 ~ "moderate evidence",
                           pval <= 0.1 ~ "weak evidence",
                           TRUE ~ "no evidence"
    )

    mad.cric <- quantile(mad.rand, 1-alpha.post)    #controlling family-wise error rate

    if (type.post == "all") post.test <- d.multi.obs >= mad.cric #where the differences (if any) come from
    if (type.post == "control") post.test <- abs(d.multi.obs) >= mad.cric

    list(
         n.obs=n.obs,
         avg.obs=avg.obs,
         T.obs=T.obs,
         pval=pval,
         pval.round=pval.round,
         main.test=main.test,
         d.multi.obs=d.multi.obs,
         # mad.obs=mad.obs,
         mad.cric=mad.cric,
         post.test=post.test
    )
}
