#'Two-Way Layout Permutation Test
#'
#'\code{twl} performs the global test and multiple comparisons for two-factor
#'experiments.
#'
#'@param df a data frame with the first
#'  and second columns containing the levels of the two main factors and the third column containing the measurement of responses.
#'@param rand an integer, the number of randomization samples. The default value
#'  is 4999.
#'@param seed an integer, the seed for random number generation. Setting a seed
#'  ensures the reproducibility of the result. See \code{\link{set.seed}} for
#'  more details.
#'@param mult a logical, whether to perform multiple comparisons.
#'@param simple a logical, whether to perform comparisons for simple effects.
#'@param control a logical, whether to perform only comparisons with the control
#'  group.
#'@param alpha a numeric, the Type I error rate for multiple comparisons. The
#'  default value is 0.05.
#'
#'@details The first levels appearing in the first and second columns will
#'  determine the control groups (if any). The other levels will be treatment
#'  groups.
#'
#'@return \code{twl} returns a list with possible components: \item{\code{n},
#'  \code{n.main1}, and \code{n.main2}}{the sample sizes.} \item{\code{avg},
#'  \code{avg.main1}, and \code{avg.main2}}{the mean responses.}
#'  \item{\code{Fs}}{the F statistics, p-values, reported form of p-value, and strength of evidence against the null
#'  hypotheses.}
#'  \item{\code{d.main1sub} and \code{d.main2sub} or \code{d.main1} and \code{d.main2}}{the differences in means for multiple
#'  comparisons.}
#'  \item{\code{mad.main1sub.cric} and \code{mad.main2sub.cric} or \code{mad.main1.cric} and \code{mad.main2.cric}}{the critical value of maximum absolute
#'  differences in means.}
#'  \item{\code{mult.test.main1sub} and \code{mult.test.main2sub} or \code{mult.test.main1} and \code{mult.test.main2}}{\code{TRUE} if the
#'  differences are significant.}
#'
#'@references Manly, B. F. J. (2007). Randomization, bootstrap, and Monte Carlo
#'  methods in biology (3rd ed). Chapman & Hall/ CRC. \cr \cr Ernst, M. D.
#'  (2004). Permutation Methods: A Basis for Exact Inference. Statistical
#'  Science, 19(4), 676–685. \doi{10.1214/088342304000000396}. \cr \cr Muff, S.,
#'  Nilsen, E. B., O’Hara, R. B., & Nater, C. R. (2022). Rewriting results
#'  sections in the language of evidence. Trends in Ecology & Evolution, 37(3),
#'  203–210. \doi{10.1016/j.tree.2021.10.009}. \cr \cr Motulsky, H. (2020).
#'  GraphPad Statistics Guide. GraphPad Software Inc.
#'  \url{https://www.graphpad.com/guides/prism/latest/statistics/index.htm}.
#'
#' @examples
#' \donttest{
#' manly2007 <- data.frame(
#' month = factor(rep(c("jun", "jul", "aug", "sep"), each = 6 ),
#' levels = c("jun", "jul", "aug", "sep")),
#' size = factor(rep(c("small", "large"), each = 3, times = 4),
#' levels = c("small", "large")),
#' consume = c( 13,242,105,182,21,7,8,59,20,24,312,68,515,488,88,460,1223,990,18,44,21,140,40,27))
#' twl(manly2007)
#' twl(manly2007, mult = TRUE, simple = TRUE, control = FALSE)
#' } #might take more than 5s in some machines
#'
#'@export
twl <- function(df, rand = 4999, seed = 1, mult = FALSE, simple = TRUE, control = TRUE, alpha = 0.05){

    set.seed(seed)

    n <- df %>% split(df[, 1:2]) %>% sapply("[", 3) %>% lapply(na.omit) %>% sapply(length)
    avg <- df %>% split(df[, 1:2]) %>% sapply("[", 3) %>% lapply(na.omit) %>% sapply(mean)

    n.main1 <- df %>% split(df[, 1]) %>% sapply("[", 3) %>% lapply(na.omit) %>% sapply(length)
    avg.main1 <- df %>% split(df[, 1]) %>% sapply("[", 3) %>% lapply(na.omit) %>% sapply(mean)

    n.main2 <- df %>% split(df[, 2]) %>% sapply("[", 3) %>% lapply(na.omit) %>% sapply(length)
    avg.main2 <- df %>% split(df[, 2]) %>% sapply("[", 3) %>% lapply(na.omit) %>% sapply(mean)

    stat.obs <- twlStat(df)

    F.main1.rand <- F.main2.rand <- F.int.rand <-

        mad.main1.rand <- mad.main2.rand <- rep(0, rand)

    mad.main1sub.rand <- matrix(rep(0, rand*nlevels(df[, 2])), nrow = nlevels(df[, 2]))
        mad.main2sub.rand <- matrix(rep(0, rand*nlevels(df[, 1])), nrow = nlevels(df[, 1]))

    response <- df[3] %>% unlist
    pool <- response %>% na.omit %>% sort

    for (i in 1:rand) {
        new.res <- rep(NA, length(response))
        new.res[!is.na(response)] <- sample(pool)
        temp <- twlStat(data.frame(df[1:2], new.res))
        F.main1.rand[i] <- temp$F.main1
        F.main2.rand[i] <- temp$F.main2
        F.int.rand[i] <- temp$F.int
        if (mult == TRUE){
            if (simple == TRUE) {
                mad.main1sub.rand[, i] <- temp$mad.main1sub
                mad.main2sub.rand[, i] <- temp$mad.main2sub
            } else {
                mad.main1.rand[i] <- temp$mad.main1
                mad.main2.rand[i] <- temp$mad.main2
            }

         }
    }

    pval.main1 <- (sum(F.main1.rand >= stat.obs$F.main1) + 1)/ (rand + 1)
    pval.main2 <- (sum(F.main2.rand >= stat.obs$F.main2) + 1)/ (rand + 1)
    pval.int <- (sum(F.int.rand >= stat.obs$F.int) + 1)/ (rand + 1)
    pval <- c(pval.main1, pval.main2, pval.int)

    pval.round <- case_when(pval < 0.001 ~ paste("p < 0.001"),
                            pval < 0.01 ~ paste("p =", round(pval, 3)),
                            TRUE ~ paste("p =", round(pval, 2))
    )

    global.test <- case_when(pval <= 0.001 ~ "very strong evidence",
                           pval <= 0.01 ~ "strong evidence",
                           pval <= 0.05 ~ "moderate evidence",
                           pval <= 0.1 ~ "weak evidence",
                           TRUE ~ "no evidence"
    )

    Fs <- data.frame(stat.obs$Fs, pval, pval.round, global.test)

    if (mult == TRUE){
        if (simple == TRUE) {
            mad.main1sub.cric <- apply(mad.main1sub.rand, 1,  quantile, 1-alpha)
            mad.main2sub.cric <- apply(mad.main2sub.rand, 1,  quantile, 1-alpha)

            # mult.test.main1sub <- lapply(stat.obs$d.main1sub, nolesser, mad.main1sub.cric)
            # mult.test.main2sub <- lapply(stat.obs$d.main2sub, nolesser, mad.main2sub.cric)
            mult.test.main1sub <- mapply(nolesser, stat.obs$d.main1sub, mad.main1sub.cric, SIMPLIFY = FALSE)
            mult.test.main2sub <- mapply(nolesser, stat.obs$d.main2sub, mad.main2sub.cric, SIMPLIFY = FALSE)

            rs <- list(
                n = n,
                avg = avg,
                Fs = Fs,


                d.main1sub = stat.obs$d.main1sub,
                mad.main1sub.cric=mad.main1sub.cric,
                mult.test.main1sub=mult.test.main1sub,

                d.main2sub = stat.obs$d.main2sub,
                mad.main2sub.cric=mad.main2sub.cric,
                mult.test.main2sub=mult.test.main2sub

            )

        } else {
            mad.main1.cric <- quantile(mad.main1.rand, 1-alpha)
            mad.main2.cric <- quantile(mad.main2.rand, 1-alpha)

            mult.test.main1 <- abs(stat.obs$d.main1) >= mad.main1.cric
            mult.test.main2 <- abs(stat.obs$d.main2) >= mad.main2.cric
            rs <- list(
                n = n,
                avg = avg,
                Fs = Fs,

                avg.main1 = stat.obs$avg.main1,
                d.main1 = stat.obs$d.main1,
                mad.main1.cric=mad.main1.cric,
                mult.test.main1=mult.test.main1,

                avg.main2 = stat.obs$avg.main2,
                d.main2 = stat.obs$d.main2,
                mad.main2.cric=mad.main2.cric,
                mult.test.main2=mult.test.main2

            )
        }

    } else {

        rs <- list(
            n = n,
            avg = avg,
            n.main1 =n.main1,
            avg.main1=avg.main1,
            n.main2 =n.main2,
            avg.main2=avg.main2,
            Fs = Fs)
    }


    rs

}
