#'Two-Way Layout Permutation Test
#'
#' @examples
#' manly2007 <- data.frame(
#' month = factor(rep(c("jun", "jul", "aug", "sep"), each = 6 ),
#' levels = c("jun", "jul", "aug", "sep")),
#' size = factor(rep(c("small", "large"), each = 3, times = 4),
#' levels = c("small", "large")),
#' consume = c( 13,242,105,182,21,7,8,59,20,24,312,68,515,488,88,460,1223,990,18,44,21,140,40,27))
#' twl(manly2007)
#'
#'@export
twl <- function(df, rand = 4999, seed = 1, mult = FALSE, type.mult = "control", alpha.mult = 0.05){

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
        mad.main1sub.rand[, i] <- temp$mad.main1sub
        mad.main2sub.rand[, i] <- temp$mad.main2sub
        mad.main1.rand[i] <- temp$mad.main1
        mad.main2.rand[i] <- temp$mad.main2 }
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
    mad.main1sub.cric <- apply(mad.main1sub.rand, 1,  quantile, 1-alpha.mult)
    mad.main2sub.cric <- apply(mad.main2sub.rand, 1,  quantile, 1-alpha.mult)
    mad.main1.cric <- quantile(mad.main1.rand, 1-alpha.mult)
    mad.main2.cric <- quantile(mad.main2.rand, 1-alpha.mult)

    # mult.test.main1sub <- lapply(stat.obs$d.main1sub, nolesser, mad.main1sub.cric)
    # mult.test.main2sub <- lapply(stat.obs$d.main2sub, nolesser, mad.main2sub.cric)
    mult.test.main1sub <- mapply(nolesser, stat.obs$d.main1sub, mad.main1sub.cric, SIMPLIFY = FALSE)
    mult.test.main2sub <- mapply(nolesser, stat.obs$d.main2sub, mad.main2sub.cric, SIMPLIFY = FALSE)
    mult.test.main1 <- abs(stat.obs$d.main1) >= mad.main1.cric
    mult.test.main2 <- abs(stat.obs$d.main2) >= mad.main2.cric

    rs <- list(
        n = n,
        avg = avg,
        Fs = Fs,


        d.main1sub = stat.obs$d.main1sub,
        mad.main1sub.cric=mad.main1sub.cric,
        mult.test.main1sub=mult.test.main1sub,

        d.main2sub = stat.obs$d.main2sub,
        mad.main2sub.cric=mad.main2sub.cric,
        mult.test.main2sub=mult.test.main2sub,

        avg.main1 = stat.obs$avg.main1,
        d.main1 = stat.obs$d.main1,
        mad.main1.cric=mad.main1.cric,
        mult.test.main1=mult.test.main1,

        avg.main2 = stat.obs$avg.main2,
        d.main2 = stat.obs$d.main2,
        mad.main2.cric=mad.main2.cric,
        mult.test.main2=mult.test.main2

    )
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
