#'Test Statistics for Two-Way Layout Permutation Test
#'
#'\code{twlStat} computes statistics for \code{twl}. This is not meant to be
#'called directly.
#'
#'@param df a data frame with the levels of the two main factors as the first
#'  and second columns and the measurement of responses as the third column.
#'@param env an environment, to access outer scope variables.
#'
#'@return \code{twlStat} returns a list with at least 4 components:
#'  \item{\code{Fs}}{the F statistics for global test.}
#'  \item{\code{F.main1} and \code{F.main2}}{the F statistics for the first main
#'  factor and the second main factor.}
#'  \item{\code{F.int}}{the F statistic for the interaction.} In case of
#'  multiple comparisons, additional components are:
#'  \item{\code{avg} or \code{avg.main1} and \code{avg.main2}}{the mean
#'  responses for multiple
#'  comparisons.}
#'  \item{\code{d.main1sub} and \code{d.main2sub} or \code{d.main1}
#'  and \code{d.main2}}{the differences in means.}
#'  \item{\code{mad.main1sub} and \code{mad.main2sub} or \code{mad.main1} and
#'  \code{mad.main2}}{the maximum absolute
#'   differences in means.}
#'
#'
#'@references Manly, B. F. J. (2007). Randomization, bootstrap, and Monte Carlo
#'  methods in biology (3rd ed). Chapman & Hall/ CRC.
#'
#'@export
twlStat <- function(df, env = parent.frame()) {
    #global

    model <- aov(as.formula(paste(
        colnames(df)[3] ,
        "~",
        (colnames(df)[1]),
        "+" ,
        (colnames(df)[2]) ,
        "+" ,
        (colnames(df)[1]),
        ":" ,
        (colnames(df)[2])
    )),
    data = df)

    Fs <- summary(model)[[1]][4] %>% head(-1)
    F.main1 <- summary(model)[[1]]$"F value"[1]
    F.main2 <- summary(model)[[1]]$"F value"[2]
    F.int <- summary(model)[[1]]$"F value"[3]

    #multiple
    if (env$mult == TRUE){
        if (env$simple == TRUE) {
    ##subset

    splitdata <-
        df %>% split(df[, 1:2]) %>% sapply("[", 3) %>% lapply(na.omit)

    avg <- sapply(splitdata, mean)

    d.main1sub <- split(avg, rep(1:nlevels(df[, 2]),
                                  each = nlevels(df[, 1]))) %>%
        lapply(diffcalc, env$control)

    d.main2sub <- split(avg, rep(1:nlevels(df[, 1]),
                                  time = nlevels(df[, 2]))) %>%
        lapply(diffcalc, env$control)

    # mad.main1sub <- d.main1sub %>% unlist() %>% abs() %>% max() single family
    # mad.main2sub <- d.main2sub %>% unlist() %>% abs() %>% max()

    mad.main1sub <- d.main1sub %>% sapply(FUN = function(x) (max(abs(unlist(x))))) #multiple families
    mad.main2sub <- d.main2sub %>% sapply(FUN = function(x) (max(abs(unlist(x)))))

    rs <- list(
        Fs = Fs,
        F.main1 = F.main1,
        F.main2 = F.main2,
        F.int = F.int,

        avg = avg,
        d.main1sub = d.main1sub,
        mad.main1sub = mad.main1sub,
        d.main2sub = d.main2sub,
        mad.main2sub = mad.main2sub

    )

        } else {
    ##pooled

    split.main1 <-
        df %>% split(df[, 1]) %>% sapply("[", 3) %>% lapply(na.omit)
    split.main2 <-
        df %>% split(df[, 2]) %>% sapply("[", 3) %>% lapply(na.omit)

    avg.main1 <- sapply(split.main1, mean)
    avg.main2 <- sapply(split.main2, mean)

    d.main1 <- diffcalc(avg.main1, env$control)
    d.main2 <- diffcalc(avg.main2, env$control)

    mad.main1 <- d.main1 %>% abs() %>% max()
    mad.main2 <- d.main2 %>% abs() %>% max()

    rs <- list(
        Fs = Fs,
        F.main1 = F.main1,
        F.main2 = F.main2,
        F.int = F.int,

        avg.main1 = avg.main1,
        d.main1 = d.main1,
        mad.main1 = mad.main1,
        avg.main2 = avg.main2,
        d.main2 = d.main2,
        mad.main2 = mad.main2

    )
        }



    } else (

        rs <- list(
            Fs = Fs,
            F.main1 = F.main1,
            F.main2 = F.main2,
            F.int = F.int)
    )

    rs

}

