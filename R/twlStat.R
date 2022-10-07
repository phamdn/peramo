#' Test Statistics for Two-Way Layout Permutation Test
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
    ##subset

    splitdata <-
        df %>% split(df[, 1:2]) %>% sapply("[", 3) %>% lapply(na.omit)

    avg <- sapply(splitdata, mean)

    d.main1sub <- split(avg, rep(1:nlevels(df[, 2]),
                                  each = nlevels(df[, 1]))) %>%
        lapply(diffcalc, env$type.mult)

    d.main2sub <- split(avg, rep(1:nlevels(df[, 1]),
                                  time = nlevels(df[, 2]))) %>%
        lapply(diffcalc, env$type.mult)

    # mad.main1sub <- d.main1sub %>% unlist() %>% abs() %>% max()
    # mad.main2sub <- d.main2sub %>% unlist() %>% abs() %>% max()

    mad.main1sub <- d.main1sub %>% sapply(FUN = function(x) (max(abs(unlist(x)))))
    mad.main2sub <- d.main2sub %>% sapply(FUN = function(x) (max(abs(unlist(x)))))
    ##pooled

    split.main1 <-
        df %>% split(df[, 1]) %>% sapply("[", 3) %>% lapply(na.omit)
    split.main2 <-
        df %>% split(df[, 2]) %>% sapply("[", 3) %>% lapply(na.omit)

    avg.main1 <- sapply(split.main1, mean)
    avg.main2 <- sapply(split.main2, mean)

    d.main1 <- diffcalc(avg.main1, env$type.mult)
    d.main2 <- diffcalc(avg.main2, env$type.mult)

    mad.main1 <- d.main1 %>% abs() %>% max()
    mad.main2 <- d.main2 %>% abs() %>% max()

    rs <- list(
        Fs = Fs,
        F.main1 = F.main1,
        F.main2 = F.main2,
        F.int = F.int,

        avg = avg,
        d.main1sub = d.main1sub,
        mad.main1sub = mad.main1sub,
        d.main2sub = d.main2sub,
        mad.main2sub = mad.main2sub,

        avg.main1 = avg.main1,
        d.main1 = d.main1,
        mad.main1 = mad.main1,
        avg.main2 = avg.main2,
        d.main2 = d.main2,
        mad.main2 = mad.main2

    )

    } else (

        rs <- list(
            Fs = Fs,
            F.main1 = F.main1,
            F.main2 = F.main2,
            F.int = F.int)
    )

    rs

}

