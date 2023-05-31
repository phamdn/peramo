#'Permutation Test for Two-Way Layout with Extra Factors
#'
#'\code{tw_complex} performs the permutation test for ANOVA of two-factor
#'experiments with complex design.
#'
#'@param df a data frame with at least three columns.
#'@param res a character string, name of response variable.
#'@param mains two character strings, names of two main factors.
#'@param nested (optional) a character string, name of the nested factor.
#'@param nuis (optional) a character string, name of the nuisance factor.
#'@param seed an integer, the seed for random number generation. Setting a seed
#'  ensures the reproducibility of the result. See \code{\link{set.seed}} for
#'  more details.
#'@param rand an integer, the number of randomization samples. The default value
#'  is 1999.
#'@param emm a logical, whether to compute estimated marginal means.
#'
#'@details \code{res}, \code{mains}, \code{nested}, and \code{nuis} refer to
#'  column names in \code{df}. While \code{nuis} column must be a numeric
#'  vector, \code{mains} and \code{nested} columns must be factors. \code{res}
#'  can be a numeric or logical vector. \cr \cr \code{tw_complex} currently
#'  support linear models with only \code{mains}, generalized linear
#'  mixed-effects models with \code{mains} and \code{nested}, and linear
#'  mixed-effects models with \code{mains}, \code{nested}, and \code{nuis}.
#'
#'@return \code{tw_complex} returns a list with 3 main components:
#' \item{\code{lm},
#'  \code{glmer}, or \code{lmer}}{model results.}
#'  \item{\code{anova}}{anova table.} \item{\code{perm}}{permutation test
#'  results with F-statistics, p-values, and strength of evidence.}
#'
#'@seealso \code{\link{lm}}, \code{\link{glmer}}, and \code{\link{lmer}}.
#'
#'@references Manly, B. F. J. (2007). Randomization, bootstrap, and Monte Carlo
#'  methods in biology (3rd ed). Chapman & Hall/ CRC. \cr \cr Ernst, M. D.
#'  (2004). Permutation Methods: A Basis for Exact Inference. Statistical
#'  Science, 19(4), 676–685. \doi{10.1214/088342304000000396}. \cr \cr Anderson,
#'  M., & Braak, C. T. (2003). Permutation tests for multi-factorial analysis of
#'  variance. Journal of Statistical Computation and Simulation, 73(2), 85–113.
#'  \doi{10.1080/00949650215733}.
#'
#' @examples
#' \donttest{
#' tw_complex(df = subset(ctm_Cu, run == "Jan",
#' select = c("copper", "temp", "sediment")),
#' res = "sediment",
#' mains = c("copper", "temp"))
#' }
#'
#' #might take more than 5s in some machines
#'
#'@export
#'
tw_complex <- function(df, res, mains, nested, nuis, seed = 1, rand = 1999, emm = FALSE){

    set.seed(seed)

    skip <- 0
    emm_obs <- NULL

    #exp_unit
    if (!is.logical(df[, res]) & missing(nested) & missing(nuis)) {

        TS_calc_lm <- function(dataframe, response, main_factors){

            model <- suppressMessages(
                lm(
                    # as.formula(
                    paste(
                        response, "~" , main_factors[1], "*" , main_factors[2]
                    )
                    # )
                    , data = dataframe)
            )

            result <- list(
                # lm = summary(model),
                lm = model,
                anova = anova(model)
            )

            result
        }

        TS_obs <- TS_calc_lm(df, res, mains)
        F_obs <- TS_obs$anova[c(mains, paste(mains[1],":",mains[2], sep = "")), "F value"]

        F_rand <- matrix(rep(NA, rand*3), nrow = 3)

        for (i in 1:rand) {

            res_new <- sort(df[, res], na.last = TRUE) #na.last to deal with missing res
            df_new <- data.frame(
                df[mains],
                sample(res_new)
            ); names(df_new) <- c(mains, res)

            TS_new <- TS_calc_lm(df_new, res, mains)
            F_new <- TS_new$anova[c(mains, paste(mains[1],":",mains[2], sep = "")), "F value"]
            F_rand[, i] <- F_new
        }

    }
    #obs_unit survival
    if (is.logical(df[, res]) & !missing(nested) & missing(nuis)) {

        TS_calc_glmer <- function(dataframe, response, main_factors, nested_factor){

            model <-
                suppressMessages(
                    glmer(
                        as.formula(
                            paste(
                                response, "~" , main_factors[1], "*" , main_factors[2], "+(1|" , nested_factor, ")"
                            )
                        ), data = dataframe, family = binomial)
                )

            result <- list(
                # glmer = suppressWarnings(summary(model)),
                glmer = model,
                anova = anova(model)
            )

            result
        }

        TS_obs <- TS_calc_glmer(df, res, mains, nested)
        F_obs <- TS_obs$anova[c(mains, paste(mains[1],":",mains[2], sep = "")), "F value"]

        F_rand <- matrix(rep(NA, rand*3), nrow = 3)

        nested_levels <- df[, nested] %>% unique

        for (i in 1:rand) {
            nested_levels_shuffle <- sample(nested_levels)

            location <- sapply(nested_levels_shuffle, function(x){
                which(df[, nested] == x)
            })

            df_new <- data.frame(
                df[, nested][location],
                df[mains],
                df[, res][location]
            ); names(df_new) <- c(nested, mains, res)

            skip_to_next <- FALSE #in case of glmer error
            TS_new <- tryCatch(TS_calc_glmer(df_new, res, mains, nested), error = function(e) { skip_to_next <<- TRUE
                                                                                            skip <<- skip + 1
                                                                                            })
            if(skip_to_next) { next } #in case of glmer error

            F_new <- TS_new$anova[c(mains, paste(mains[1],":",mains[2], sep = "")), "F value"]
            F_rand[, i] <- F_new
        }

    }
    #obs_unit continuous
    if (!is.logical(df[, res]) & !missing(nested) & !missing(nuis)) {

        TS_calc_lmer <- function(dataframe, response, main_factors, nested_factor, nuisance_var){

            model <- suppressMessages(
                lmer(
                    as.formula(
                        paste(
                            response, "~" , main_factors[1], "*" , main_factors[2], "+(1|" , nested_factor, ")+", nuisance_var
                        )
                    ), data = dataframe)
            )

            result <- list(
                # lmer = summary(model),
                lmer = model,
                anova = anova(model)
            )

            result
        }

        TS_obs <- TS_calc_lmer(df, res, mains, nested, nuis)
        F_obs <- TS_obs$anova[c(mains, paste(mains[1],":",mains[2], sep = "")), "F value"]

        F_rand <- matrix(rep(NA, rand*3), nrow = 3)

        nested_levels <- df[, nested] %>% unique

        for (i in 1:rand) {
            nested_levels_shuffle <- sample(nested_levels)

            location <- sapply(nested_levels_shuffle, function(x){
                which(df[, nested] == x)
            })

            df_new <- data.frame(
                df[, nested][location],
                df[mains],
                df[, nuis][location],
                df[, res][location]
            ); names(df_new) <- c(nested, mains, nuis, res)

            TS_new <- TS_calc_lmer(df_new, res, mains, nested, nuis)
            F_new <- TS_new$anova[c(mains, paste(mains[1],":",mains[2], sep = "")), "F value"]
            F_rand[, i] <- F_new
        }

        if (emm) {
        #EMM

        emm_obs <- emmeans(TS_obs$lmer,
                           as.formula(paste("~" , mains[1], "*" , mains[2])))
        lmer_boot <- bootstrap_parameters(TS_obs$lmer, centrality = "mean")
        emm_boot <- emmeans(lmer_boot,
                            as.formula(paste("~" , mains[1], "*" , mains[2])))

        emm_main1 <- suppressMessages(emmeans(TS_obs$lmer, as.formula(paste("~" , mains[1]))))
        emm_main2 <- suppressMessages(emmeans(TS_obs$lmer, as.formula(paste("~" , mains[2]))))

        #ES
        sigma <- sigma(TS_obs$lmer)
        edf <- df.residual(TS_obs$lmer)
        es_main1 <- eff_size(emm_main1,
                     sigma, edf, method = "trt.vs.ctrl")
        es_main2 <- eff_size(emm_main2,
                     sigma, edf, method = "trt.vs.ctrl")
        }


    }

#report

pval <- (rowSums(F_rand >= F_obs, na.rm = TRUE) + 1) / (rand + 1)

pval_format <- case_when(pval < 0.001 ~ paste("< 0.001"),
                    pval < 0.01 ~ paste(round(pval, 3)),
                    TRUE ~ paste(round(pval, 2))
)

evidence <- case_when(pval <= 0.001 ~ "very strong",
                      pval <= 0.01 ~ "strong",
                      pval <= 0.05 ~ "moderate",
                      pval <= 0.1 ~ "weak",
                      TRUE ~ "no"
)

TS_obs$perm <- data.frame(
    Fval = round(F_obs, 2),
    # pval = pval,
    pval = pval_format,
    evidence = evidence
); row.names(TS_obs$perm) <- c(mains,paste(mains[1],":",mains[2], sep = ""))

if (skip > 0) {
    TS_obs$skip <- skip
}

if (!is.null(emm_obs)) {
    TS_obs$emm <- data.frame(as.data.frame(emm_obs)[, 1:3],
                             lower_ci = as.data.frame(emm_boot)$lower.HPD,
                             upper_ci = as.data.frame(emm_boot)$upper.HPD
                             )
    TS_obs$emm_main1 <- as.data.frame(emm_main1)[,1:2]
    TS_obs$es_main1 <- as.data.frame(es_main1)[,1:2]
    TS_obs$emm_main2 <- as.data.frame(emm_main2)[,1:2]
    TS_obs$es_main2 <- as.data.frame(es_main2)[,1:2]
    TS_obs$sigma <- sigma
}

TS_obs

}


