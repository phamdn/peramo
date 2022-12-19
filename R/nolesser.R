#' Compare the Differences with Critical Values
#'
#' @param obs a numeric, the observed difference.
#' @param cric a numeric, the critical values of maximum absolute differences.
#'
#'@export
nolesser <- function(obs, cric) {
    abs(obs) >= cric
}

