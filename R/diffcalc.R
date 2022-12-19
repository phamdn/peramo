#' Calculate the Differences between Means
#'
#' @param vec a numeric vector, the mean responses.
#' @param control a logical, whether the control group exists.
#'
#'@export
diffcalc <- function(vec, control) {
    if (control == FALSE)
        rs <- outer(vec, vec, "-")
    if (control == TRUE)
        rs <- vec[-1] - vec[1]
    rs
}
