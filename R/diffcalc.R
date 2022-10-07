#'@export
diffcalc <- function(vec, type.mult) {
    if (type.mult == "all")
        rs <- outer(vec, vec, "-")
    if (type.mult == "control")
        rs <- vec[-1] - vec[1]
    rs
}
