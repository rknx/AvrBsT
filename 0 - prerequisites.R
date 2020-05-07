################### Pipes ###################
`%=>%` <- function(lhs, rhs) {
    eval(substitute(rhs), envir = list(. = lhs), enclos = parent.frame())
}

`%>=>%` <- function(lhs, rhs) {
    eval(substitute(rhs), envir = list(. = lhs), enclos = parent.frame())
    lhs
}

`%<=>%` <- function(lhs, rhs) {
    print(substitute)
    invisible(eval.parent(substitute(lhs <- lhs %=>% rhs)))
}

`%!=>%` <- function(lhs, rhs) {
    suppressWarnings(eval.parent(substitute(lhs %=>% rhs)))
}