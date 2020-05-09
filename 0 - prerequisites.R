# Pipes ------------------------------------------------------------------------

## Basic pipe

`%=>%` <- function(lhs, rhs) {
    rhs <- substitute(rhs)
    if (is.symbol(rhs)) rhs <- as.call(c(rhs, quote(..)))
    if (length(rhs) == 1) rhs <- as.call(c(rhs[[1L]], quote(..)))
    eval(rhs, envir = list(.. = lhs), enclos = parent.frame())
}

## T pipe (pass 1st argument to third)
`%>=>%` <- function(lhs, rhs) {
    rhs <- substitute(rhs)
    if (is.symbol(rhs)) rhs <- as.call(c(rhs, quote(..)))
    if (length(rhs) == 1) rhs <- as.call(c(rhs[[1L]], quote(..)))
    eval(rhs, envir = list(.. = lhs), enclos = parent.frame())
    lhs
}

## Two way pipe, return 2nd argument to first
`%<=>%` <- function(lhs, rhs) {
    invisible(eval.parent(substitute(lhs <- lhs %=>% rhs)))
}

## SuppressWarning pipe
`%!=>%` <- function(lhs, rhs) {
    suppressWarnings(eval.parent(substitute(lhs %=>% rhs)))
}

## Assign operator, can be used at line endings as opposed to ->
`%->%` <- function(lhs, rhs) {
    eval.parent(substitute(rhs <- lhs))
}

# Avilable pipes: `%->%`, `%>->%`, `%<->%`, `%!->%`

# Install library if not avaiable, then load it --------------------------------

lib_install <- function(pac) if (!require(pac, char = T)) install.packages(pac)

# Set column names -------------------------------------------------------------

col_names <- function(a, b) {
    names(a) <- b
    return(a)
}