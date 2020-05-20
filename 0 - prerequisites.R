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

## Foreach / looping operator pipe
`%=>>%` <- function(lhs, rhs) {
    rhs <- substitute(rhs)
    lapply(lhs, function(x) {
        if (is.symbol(rhs)) rhs <- as.call(c(rhs, quote(..)))
        if (length(rhs) == 1) rhs <- as.call(c(rhs[[1L]], quote(..)))
        eval(rhs, envir = list(.. = x), enclos = parent.frame())
    })
}

## Two way pipe, return 2nd argument to first
`%<=>%` <- function(lhs, rhs) {
    eval.parent(substitute(lhs <- lhs %=>% rhs))
}

## SuppressWarning pipe
`%!=>%` <- function(lhs, rhs) {
    eval.parent(substitute(suppressWarnings(lhs %=>% rhs)))
}

## Assign operator, can be used at line endings as opposed to ->
`%->%` <- function(lhs, rhs) {
    eval.parent(substitute(rhs <- lhs))
}

## Lambda operator
`<<-` <- function(lhs, rhs) {
    args <- all.vars(substitute(rhs))
    arglist <- as.pairlist(setNames(replicate(length(args), quote(expr)), args))
    eval.parent(call("function", arglist, substitute(lhs)))
}


# Avilable pipes: `%->%`, `%>->%`, `%<->%`, `%!->%`

# Install library if not avaiable, then load it --------------------------------

lib_install <- function(pac) if (!require(pac, char = T)) install.packages(pac)

# Set column names -------------------------------------------------------------
col_names <- function(a, b) {
    names(a) <- b
    return(a)
}

# Calculate more ---------------------------------------------------------------
math_mode <- function(x) {
    x %=>% match(.., unique(..)) %=>%
        tabulate %=>% which.max %=>% unique(x)[..]
}

# Remove library and its useless dependencies
remove_depends <- function(pkg, recursive = FALSE) {
    library("tools")
    d <- package_dependencies(, installed.packages(), recursive = recursive)
    depends <- if (!is.null(d[[pkg]])) d[[pkg]] else character()
    needed <- unique(unlist(d[!names(d) %in% c(pkg, depends)]))
    toRemove <- depends[!depends %in% needed]
    if (length(toRemove)) {
        toRemove <- select.list(c(pkg, sort(toRemove)),
            multiple = TRUE,
            title = "Select packages to remove"
        )
        remove.packages(toRemove)
        return(toRemove)
    } else {
        invisible(character())
    }
}