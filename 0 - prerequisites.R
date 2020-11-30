# Pipes ------------------------------------------------------------------------

## Basic pipe
`%=>%` = function(lhs, rhs) {
    rhs = substitute(rhs)
    if (is.symbol(rhs)) rhs = as.call(c(rhs, quote(..)))
    if (length(rhs) == 1) rhs = as.call(c(rhs[[1L]], quote(..)))
    eval(rhs, envir = list(.. = lhs), enclos = parent.frame())
}

## Lambda anonymous functions, Default argument value not supported yet.
`<<-` = function(lhs, rhs) {
    args = all.vars(substitute(rhs))
    pArgs = as.pairlist(setNames(replicate(length(args), quote(.)), args))
    eval.parent(call("function", pArgs, substitute(lhs)))
}

## T pipe (pass 1st argument to third)
`%>=>%` = function(lhs, rhs) {
    rhs = substitute(rhs)
    if (is.symbol(rhs)) rhs = as.call(c(rhs, quote(..)))
    if (length(rhs) == 1) rhs = as.call(c(rhs[[1L]], quote(..)))
    eval(rhs, envir = list(.. = lhs), enclos = parent.frame())
    lhs
}

## Foreach / looping operator pipe, wrapper for lapply
`%=>>%` = function(lhs, rhs) {
    eval.parent(substitute(lapply(lhs, .x ->> .x %=>% rhs)))
}

## Foreach / looping operator pipe, wrapper for lapply
`%|>%` = function(lhs, rhs) {
    seq = seq_len(nrow(lhs))
    eval.parent(substitute(lapply(seq, .x ->> lhs[.x, ] %=>% rhs)))
}

## Two way pipe, return 2nd argument to first
`%<=>%` = function(lhs, rhs) {
    invisible(eval.parent(substitute((lhs = lhs %=>% rhs))))
}

## SuppressWarning pipe
`%!=>%` = function(lhs, rhs) {
    eval.parent(substitute(suppressWarnings(suppressMessages(lhs %=>% rhs))))
}

## Assign operator, can be used at line endings as opposed to ->
`%->%` = function(lhs, rhs) {
    invisible(eval.parent(substitute((rhs = lhs))))
}

### Avilable pipes: `%>->%`, `%<->%`, `%!->%`

# Install library if not avaiable, then load it --------------------------------

libInstall = function(pac) if (!require(pac, char = T)) install.packages(pac)

# Calculate more ---------------------------------------------------------------
mathMode = function(x) {
    x %=>% match(.., unique(..)) %=>%
        tabulate %=>% which.max %=>% unique(x)[..]
}

# Equvalent of mutate function in dplyr
mutate = function(.data, ...) { # . is used to deal with partial match in ...
    .cond = vapply(substitute(...()), .x ->> deparse(.x, 500), NA_character_)
    names(.cond) = ifelse(names(.cond) == "", .cond, names(.cond))
    for (i in seq_along(.cond)) #Don't change to lappy for realtime update!
        .data[, names(.cond)[i]] = eval(
            str2lang(.cond[i]),
            envir = .data,
            enclos = parent.frame()
        )
    .data
   # lapply(.cond, .x ->> eval(str2lang(.x), envir = .data)) %=>%
   #     do.call(data.frame, list(.data[, !names(.data) %in% names(.cond)], ..))
}

# Equvalent of filter function in dplyr
filter = function(.data, ...) {
    .cond = vapply(substitute(...()), .x ->> deparse(.x, 500), NA_character_)
    .data[
        eval(
            str2lang(paste(.cond, collapse = " & ")),
            envir = .data,
            enclos = parent.frame()
        ),
    ]
}

# Equvalent of arrange function in dplyr
arrange = function(.data, ...) {
    .data[
        eval(
            substitute(order(...)),
            envir = .data,
            enclos = parent.frame()
        ), , drop = FALSE
    ]
}

# Equvalent of summarise function in dplyr
summarize = function(.data, ...) {
    .cond = vapply(substitute(...()), .x ->> deparse(.x, 500), NA_character_)
    names(.cond) = ifelse(names(.cond) == "", .cond, names(.cond))
    lapply(.cond, .x ->> eval(
        str2lang(.x),
        envir = .data,
        enclos = parent.frame()
    )) %=>%
        do.call(data.frame, ..)
}

# Custom aggragate function to append function to colnames
gather = function(.data, .formula, .fun, ...) {
    lhs = .data[, all.vars(.formula[[2]]), drop = F]
    rhs = .data[, all.vars(.formula[[3]]), drop = F]
    res = aggregate.data.frame(lhs, rhs, .fun, ...)
    setNames(res, c(names(rhs), paste(names(lhs), substitute(.fun), sep = ".")))
}

# Custom dcast function to append function to colname
cast = function(.data, .formula, .fun, .value) {
    i = seq_along(all.vars(.formula[[2]]))
    out = reshape2::dcast(.data, .formula, .fun, value.var = .value)
    setNames(out, c(
        names(out)[i],
        paste(.value, names(out)[-i], substitute(.fun), sep = ".")
    ))
}

# Custom print for long dataframe
print.data.frame = function(df) {
    if (nrow(df) > 25) {
        base::print.data.frame(head(df, 5))
        cat("----\n")
        base::print.data.frame(tail(df, 5))
    } else {
        base::print.data.frame(df)
    }
}
