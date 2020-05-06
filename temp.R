# Dumnmy scratchbook script to quickly test code

a <- c("gene", "dpi", "dis", "rain", "wind", "temp", "rh", "dew")

vec <- c(F, F, F, T, T, T, T, T, T, T)

l <- list(c(1:2), c(3:4))

list_extract <- function(var, ind) var[[ind]]

f <- function(list) {
    lapply(list, function(v, i) v[[i]], 1)
}

function(x) {
    print("[" + x)
}

"[["(l, 1)

f(l)