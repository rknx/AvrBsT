driftSelection = function(
    p0 = 0.5, # Starting frequency = 1/1000000
    Ne = 100, # Population size = ??????
    w = c(1, 1, 1), # Finess of 3 genotypes AA, Aa, aa -> 2 types: +, -
    ngen = 400, # Number of generation -> 12
    nrep = 10, # nmber of replications -> 20
    lwd = 2,
    type = "l",
    ...
) {

    w = (w / max(w))[3:1]

    gametes = rep(0, 2 * Ne)

    if (p0 > 0) gametes[1:round(p0 * 2 * Ne)] = 1

    gametes = replicate(nrep, gametes, simplify = FALSE)

    p = lapply(gametes, mean)

    for (i in 1:ngen) {

        genotypes = lapply(
            gametes,
            x ->> matrix(
                sample(x),
                length(x) / 2,
                2
            )
        )

        fitness = lapply(
            genotypes,
            .(x, w) ->> w[rowSums(x) + 1],
            w = w
        )

        selected = lapply(
            fitness,
            .(prob, x) ->> cbind(
                sample(x, prob = prob, replace = TRUE),
                sample(x, prob = prob, replace = TRUE)
            ),
            x = Ne
        )

        copy = replicate(
            nrep,
            matrix(sample(1:2, 2 * Ne, replace = TRUE), Ne, 2),
            simplify = FALSE
        )

        gametes = mapply(
            function(g, s, c) {
                c(
                    diag(g[s[, 1], ][, c[, 1]]),
                    diag(g[s[, 2], ][, c[, 2]])
                )
            },
            g = genotypes,
            s = selected,
            c = copy,
            SIMPLIFY = FALSE
        )

        for (j in 1:nrep) p[[j]][i + 1] = mean(gametes[[j]])
    }

    plot(
        0:ngen,
        p[[1]],
        type = type,
        col = rainbow(nrep)[1],
        lwd = lwd,
        ylim = c(0, 1),
        xlab = "time (generations)",
        ylab = "f(A)"
    )

    if (nrep > 1) {
        nulo = mapply(
            lines,
            x = replicate(nrep - 1, 0:ngen, simplify = FALSE),
            y = p[2:nrep],
            col = rainbow(nrep)[2:nrep],
            lwd = 2
        )
    }

    cat(
        paste(
            "\nResult of ", nrep, "simulation(s) for", ngen, "generations of",
            "\ngenetic drift & natural selection with normalized fitnesses of:",
            "\n    W(AA) =", round(w[1], 2),
            "\n    W(Aa) =", round(w[2], 2),
            "\n    W(aa) =", round(w[3], 2),
            "\n\n"
        )
    )

    invisible(p)
}

driftSelection(
    p0 = 0.000001, # Starting frequency = 1/1000000
    Ne = 100000, # Population size = ??????
    w = c(10, 1, 1), # Finess of 3 genotypes AA, Aa, aa -> 2 types: +, -
    ngen = 12, # Number of generation -> 12
    nrep = 10, # nmber of replications -> 20
    lwd = 2,
    type = "l"
)