# Goodness of fit and model accuracy stats
gofTest = function(targ, pred, thres = 0.5) {
    if ((n = nrow(targ)) != nrow(pred)) stop("targ and pred length vary.")

    pos = pred[targ == 1]
    neg = pred[targ == 0]
    pair = length(pos) * length(neg)

    conc = do.call(sum, lapply(pos, x ->> sum(x < neg)))
    disc = do.call(sum, lapply(pos, x ->> sum(x > neg)))

    t(
        "Percent Concordance" = conc / pair,
        "Percent Discordance" = disc / pair,
        "Percent Tied" = 1 - (conc + disc) / pair,
        "Number of Pairs (mil)" = pair / 1000000,

        "Somers D" = (conc - disc) / pair,
        "Goodman - Kruskal's Gamma" = (conc - disc) / (conc + disc),
        "Kendall's Tau A" = 2 * (conc - disc) / (n * (n - 1)),

        "Kendall's Tau B" = cor(targ, pred, method = "kendall"),
        "Spearman's rho" = cor(targ, pred, method = "spearman")
    )
}

rSquared = function(.model) {
    vF = var(model.matrix(.model) %*% fixef(.model))
    vT = vF + sum(as.numeric(VarCorr(.model)))
    t(
        "R -squared (marginal)" = vF / (vT + 3.289868),
        "R-squared (conditional)" = vT / (vT + 3.289868)
    )
}

gofFinal = function(.model, ...) {
    if (! "glmerMod" %in% class(.model)) stop("Model should be class glmerMod.")
    gof = gofTest(.model@resp$y, fitted(.model))
    rsq = rSquared(.model)
    t(gof, rsq)
}