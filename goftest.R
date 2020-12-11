# Set up environment -----------------------------------------------------------

## Libraries
"lme4" %>=>% libInstall %!=>% library(.., char = T)

# Goodness of fit and model accuracy stats -------------------------------------

## Association measures
gofCalc = function(targ, pred, thres = 0.5, ...) {
    if ((n = length(targ)) != length(pred)) stop("targ and pred length vary.")

    pos = pred[targ == 1]
    neg = pred[targ == 0]
    pair = length(pos) * length(neg)

    conc = do.call(sum, lapply(pos, x ->> sum(x > neg)))
    disc = do.call(sum, lapply(pos, x ->> sum(x < neg)))

    data.frame(check.names = FALSE,
        "Percent Concordance" = conc / pair,
        "Percent Discordance" = disc / pair,
        "Percent Tied" = 1 - (conc + disc) / pair,
        "Number of Pairs (mil)" = pair / 1000000,
        "Somers D" = (conc - disc) / pair,
        "Goodman - Kruskal's Gamma" = (conc - disc) / (conc + disc),
        "Kendall's Tau A" = (2 * (conc - disc)) / (n * (n - 1)),
        "Kendall's Tau C" = 4 * (conc - disc) / n^2,
        "Spearman's rho" = cor(targ, pred, method = "spearman")
    )
}

## Pseudo-r-squared values
rSquared = function(.model) {
    vF = var(model.matrix(.model) %*% fixef(.model))
    vT = vF + sum(as.numeric(VarCorr(.model)))

    cl = getCall(.model)
    cl$formula = reformulate(
        sapply(findbars(formula(.model)), x ->> paste0("(", deparse(x), ")")),
        response = formula(.model)[[2]]
    )
    eval(cl) %=>% fixef %=>% unname %->% fN
    eval(cl) %=>% VarCorr %=>% as.numeric %=>% sum %->% vN
    pM = family(.model)$linkinv(
        fN - vN / 2 * tanh(fN * (1 + 2 * exp(-0.5 * vN)) / 6)
    )
    data.frame(check.names = FALSE,
        "R-squared (theoretical, marginal)" = vF / (vT + pi ^ 2 / 3),
        "R-squared (theoretical, conditional)" = vT / (vT + pi ^ 2 / 3),
        "R-squared (delta, marginal)" = vF / (vT + 1 / (pM * (1 - pM))),
        "R-squared (delta, conditional)" = vT / (vT + 1 / (pM * (1 - pM)))
    )
}

## Main wrapper function
gofTest = function(.model, ...) {
    if (! "glmerMod" %in% class(.model)) stop("Model should be class glmerMod.")
    table = data.frame()
    gof = gofCalc(targ = .model@resp$y, pred = fitted(.model), ...)
    rsq = rSquared(.model)
    table = cbind(gof, rsq, row.names = "Final")
    format(as.data.frame(t(round(table, 4))), scientific = F, digits = 4)
}