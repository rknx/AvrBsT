# Set up environment -----------------------------------------------------------

## Libraries
"lme4" %>=>% lib_install %=>% library(.., char = T)
"ggplot2" %>=>% lib_install %=>% library(.., char = T)
"pbapply" %>=>% lib_install %=>% library(.., char = T)
"car" %>=>% lib_install %=>% library(.., char = T)
"extrafont" %>=>% lib_install %=>% library(.., char = T)

## Import fonts for plots
if (!"Open Sans" %in% fonts()) {
    font_import(
        path = "/mnt/c/Users/AJ/AppData/Local/Microsoft/Windows/Fonts/",
        pattern = "opensans",
        prompt = F
    )
}



# Import data ------------------------------------------------------------------

## Load saved R objects
"/Data/data.rda" %=>% paste0(getwd(), ..) %=>% load(.., envir = globalenv())

# Final model cross validation
tss <- function(fit) {
    if (class(fit) %in% "glmerMod") targ <- fit@resp$y
    if (class(fit) %in% "glm") targ <- unname(fit$y)

    pos <- targ == 1
    neg <- targ == 0
    pair <- sum(pos) * sum(neg)

    A <- sum(fitted(fit)[pos] >= 0.5)
    C <- sum(pos) - A
    D <- sum(fitted(fit)[neg] < 0.5)
    B <- sum(neg) - D

    (A * D - B * C) / pair
}

## Create list of formula
formulae <- do.call(c, lapply(list(clust5, clust6), function(x) {
    expand.grid(x) -> pred_combn
    sapply(seq_len(nrow(pred_combn)), function(y) {
        pred_combn[y, ] %=>% unlist %=>%
            paste(.., collapse = " + ") %=>%
            paste("dis", "dir", "gene", "dir", .., sep = " + ") %=>%
            paste(.., "dis:dir", "gene:dir", "gene:dis", sep = " + ") %=>%
            paste("presence ~", ..) %=>% as.formula
    })
}))

## Models
pblapply(formulae, function(y) {
    glm(y, data = data_binom, family = binomial(link = "logit")) %=>%
        data.frame(
            paste(deparse(formula(..)[[3]], width.cutoff = 180)),
            AIC(..), BIC(..), logLik(..), tss(..)
        )
}) %=>%
    do.call(rbind, ..) %=>%
    setNames(.., c("Formula", "AIC", "BIC", "logLik", "TSS")) %->%
    db_model_main

db_model_main[order(db_model_main$BIC), ]

fit <- glmer(
    presence ~
    gene + week + dis +
        gene:week + gene:dis +
        Dew.Point.max + Humidity.mean + Wind.mean +
        Radiation.mean + Temp.2m.max +
        (1 | year / rep / id) + (1 | year / dir),
    data = data_binom,
    family = binomial(link = "logit"),
    control = glmerControl(optCtrl = list(maxfun = 10000))
)

summary(fit)
tss(fit)
Anova(fit)