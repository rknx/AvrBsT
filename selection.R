# Set up environment -----------------------------------------------------------

## Libraries
"lme4" %>=>% libInstall %=>% library(.., char = T)

## Message
message("***\n",
    "The formula for full model is supplied manually and may neeed to be.\n",
    "changed in function fullModel() manually.",
    "***")
    
# Likelihood ratio for model selection

## For null model
model2null = function(.model, nullVar = NULL) {
    (if (is.null(nullVar)) {
        findbars(formula(.model)) %=>%
            sapply(.., x ->> paste0("(", deparse(x), ")"))
    } else {
        str2lang(paste("1 |", nullVar)) %=>% deparse %=>% paste0("(", .., ")")
    }) %=>%
        reformulate(.., response = formula(.model)[[2]]) %=>%
        update(.model, ..)
}

## For main effect only model
model2main = function(.model) {
    formula(.model)[[3]] %=>% c(
        all.vars(nobars(..)),
        findbars(..) %=>% sapply(.., x ->> paste0("(", deparse(x), ")"))
    ) %=>%
    reformulate(.., response = formula(.model)[[2]]) %=>%
    update(.model, ..)
}

## For final model with new dataset
reModel = function(.model) {
    glmer(
        formula(.model),
        na.omit(model.frame(.model)),
        family(.model),
        glmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 2e5))
    )
}

# For full model
fullModel = function(.model) {
    formulaFull = presence ~ (week + dis + gene + dir)^2 *
        Temp.60cm.mean + Humidity.mean + Rain.sum + Wind.max + Radiation.mean +
        gene:Temp.60cm.mean + gene:Humidity.mean + gene:Rain.sum +
        gene:Wind.max + gene:Radiation.mean +
        dis:Temp.60cm.mean + dis:Humidity.mean + dis:Rain.sum +
        dis:Wind.max + dis:Radiation.mean +
        (1 | year / rep / id) + (1 | year : dir)
    update(.model, formulaFull)
}

## Main wrapper function
modelSelection = function(.model, nullVar = NULL) {
    final = reModel(.model) # !important: anova doesn't play nice with NAs.
    null = model2null(final, nullVar)
    main = model2main(final)
    anova(null, main, final) %=>%
        setNames(..,
            c(
                "Number of parameters", "AIC", "BIC", "Log likelihood",
                "Deviance", "LR Chi-sq", "DF (LR Chi-sq)", "Pr(> LR Chi-sq)"
            )
        ) %=>%
        round(.., 4) %=>%
        t %=>%
        as.data.frame %=>%
        format(.., scientific = F, digits = 6) %=>%
        setNames(.., c("Null Model", "Main Effects Model", "Final Model"))
}