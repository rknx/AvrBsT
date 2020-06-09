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