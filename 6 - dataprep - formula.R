# Set up environment -----------------------------------------------------------

## Libraries
"lme4" %>=>% libInstall %=>% library(.., char = T)
"pbapply" %>=>% libInstall %=>% library(.., char = T)



# Import data ------------------------------------------------------------------

## Load saved R objects
"/Data/data.rda" %=>% paste0(getwd(), ..) %=>% load(.., envir = globalenv())



# Model selection from range of formulae ---------------------------------------

## TSS function
tss = function(targ, pred) {
    pos = targ == 1
    neg = targ == 0
    pair = sum(pos) * sum(neg)

    A = sum(pred >= 0.5 & pos)
    C = sum(pos) - A
    D = sum(pred < 0.5 & neg)
    B = sum(neg) - D

    (A * D - B * C) / pair
}

## Function for calculating final TSS
tssFinal = function(fit) {
    if (any(class(fit) %in% "glmerMod")) targ = fit@resp$y
    if (any(class(fit) %in% "glm")) targ = unname(fit$y)
    tss(targ, fitted(fit))
}

## Function for calculating external TSS
tssTe = function(fit, tedata = tedataBinom) {
    .data = tedata[, colnames(tedata) %in% colnames(model.frame(fit))]
    .data = na.omit(.data)
    pred = predict(fit, .data, type = "resp", allow.new.levels = T)
    tss(.data$presence, pred)
}

# Generate formula and extract summaries (GLM) ---------------------------------

## Create list of formula
formulae = unlist(
    lapply(3:5, x ->> {
        cluster[[x]] %=>%
            expand.grid %|>%
            paste(unlist(..), collapse = " + ") %=>%
            paste("week", "dis", "gene", "dir", .., sep = " + ") %=>%
            paste(.., "week:dis", "gene:week", "gene:dis", sep = " + ") %=>%
            paste("presence ~", ..)
    })
)

## Models
pblapply(formulae, y ->> {
    glm(y, binomial, merge(field, env[[4]])) %=>%
        data.frame(
            paste(deparse(formula(..)[[3]], width.cutoff = 500)),
            AIC(..), BIC(..), logLik(..), tssFinal(..), tssTe(..)
        )
}) %=>%
    do.call(rbind, ..) %=>%
    setNames(.., c("Formula", "AIC", "BIC", "logLik", "TSS", "eTSS")) %=>%
    # arrange(.., BIC) %->%
    filter(.., TSS >= 0.70) %=>%
    arrange(.., -eTSS) %->%
    modelsBinom

## Selecting best formula
modelsBinom %=>%
    ..$Formula[which.min(..$BIC), ] %->%
    bFormGLM %=>%
    cat(paste0("Best formula by GLM is ", .., "."))

# Generate formula and extract summaries (GLMM - execute in cluster) -----------

## Create list of formula
# formulaeMM <- unlist(
#     lapply(3:5, x ->> {
#         cluster[[x]] %=>%
#             expand.grid %|>%
#             paste(unlist(..), collapse = " + ") %=>%
#             paste("week", "dis", "gene", "dir", .., sep = " + ") %=>%
#             paste(.., "week:dis", "gene:week", "gene:dis", sep = " + ") %=>%
#             paste(.., "+ (1 | year / rep / id) + (1 | year : dir)") %=>%
#             paste("presence ~", ..)
#     })
# )

## Models
# pblapply(formulaeMM, y ->> {
#     glmer(y, dataBinom, binomial(link = "logit")) %=>%
#         data.frame(
#             paste(deparse(formula(..)[[3]], width.cutoff = 500)),
#             AIC(..), BIC(..), logLik(..), tss(..), tssTe(..)
#         )
# }, cl = makeForkCluster()) %=>%
#     do.call(rbind, ..) %=>%
#     setNames(.., c("Formula", "AIC", "BIC", "logLik", "TSS", "eTSS")) %=>%
#     # arrange(.., BIC) %->%
#     filter(.., TSS >= 0.80) %=>%
#     arrange(.., -eTSS) %->%
#     modelsBinomMM

## Selecting best formula
# modelsBinomMM %=>%
#     ..$Formula[which.min(..$BIC), ] %->%
#     bFormGLMM %=>%
#     cat(paste0("Best formula by GLM is ", .., "."))


# Merge best lag period --------------------------------------------------------

## Select preferred approach
if (bFormGLM == bFormGLMM) {
    bForm = bFormGLMM
} else {
    cat("Best formula by GLM and GLMM are different. Select one to proceed.")
    userChoice = select.list(c("GLM", "GLMM"))
    bForm = ifelse(userChoice == "GLM", bFormGLM, bFormGLMM)
}


# Cleaning up ------------------------------------------------------------------

## Save dataframes as R object
c("bForm") %=>%
    save(list = .., file = paste0(getwd(), "/Data/formula.rda"))

## Remove old dataframes
rm(
    "dataBinom", "teDataBinom", "formulae", "modelsBinom",
    "formulaeMM", "modelsBinomMM"
)

## Load saved .rda
"/Data/foumula.rda" %=>% paste0(getwd(), ..) %=>% load(.., envir = globalenv())