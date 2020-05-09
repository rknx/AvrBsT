# Set up environment -----------------------------------------------------------

## Basic project functions
source("0 - prerequisites.R")

## Libraries
"lme4" %>=>% lib_install %=>% library(.., char = T)
"ggplot2" %>=>% lib_install %=>% library(.., char = T)
"pbapply" %>=>% lib_install %=>% library(.., char = T)
"extrafont" %>=>% lib_install %=>% library(.., char = T)

## Set the working directory
setwd(rprojroot::find_rstudio_root_file())
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
    pos <- unname(fit$y) == 1
    neg <- unname(fit$y) == 0
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
            paste("presence ~", ..) %=>% as.formula
    })
}))

## Models
pblapply(formulae, function(y) {
    glm(y, data = data_binom, family = binomial(link = "logit")) %=>%
        data.frame(
            paste(deparse(formula(..)[[3]], width.cutoff = 100)),
            AIC(..), BIC(..), logLik(..), tss(..)
        )
}) %=>%
    do.call(rbind, ..) %=>%
    col_names(.., c("Formula", "AIC", "BIC", "logLik", "TSS")) %->%
    db_model_main