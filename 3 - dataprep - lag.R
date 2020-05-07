# Set up environment

## Libraries
if (!require(lme4)) install.packages("lme4")
library(lme4)
if (!require(ggplot2)) install.packages("ggplot2")
library(ggplot2)
if (!require(pbapply)) install.packages("pbapply")
library(pbapply)

## Set the working directory
setwd(rprojroot::find_rstudio_root_file())

## Load pipe function
source("0 - prerequisites.R")




# Import data

## Load saved R objects
"/Data/weather.rda" %=>% paste0(getwd(), .) %=>% load(., envir = globalenv())
"/Data/field.rda" %=>% paste0(getwd(), .) %=>% load(., envir = globalenv())

# Correlation between predictors and clustering

## Clustering function for weather predictors
my_clust <- function(cor_mat, k) {
    cor_mat %=>% dist(.) %=>% hclust(.) %=>% cutree(., k = k) -> groups
    lapply(unique(groups), function(x) names(groups)[groups == x])
}

## Correlation matrix
weather[-1] %=>% cor(., use = "complete.obs") -> cor_mat

## Clusters
#### cor_mat %=>% my_clust(., k = 5)



# Correlation between presence and weather predictors

## Point biserial correlation function
## from github.com/drizopoulos/ltm/blob/master/R/biserial.cor.R
biserial_cor <- function(x, y) {
    na.row <- complete.cases(x, y) # !imp make na index prior to applying
    x <- x[na.row]
    y <- y[na.row]
    Δμ <- mean(x[y == 1]) - mean(x[y == 0])
    ℙ <- mean(y == 1)
    σ <- sd(x) * sqrt((length(x) - 1) / length(x))
    Δμ * sqrt(ℙ * (1 - ℙ)) / σ
}

## Correlation over lag periods
cor_table <- do.call(rbind, lapply(env, function(x) {
    data <- merge(field, x, by = "date")
    out <- do.call(data.frame, lapply(names(x)[-1], function(y) {
        biserial_cor(data[[y]], data$presence)
    }))
    names(out) <- names(x)[-1]
    round(out, 2)
}))

## Function for finding best lag period
best_lag <- function(cor_table, cor_mat, k = F) {
    if (k) {
        pred_clust <- my_clust(cor_mat, k = k)
        pred_select <- sapply(pred_clust, function(v, i) v[[i]], 1)
        cor_table <- cor_table[, pred_select]
    }
    cor_sum <- rowSums(abs(cor_table))
    cat("At k=", k, ", best lag is", which.max(cor_sum) - 1, "days.\n")
    which.max(cor_sum)
}

## Calculate best lag over various numbers of predictors
sapply(3:10, function(x) best_lag(cor_table, cor_mat, x)) %=>%
    match(., unique(.)) %=>% tabulate %=>% which.max %=>%
    unique(lags_cor)[.] -> blag_cor



# Regression method

## function for calculating TSS
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

## Prepare list of formulas
formulae <- do.call(c, lapply(5:6, function(x) {
    x %=>% my_clust(., 5) %=>% expand.grid(.) -> pred_combn
    sapply(seq_len(nrow(pred_combn)), function(y) {
        pred_combn[y, ] %=>% unlist(.) %=>%
            paste(., collapse = " + ") %=>%
            paste("presence ~", .) %=>% as.formula
    })
}))

## Execute glm using formulae for each lag
fit_table <- lapply(seq_len(length(env)), function(x) {
    data <- merge(field, env[[x]], by = "date")
    out <- do.call(rbind, pblapply(formulae, function(y) {
        fit <- glm(y, data = data, family = binomial(link = "logit"))
        data.frame(
            paste(deparse(y[[3]])),
            AIC(fit), BIC(fit), logLik(fit), tss(fit)
        )
    }))
    names(out) <- c("Formula", "AIC", "BIC", "logLik", "tss")
    out
})

save(fit_table, file = paste0(getwd(), "/Data/lag_fit_table.rda"))

# Get some metrics
best_fit <- do.call(rbind, lapply(seq_len(length(fit_table)), function(x) {
    df <- fit_table[[x]]
    data.frame(
        min_AIC = min(df$AIC),
        min_BIC = min(df$BIC),
        min_lokLik = min(df$logLik),
        max_tss = max(df$tss),
        mean_AIC = mean(df$AIC),
        mean_BIC = mean(df$BIC),
        mean_lokLik = mean(df$logLik),
        mean_tss = mean(df$tss),
        formula = df$Formula[which.min(df$BIC)],
        row.names = x
    )
}))

## Best lag period

cat(
    "The best lag is",
    which.min(best_fit$mean_BIC) - 1,
    "days using formula: presence ~ ",
    as.character(best_fit$formula[which.min(best_fit$mean_BIC)]),
    "\n"
)

## Plot the fits
plot_out <- do.call(rbind, lapply(seq_len(length(fit_table)), function(x) {
    df <- fit_table[[x]]
    data.frame(Lag = x - 1, Model = seq_len(nrow(df)), BIC = df$BIC)
}))

fit_plot <- ggplot(plot_out, aes(Lag, BIC, group = Lag)) +
    geom_boxplot() +
    scale_x_continuous("Lag (days)", breaks = unique(plot_out$Lag)) +
    geom_hline(yintercept = min(best_fit$mean_BIC) + 15, linetype = "dashed") +
    theme_classic()

ggsave(
    fit_plot,
    filename = paste0(getwd(), "/outputs/lag_fit.png"),
    width = 10, height = 5, units = "in", dpi = 300
)



############# Do lagplot
source("lagplot.R")
lagplot(
    fd = data.frame(date = 7 * dm$week + doi[dm$year], rating = dm$  rating),
    wd = data.frame(
        date = c(outer(
            as.numeric(rownames(w)),
            doi[names(doi)], `+`
        )),
        var = unlist(w[, names(w) %in% "rain"])
    ),
    labs = c("Rating", "Rain 'in'", dir = "h")
)