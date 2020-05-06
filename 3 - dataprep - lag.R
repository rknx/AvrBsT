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




# Import data

## Load saved R objects
load(paste0(getwd(), "/Data/field.rda"))
load(paste0(getwd(), "/Data/weather.rda"))




# Correlation between predictors and clustering

## Clustering function for weather predictors
my_clust <- function(cor_mat, k) {
    groups <- cutree(hclust(dist(cor_mat)), k = k)
    lapply(unique(groups), function(x) names(groups)[groups == x])
}

## Correlation matrix
cor_mat <- cor(weather[-1], use = "complete.obs")

## Clusters
# my_clust(cor_mat, k = 5) # for 5 clusters





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

## Calculate best lag over various number of predictors
lags <- sapply(3:10, function(x) best_lag(cor_table, cor_mat, x))
blag_cor <- unique(lags)[which.max(tabulate(match(lags, unique(lags))))]




# Regression method

## Prepare list of formulas
formulae <- do.call(c, lapply(5:6, function(x) {
    pred_clust <- my_clust(cor_mat, 5)
    pred_combn <- do.call(expand.grid, lapply(pred_clust, function(x) x))
    sapply(seq_len(nrow(pred_combn)), function(y) {
        row <- unlist(pred_combn[y, ])
        as.formula(paste("presence ~", paste(row, collapse = " + ")))
    })
}))

## Execute glm and
fit_table <- do.call(rbind, lapply(seq_len(length(env)), function(x) {
    data <- merge(field, env[[x]], by = "date")
    out <- do.call(rbind, pblapply(formulae, function(y) {
        fit <- glm(y, data = data, family = binomial(link = "logit"))
        data.frame(AIC(fit), BIC(fit), logLik(fit)) ############ implement xval
    }))
    df_min <- sapply(out, min)
    df_mean <- colMeans(out)
    data.frame(
        min_AIC = df_min[1], min_BIC = df_min[2], min_lokLik = df_min[3],
        mean_AIC = df_mean[1], mean_BIC = df_mean[2], mean_lokLik = df_mean[3],
        row.names = x - 1
    )
}))
fit_table










###############################################################

fitpar <- data.frame(lag = 0, aic = 0, bic = 0, logLik = 0)


for (i in 1:dim(env)[3]) {
    fit <- glmer(
        sev ~ rain + temp + wind + RH + (1 | ryd / pid),
        data = merge(dm, env[, , i]),
        family = Gamma(link = log)
    )
    fitpar[i, ] <- data.frame(i - 1, AIC(fit), BIC(fit), logLik(fit))
}


cat(paste0(
    "Best lag period is ",
    (fitpar$lag[which.min(fitpar$bic)]),
    " days (BIC = ",
    round(min(fitpar$bic), 2), ").\n"
))


ggplot(fitpar, aes(lag, bic, label = round(bic))) +
    geom_smooth(method = "loess", span = 0.3, col = "grey40", alpha = 0.4) +
    geom_hline(aes(yintercept = min(fitpar$bic)), linetype = "dashed") +
    geom_point() +
    geom_text()




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


env_m <- which.min(fitpar$bic)