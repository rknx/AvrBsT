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
        pattern = "segoeui",
        prompt = F
    )
}


# Import data ------------------------------------------------------------------

## Load saved R objects
"/Data/weather.rda" %=>% paste0(getwd(), ..) %=>% load(.., envir = globalenv())
"/Data/bacteria.rda" %=>% paste0(getwd(), ..) %=>% load(.., envir = globalenv())

field <- field_bacteria[field_bacteria$year != "2015", ]



# Graphically visualize the plot -----------------------------------------------

## Get data
df_field <- with(field, aggregate(presence ~ date, FUN = mean))
df_weather <- weather[, c("date", "Rain.sum")] # to remove 2015
df <- merge(df_field, df_weather, by = "date", all = T)
df$year <- substring(df$date, 1, 4)
df <- df[df$year %in% c("2016", "2017"), ]

## Draw the plot
ggplot(df, aes(date)) +
    geom_point(aes(y = presence, group = date)) +
    geom_line(aes(y = Rain.sum / 8)) +
    facet_wrap(year ~ ., scales = "free_x") +
    scale_y_continuous("Presence", sec.axis = sec_axis(~ . * 8, name = "Rain")) +
    scale_x_date("Date", date_breaks = "2 weeks", date_labels = "%b %d") +
    theme_classic() +
    theme(
        text = element_text(family = "Segoe UI")
    ) %->%
    lagplot %=>%
    # Save the plot
    ggsave(
        filename = paste0(getwd(), "/outputs/lag_rain.png"),
        plot = .., width = 10, height = 5, units = "in", dpi = 300
    )


# Correlation between predictors and clustering --------------------------------

## Clustering function for weather predictors
my_clust <- function(cor_mat, k) {
    cor_mat %=>% dist %=>% hclust %=>% cutree(.., k = k) %=>%
        lapply(unique(..), function(x) names(..)[.. == x])
}

## Compute correlation matrix
weather %=>%
    ..[, sapply(.., is.numeric)] %=>% # Only predictor columns, not date
    cor(.., use = "complete.obs") %->%
    cor_mat

## View clusters: my_clust(cor_mat, k = 6)



# Correlation between presence and weather predictors --------------------------

## Point biserial correlation function (drizopoulos/ltm -> biserial.cor)
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
    x %=>% ..[, sapply(.., is.numeric)] %=>% names -> cols
    x %=>% merge(field, x, by = "date") %=>%
        lapply(cols, function(y) biserial_cor(..[[y]], ..[["presence"]])) %=>%
        do.call(data.frame, ..) %=>%
        col_names(.., cols) %=>% # from prerequisites, change colnames
        round(.., 2)
}))

## Function for finding best lag period
best_lag <- function(cor_table, cor_mat, k = F) {
    if (k) {
        cor_mat %=>%
            my_clust(.., k = k) %=>% # get clusters by number of clusters
            sapply(.., function(v, i) v[[i]], 1) %=>% # get first element
            cor_table[, ..] %->% # filter cor_table by cluster element
            cor_table
    }
    cor_table %=>% abs %=>%
        rowSums %>=>%
        cat("At k =", k, ", best lag is", which.max(..) - 1, "days.\n") %=>%
        which.max # return row number of best lag
}

## Calculate best lag over various numbers of predictors
3:10 %=>% # cluster lengths to evaluate over
    sapply(.., function(x) best_lag(cor_table, cor_mat, k = x)) %=>%
    math_mode %>=>% # calculate mode, see prerequisites
    cat("Overall best lag period is", .. - 1, "days\n") %->%
    blag_cor



# Regression method ------------------------------------------------------------

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
    x %=>% my_clust(cor_mat, ..) %=>% expand.grid -> pred_combn
    sapply(seq_len(nrow(pred_combn)), function(y) {
        pred_combn[y, ] %=>% unlist %=>%
            paste(.., collapse = " + ") %=>%
            paste("presence ~", ..) %=>% as.formula
    })
}))

## Execute glm using formulae for each lag
fit_table <- lapply(env, function(x) {
    data <- merge(field, x, by = "date")
    pblapply(formulae, function(y) {
        glm(y, data = data, family = binomial(link = "logit")) %=>%
            data.frame(
                paste(deparse(formula(..)[[3]], width.cutoff = 100)),
                AIC(..), BIC(..), logLik(..), tss(..)
            )
    }) %=>%
        do.call(rbind, ..) %=>%
        col_names(.., c("Formula", "AIC", "BIC", "logLik", "TSS"))
})

save(fit_table, file = paste0(getwd(), "/Data/lag_fit_table.rda"))

## Evaluate fittings by lag periods
best_fit <- do.call(rbind, lapply(seq_len(length(fit_table)), function(x) {
    fit_table[[x]] %=>% data.frame(
        min_AIC = min(..$AIC),
        min_BIC = min(..$BIC),
        min_lokLik = min(..$logLik),
        max_tss = max(..$TSS),
        mean_AIC = mean(..$AIC),
        mean_BIC = mean(..$BIC),
        mean_lokLik = mean(..$logLik),
        mean_tss = mean(..$TSS),
        formula = ..$Formula[which.min(..$BIC)],
        row.names = x
    )
}))

## Best lag period

which.min(best_fit$mean_BIC) %>=>%
    cat(
        "The best lag is", .. - 1, "days using formula: presence ~ ",
        as.character(best_fit$formula[..]), "\n"
    ) %->%
    blag_fit

## Plot the fits
fit_table %=>% length %=>% seq_len %=>%
    # Get all BIC
    lapply(.., function(x) {
        data.frame(Lag = x - 1, BIC = fit_table[[x]]$BIC)
    }) %=>%
    do.call(rbind, ..) %=>%
    # Draw the plot
    (ggplot(.., aes(Lag, BIC, group = Lag)) +
        geom_boxplot() +
        scale_x_continuous("Lag (days)", breaks = unique(plot_out$Lag)) +
        geom_hline(yintercept = min(best_fit$mean_BIC), linetype = "dashed") +
        theme_classic()) %->%
    fitplot %=>%
    # Save the plot
    ggsave(
        filename = paste0(getwd(), "/outputs/lag_fit2.png"),
        plot = .., width = 10, height = 5, units = "in", dpi = 300
    )



# Merge best lag period --------------------------------------------------------
################ to-do#####################


# Cleaning up ------------------------------------------------------------------

## Save dataframes as R object
save(field_bacteria, file = paste0(getwd(), "/Data/data.rda"))

## Remove old dataframes
rm(
    "field", "field_bacteria", "weather", "env",
    "lagtable", "cor_table", "cor_mat",
    "formulae", "fit_table", "best_fit", "fitplot"
)

## Load saved .rda
load(paste0(getwd(), "/Data/data.rda"))