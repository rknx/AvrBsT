# Set up environment -----------------------------------------------------------

## Basic project functions
source("0 - prerequisites.R")

## Libraries
"lme4" %>=>% libInstall %=>% library(.., char = T)
"ggplot2" %>=>% libInstall %=>% library(.., char = T)
"pbapply" %>=>% libInstall %=>% library(.., char = T)
"extrafont" %>=>% libInstall %=>% library(.., char = T)

## Set the working directory
setwd(rprojroot::find_rstudio_root_file())
if (!"Open Sans" %in% fonts()) {
    font_import(
        path = "/mnt/c/Users/rknx/AppData/Local/Microsoft/Windows/Fonts/",
        pattern = "OpenSans",
        prompt = F
    )
}


# Import data ------------------------------------------------------------------

## Load saved R objects
"/Data/weather.rda" %=>% paste0(getwd(), ..) %=>% load(.., envir = globalenv())
"/Data/bacteria.rda" %=>% paste0(getwd(), ..) %=>% load(.., envir = globalenv())

field <- filter(fieldBacteria, year != "2015")
teField <- filter(fieldBacteria, year == "2015")

# Graphically visualize the plot -----------------------------------------------

## Get data
# dfField <- with(field, aggregate(presence ~ date, FUN = mean))
# df_weather <- weather[, c("date", "Rain.sum")] # to remove 2015
# df <- merge(dfField, df_weather, by = "date", all = T)
# df$year <- substring(df$date, 1, 4)
# df <- df[df$year %in% c("2016", "2017"), ]

## Draw the plot
# ggplot(df, aes(date)) +
#     geom_point(aes(y = presence, group = date)) +
#     geom_line(aes(y = Rain.sum / 8)) +
#     facet_wrap(year ~ ., scales = "free_x") +
#     scale_y_continuous(sec.axis = sec_axis(~ . * 8, name = "Rain")) +
#     scale_x_date("Date", date_breaks = "2 weeks", date_labels = "%b %d") +
#     theme_classic() +
#     theme(
#         text = element_text(family = "Segoe UI")
#     ) %->%
#     lagplot %=>%
#     # Save the plot
#     ggsave(
#         filename = paste0(getwd(), "/outputs/lag_rain.png"),
#         plot = .., width = 10, height = 5, units = "in", dpi = 300
#     )


# Correlation between predictors and clustering --------------------------------

## Clustering function for weather predictors
myClust <- function(corMat, k) {
    corMat %=>% dist %=>% hclust %=>% cutree(.., k = k) %=>%
        lapply(unique(..), x ->> names(..)[.. == x])
}

## Compute correlation matrix
weather %=>%
    ..[, sapply(.., is.numeric)] %=>% # Only predictor columns, not date
    cor(.., use = "complete.obs") %->%
    corMat

### View clusters: myClust(corMat, k = 6)



# Correlation between presence and weather predictors --------------------------

## Point biserial correlation function (drizopoulos/ltm -> biserial.cor)
biserialCor <- function(x, y) {
    naRow <- complete.cases(x, y) # !imp make na index prior to applying
    x <- x[naRow]
    y <- y[naRow]
    delMu <- mean(x[y == 1]) - mean(x[y == 0])
    prob <- mean(y == 1)
    rho <- sd(x) * sqrt((length(x) - 1) / length(x))
    delMu * sqrt(prob * (1 - prob)) / rho
}

## Correlation over lag periods
env[1:10] %=>>%
    {
        ..[, sapply(.., is.numeric)] %=>% names %->% cols
        merge(field, .., by = "date") %=>%
            lapply(cols, y ->> biserialCor(..[[y]], ..[["presence"]])) %=>%
            do.call(data.frame, ..) %=>%
            setNames(.., cols) %=>% # from prerequisites, change colnames
            round(.., 2)
    } %=>%
    do.call(rbind, ..) %->%
    corTable

## Function for finding best lag period
bestLag <- function(corTable, corMat, k = length(corTable)) {
    myClust(corMat, k = 5) %=>% # get clusters by number of clusters
        # lapply(.., y ->> corTable[, y, drop = F] %=>% abs %=>% rowMeans) %=>%
        lapply(.., y ->> corTable[, sample(y, 1), drop = F] %=>% abs) %=>%
        # lapply(.., y ->> corTable[, y[1], drop = F] %=>% abs) %=>%
        do.call(data.frame, ..) %=>% rowMeans %>=>%
        cat("At k =", k, ", best lag is", which.max(..) - 1, "days.\n") %=>%
        which.max # return row number of best lag
}

## Calculate best lag over various numbers of predictors
3:10 %>=>% # cluster lengths to evaluate over
    set.seed(92) %=>%
    sapply(.., x ->> bestLag(corTable, corMat, k = x)) %=>%
    mathMode %>=>% # calculate mode, see prerequisites
    cat("Overall best lag period is", .. - 1, "days\n") %->%
    blagCor



# Regression method ------------------------------------------------------------

## function for calculating TSS for final model
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

## function for preparing tss for tedata
tssTe <- function(envi, fit) {
    fitData <- model.frame(fit)
    teField <- teField[teField$date < as.Date("2015-10-25"), ]
    tedata <- merge(teField, envi, all.x = T)
    data <- tedata[, colnames(tedata) %in% colnames(fitData)]
    data <- data[complete.cases(data), ]
    targ <- data$presence
    pred <- predict(
        fit,
        newdata = data,
        type = "resp",
        allow.new.levels = T,
        re.form = NA
    )

    pos <- targ == 1
    neg <- targ == 0
    pair <- sum(pos) * sum(neg)

    A <- sum(pred[pos] >= 0.5)
    C <- sum(pos) - A
    D <- sum(pred[neg] < 0.5)
    B <- sum(neg) - D

    (A * D - B * C) / pair
}

## Prepare list of formulas
formulae <- do.call(c, lapply(3:6, function(x) {
    x %=>% myClust(corMat, ..) %=>% expand.grid -> predCombn
    sapply(seq_len(nrow(predCombn)), function(y) {
        predCombn[y, ] %=>% unlist %=>%
            paste(.., collapse = " + ") %=>%
            paste("presence ~", ..) %=>% as.formula
    })
}))

## Execute glm using formulae for each lag
fitTable <- lapply(env[1:8], function(x) {
    data <- merge(field, x, by = "date")
    pblapply(formulae, function(y) {
        glm(y, data = data, family = binomial(link = "logit")) %=>%
            data.frame(
                paste(deparse(formula(..)[[3]], width.cutoff = 100)),
                AIC(..), BIC(..), logLik(..), tss(..), tssTe(x, ..)
            )
    }) %=>%
        do.call(rbind, ..) %=>%
        setNames(.., c("Formula", "AIC", "BIC", "logLik", "TSS", "eTSS"))
})

save(fitTable, file = paste0(getwd(), "/Data/lagFitTable.rda"))

## Evaluate fittings by lag periods
bestFit <- do.call(rbind, lapply(seq_len(length(fitTable)), function(x) {
    fitTable[[x]] %=>% data.frame(
        minAIC = min(..$AIC),
        minBIC = min(..$BIC),
        minLokLik = min(..$logLik),
        maxTss = max(..$TSS),
        maxETSS = max(..$eTSS),
        meanAIC = mean(..$AIC),
        meanBIC = mean(..$BIC),
        meanLokLik = mean(..$logLik),
        meanTSS = mean(..$TSS),
        meanETSS = mean(..$eTSS),
        formula = ..$Formula[which.min(..$BIC)],
        row.names = x
    )
}))

## Best lag period

which.min(bestFit$meanBIC) %>=>%
    cat(
        "The best lag is", .. - 1, "days using formula: presence ~ ",
        as.character(bestFit$formula[..]), "\n"
    ) %->%
    blagFit

## Plot the fits
fitTable %=>% length %=>% seq_len %=>%
    # Get all BIC
    lapply(.., function(x) {
        data.frame(Lag = x - 1, BIC = fitTable[[x]]$BIC)
    }) %=>%
    do.call(rbind, ..) %->%
    plotOut %=>%
    # Draw the plot
    (ggplot(.., aes(Lag, BIC, group = Lag)) +
        geom_boxplot() +
        scale_x_continuous("Lag (days)", breaks = unique(plotOut$Lag)) +
        geom_hline(yintercept = min(bestFit$meanBIC), linetype = "dashed") +
        theme_classic()) %->%
    fitplot %=>%
    # Save the plot
    ggsave(
        filename = paste0(getwd(), "/outputs/lagFit2.png"),
        plot = .., width = 10, height = 5, units = "in", dpi = 300
    )



# Merge best lag period --------------------------------------------------------
if (blagFit == blagCor) {
    dataBinom <- merge(field, env[[blagFit]], all.x = TRUE)
    tedataBinom <- merge(
        field <- fieldBacteria[fieldBacteria$year == "2015", ],
        env[[blagFit]],
        all.x = TRUE
    )
    clust5 <- myClust(corMat, k = 5)
    clust6 <- myClust(corMat, k = 6)
} else {
    stop("Best fit by correalation and regression vary.")
}



# Cleaning up ------------------------------------------------------------------

## Save dataframes as R object
c("dataBinom", "tedataBinom", "clust5", "clust6") %=>%
    save(list = .., file = paste0(getwd(), "/Data/data.rda"))

## Remove old dataframes
rm(
    "field", "fieldBacteria", "weather", "weatherScale", "env",
    "corTable", "corMat", "formulae", "fitTable", "bestFit",
    "plotOut", "fitplot"
)

## Load saved .rda
"/Data/data.rda" %=>% paste0(getwd(), ..) %=>% load(.., envir = globalenv())