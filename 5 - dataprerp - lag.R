# Set up environment -----------------------------------------------------------

## Libraries
"lme4" %>=>% libInstall %=>% library(.., char = T)
"ggplot2" %>=>% libInstall %=>% library(.., char = T)
"extrafont" %>=>% libInstall %=>% library(.., char = T)

## Import fonts for plots
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
"/Data/cfu.rda" %=>% paste0(getwd(), ..) %=>% load(.., envir = globalenv())

field = filter(fieldBacteria, year != "2015")
teField = filter(fieldBacteria, year == "2015")

# Graphically visualize the plot -----------------------------------------------

## Get data
# dfField = with(field, aggregate(presence ~ date, FUN = mean))
# df_weather = weather[, c("date", "Rain.sum")] # to remove 2015
# df = merge(dfField, df_weather, by = "date", all = T)
# df$year = substring(df$date, 1, 4)
# df = df[df$year %in% c("2016", "2017"), ]

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
myClust = function(corMat, k) {
    corMat %=>% dist %=>% hclust %=>% cutree(.., k = k) %=>%
        lapply(unique(..), x ->> names(..)[.. == x])
}

## Compute correlation matrix

weather[substring(weather$date, 1, 4) != 2015, ] %=>%
    ..[, sapply(.., is.numeric)] %=>% # Only predictor columns, not date
    cor(.., use = "complete.obs") %->%
    corMat

## Set clusters
corMat %=>%
    lapply(seq_len(sqrt(length(..))), x ->> myClust(.., k = x)) %->%
    cluster


# Correlation between presence and weather predictors --------------------------

## Point biserial correlation function (drizopoulos/ltm -> biserial.cor)
biserialCor = function(x, y) {
    naRow = complete.cases(x, y) # !imp make na index prior to applying
    x = x[naRow]
    y = y[naRow]
    delMu = mean(x[y == 1]) - mean(x[y == 0])
    prob = mean(y == 1)
    rho = sd(x) * sqrt((length(x) - 1) / length(x))
    delMu * sqrt(prob * (1 - prob)) / rho
}

## Correlation over lag periods
env[1:26] %=>>%
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
bestLag = function(corTable, k = length(corTable)) {
    cluster[[k]] %=>% # get clusters by number of clusters
        lapply(.., y ->> corTable[, y, drop = F] %=>% abs %=>% rowMeans) %=>%
        # lapply(.., y ->> sample(y, 1) %=>% corTable[, .., drop = F]) %=>%
        # lapply(.., y ->> corTable[, y[1], drop = F] %=>% abs) %=>%
        do.call(data.frame, ..) %=>%
        abs %=>%
        rowMeans %=>%
        which.max %>=>%
        cat(paste0("For k = ", k, ", best lag period is ", .. - 1, " days.\n"))
}

## Calculate best lag over various numbers of predictors
3:8 %>=>% # cluster lengths to evaluate over
    set.seed(92) %=>%
    sapply(.., x ->> bestLag(corTable, k = x)) %=>%
    mathMode %>=>% # calculate mode, see prerequisites
    cat("Overall, best lag period is", .. - 1, "days.\n") %->%
    bLagCor



# Regression method ------------------------------------------------------------

## Prepare list of formulas
formulae = unlist(lapply(3:5, x ->> {
    cluster[[x]] %=>% expand.grid  %|>% paste(unlist(..), collapse = " + ")
}))

## Execute glm using formulae for each lag
fitTable = lapply(env[1:7], x ->> {
    merge(fieldCFU, x, by = "date") %=>% na.omit %->% data
    data %<=>% mutate(.., resid = glm(lCFU ~ week + dis) %=>% residuals)
    pb = txtProgressBar(min = 0, max = length(formulae), style = 3)
    lapply(seq_along(formulae), y ->> {
        setTxtProgressBar(pb, y)
        glm(as.formula(paste("resid ~", formulae[y])), data = data) %=>%
            data.frame(Formula = formulae[y], BIC = round(BIC(..), 2))
    }) %=>%
        do.call(rbind.data.frame, ..) %>=>%
        close(pb) %=>%
        arrange(.., BIC)
})

## save(fitTable, file = paste0(getwd(), "/Data/lagFitTableResid.rda"))

## Evaluate fittings by lag periods
fitTable %=>>%
    data.frame(
        minBIC = min(..$BIC),
        meanBIC = mean(..$BIC),
        medianBIC = median(..$BIC),
        formula = ..$Formula[which.min(..$BIC)]
    ) %=>%
    do.call(rbind, ..) %->%
    bestFit

## Best lag period
which.min(bestFit$meanBIC) %>=>%
    cat(
        "The best lag is", .. - 1, "days using formula: presence ~ ",
        as.character(bestFit$formula[..]), "\n"
    ) %->%
    bLagFit

## Plot the fits ##check temp
cols = c(
    "#3f8acc", "#ac5e52", "#37bdd1", "#db449c", "#55b948", "#eb4545", "#9c64ca"
) 
fills = c(
    "#77afe0", "#d38f84", "#7fd8e6", "#ee8ac4", "#8ed385", "#f09090", "#bf96e0"
)

fitTable %=>% seq_along %=>>%
    data.frame(Lag = as.character(.. - 1), BIC = fitTable[[..]]$BIC) %=>%
    do.call(rbind, ..) %->%
    plotOut %=>%
    (ggplot(.., aes(Lag, BIC, group = Lag, col = Lag, fill = Lag)) +
        stat_boxplot(geom = "errorbar", width = 0.2) +
        geom_boxplot(width = 0.4) +
        stat_summary(
            geom = "line", fun = "mean", group = "1",
            size = 0.8, col = "grey30", linetype = "dashed"
        ) +
        stat_summary(
            geom = "point", fun = "mean",
            pch = 23, size = 3, col = "grey10", fill = cols
        ) +
        scale_color_manual(values = cols, guide = F) +
        scale_fill_manual(values = fills, guide = F) +
        scale_x_discrete("Lag (days)") +
        # geom_hline(yintercept = min(bestFit$medianBIC), linetype = "dashed") +
        theme_classic()) %->%
    fitPlot %=>%
    ggsave(
        filename = paste0(getwd(), "/Output/lagFit2.png"),
        plot = .., width = 6, height = 3.5, units = "in", dpi = 300
    )

adjustcolor()

# Merge best lag period --------------------------------------------------------

## Select preferred approach
if (blagFit == blagCor) {
    bLag = bLagFit
} else {
    cat("Best lag by correalation and regression vary. Select one to proceed.")
    userChoice = select.list(c("Correlation", "Regression"))
    bLag = ifelse(userChoice == "Correlation", bLagCor, bLagFit)
}

## Combine field and weather data
dataBinom = merge(field, env[[bLag]], all.x = TRUE)
tedataBinom = merge(teField, env[[bLag]], all.x = TRUE)



# Cleaning up ------------------------------------------------------------------

## Save dataframes as R object
c("dataBinom", "tedataBinom", "cluster") %=>%
    save(list = .., file = paste0(getwd(), "/Data/data.rda"))

## Remove old dataframes
rm(
    "fieldBacteria", "weather", "weatherScale", "env", "field", "teField",
    "corTable", "corMat", "formulae", "fitTable", "bestFit",
    "plotOut", "fitPlot"
)

## Load saved .rda
"/Data/data.rda" %=>% paste0(getwd(), ..) %=>% load(.., envir = globalenv())