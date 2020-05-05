# Set up environment

## Libraries
if (!require(openxlsx)) install.packages("openxlsx")
library(openxlsx)
if (!require(reshape2)) install.packages("reshape2")
library(reshape2)

## Set the working directory
setwd(rprojroot::find_rstudio_root_file())



# Import data

## Load excel files
datafile <- paste0(getwd(), "/Data/data.xlsx")
weather_2015 <- read.xlsx(datafile, "w2015", na.strings = "NA")
weather_2016 <- read.xlsx(datafile, "w2016", na.strings = "NA")
weather_2017 <- read.xlsx(datafile, "w2017", na.strings = "NA")

## Experiment start date
experiment_start <- c(
    `2015` = as.Date("2015-8-26"),
    `2016` = as.Date("2016-3-15"),
    `2017` = as.Date("2017-4-3")
)

## Merge datasets from all experiments
weather_imp <- NULL
if (identical(names(weather_2015), names(weather_2016)) &
    identical(names(weather_2015), names(weather_2017))) {
    weather_imp <- do.call(
        "rbind",
        list(weather_2015, weather_2016, weather_2017)
    )
    rm("weather_2015", "weather_2016", "weather_2017")
}

weather_imp <- weather_imp[complete.cases(weather_imp), ]



# Some functions
## Convert units to metric
metric <- function(data, unit) {
    f2c <- function(x) (x - 32) * 5 / 9
    in2mm <- function(x) x * 25.4
    mph2mps <- function(x) x * 1600 / 3600
    for (i in seq_len(length(data))) {
        data[, i] <- switch(unit[i],
            "F" = f2c(data[, i]),
            "in" = in2mm(data[, i]),
            "mph" = mph2mps(data[, i]),
            data[, i]
        )
    }
    data
}

## Custon aggregate function to append function name to the column name
my_agg <- function(formula, data, FUN, ...) {
    m <- match.call(expand.dots = FALSE)
    m$... <- m$FUN <- NULL
    m[[1L]] <- as.name("model.frame")
    mf <- eval(m, parent.frame())
    if (is.matrix(mf[[1L]])) {
        lhs <- as.data.frame(mf[[1L]])
        names(lhs) <- as.character(m[[2L]][[2L]])[-1L]
        my_out <- aggregate.data.frame(lhs, mf[-1L], FUN = FUN, ...)
        colnames(my_out) <- c( # https://gist.github.com/mrdwab/5123896
            names(mf[-1L]),
            paste(names(lhs), deparse(substitute(FUN)), sep = ".")
        )
    }
    my_out
}

## Custon dcast function to append id and function name to the column
my_cast <- function(data, formula, FUN = NULL, ..., value = guess_value(data)) {
    parse_formula <- getFromNamespace("parse_formula", "reshape2")
    cast <- getFromNamespace("cast", "reshape2")
    formula <- parse_formula(formula, names(data), value)
    res <- cast(data, formula, FUN, ..., value.var = value)
    data <- as.data.frame.matrix(res$data, stringsAsFactors = FALSE)
    names(data) <- paste(
        value,
        t(res$labels[[2]]),
        deparse(substitute(FUN)),
        sep = "."
    )
    cbind(res$labels[[1]], data)
}




# Data manipulation

## Convert units to metric
weather_units <- c(rep("N", 7), "F", "%", "in", "mph", "F", "F", "F", "W/m^2")
weather_all <- metric(weather_imp, weather_units)

## Get date id to calculate daily averages
weather_all$date <- apply(weather_all, 1, function(x) {
    date <- as.Date(paste0(
        x["Year"], "-", x["Month"], "-", x["Day"]
    ), "%Y-%b-%d")
    date <- ifelse(paste0(x["Hour"], x["Min"], x["Meridian"]) == "120AM",
        as.character(date - 1),
        as.character(date)
    )
})

## Differentiate day and night temperatures
weather_all$time <- ifelse(
    (
        weather_all$Hour %in% 9:11 & weather_all$Meridian == "PM"
    ) | (
        weather_all$Hour %in% c(1:6, 12) & weather_all$Meridian == "AM"
    ), "night", "day"
)

# Calculates averages/max/min/total etc.

## Daily weather Max
weather_max <- with(weather_all, my_agg(
    cbind(Rain, Temp.60cm, Temp.2m, Wind, Humidity, Dew.Point) ~ date,
    FUN = max
))

## Daily weather Sum
weather_sum <- with(weather_all, my_agg(
    cbind(Rain) ~ date,
    FUN = sum
))

## Daily weather Min
weather_min <- with(weather_all, my_agg(
    cbind(Temp.60cm, Temp.2m, Wind, Humidity, Dew.Point) ~ date,
    FUN = min
))

## Daily weather Average
weather_mean <- with(weather_all, my_agg(
    cbind(
        Temp.60cm, Temp.2m, Wind, Humidity,
        Dew.Point, Temp.Wet, Radiation
    ) ~ date,
    FUN = mean
))

## Daily and Nightly Temperature means

weather_temp2m <- my_cast(
    weather_all, date ~ time,
    FUN = mean, value = "Temp.2m"
)

weather_temp60cm <- my_cast(
    weather_all, date ~ time,
    FUN = mean, value = "Temp.60cm"
)



# Merge aggregated frames

## Function to merge multiple aggregated dataframes
merge_dfs <- function(x, y) merge(x, y, by = "date", all.x = T)

## Actual merging
weather_merge <- Reduce(
    function(x, y) merge(x, y, by = "date", all.x = T),
    list(
        weather_mean,
        weather_sum,
        weather_min,
        weather_max,
        weather_temp2m,
        weather_temp60cm
    )
)



# Scaleing the weather values for fitting

## Calculate scaled values and scale for reverting
weather_scale <- cbind(
    mean = colMeans(weather_merge[-1]), sd = apply(weather_merge[-1], 2, sd)
)

## Merge scaled value and unscaled date
weather <- cbind(weather_merge[1], scale(weather_merge[-1]))



# Setup for weekly averages with different lags

## Function for calculation weekly values
sliding_average <- function(data, lag = 0, weeks) {
    dpi <- as.Date(data$date) - experiment_start[
        unlist(lapply(strsplit(data$date, "-"), "[[", 1))
    ]
    row0 <- which(dpi == 0) # Find the row which contains the day0
    lapply(lag, function(i) {
        wpis <- ((min(weeks) - 1) * 7):(max(weeks) * 7 - 1) - i + row0
        rows <- split(wpis, sort(wpis %% (max(weeks) - min(weeks) + 1)))
        do.call(rbind, lapply(seq_len(length(rows)), function(x) {
            df <- data[rows[[x]], -1] # -1 to remove date column
            out <- data.frame(
                as.character(data$date[tail(rows[[x]], 1) + 1]),
                t(
                    sapply(seq_len(length(df)), function(y) {
                        func <- tail(strsplit(names(df)[y], "\\.")[[1]], 1)
                        do.call(func, as.list(df[, y]))
                    })
                ),
                (x - 1) + min(weeks)
            )
            names(out) <- c(names(data), "week")
            out
        }))
    })
}

## Split by year
weather_byyear <- split(
    weather,
    unlist(lapply(strsplit(weather$date, "-"), "[[", 1))
)

## Combine years after processing
env <- mapply(
    rbind,
    sliding_average(weather_byyear[[1]], lag = 0:25, weeks = 3:9),
    sliding_average(weather_byyear[[2]], lag = 0:25, weeks = 3:9),
    sliding_average(weather_byyear[[3]], lag = 0:25, weeks = 3:9),
    SIMPLIFY = F
)



# Cleaning up

## Save dataframes as R object
save(weather, env, weather_scale, weather_merge,
    file = paste0(getwd(), "/Data/weather.rda")
)

## Remove old dataframes
rm(
    "weather_all", "weather_mean", "weather_sum", "weather_min",
    "weather_max", "weather_temp2m", "weather_temp60cm",
    "weather_byyear"
)

## Load saved .rda
load(paste0(getwd(), "/Data/weather.rda"))