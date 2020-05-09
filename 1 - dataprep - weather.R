# Set up environment -----------------------------------------------------------

## Basic project functions
source("0 - prerequisites.R")

## Libraries
"openxlsx" %>=>% lib_install %!=>% library(.., char = T)
"reshape2" %>=>% lib_install %!=>% library(.., char = T)

## Set the working directory
setwd(rprojroot::find_rstudio_root_file())



# Import data ------------------------------------------------------------------

## Load excel files
datafile <- paste0(getwd(), "/Data/data.xlsx")
"w2015" %=>% read.xlsx(datafile, .., na.strings = "NA") %->% weather_2015
"w2016" %=>% read.xlsx(datafile, .., na.strings = "NA") %->% weather_2016
"w2017" %=>% read.xlsx(datafile, .., na.strings = "NA") %->% weather_2017

## Experiment start date
experiment_start <- c(
    `2015` = as.Date("2015-8-26"),
    `2016` = as.Date("2016-3-15"),
    `2017` = as.Date("2017-4-3")
)

## Merge datasets from all experiments
names(weather_2015) %=>%
    sapply(list(names(weather_2016), names(weather_2017)), identical, ..) %=>%
    all %=>%
    {
        if (..) rbind(weather_2015, weather_2016, weather_2017) else stop("!")
    } %=>%
    ..[complete.cases(..), ] %->%
    weather_imp %=>%
    rm("weather_2015", "weather_2016", "weather_2017")



# Some functions ---------------------------------------------------------------

## Convert units to metric
metric <- function(data, unit) {
    f2c <- function(x) (x - 32) * 5 / 9
    in2mm <- function(x) x * 25.4
    mph2mps <- function(x) x * 1600 / 3600
    data %=>% length %=>% seq_len %=>% lapply(.., function(x) {
        switch(unit[x],
            "F" = f2c(data[, x]),
            "in" = in2mm(data[, x]),
            "mph" = mph2mps(data[, x]),
            data[, x]
        )
    }) %=>%
        do.call(data.frame, ..) %=>%
        col_names(.., names(data))
}

## Custom aggregate function to append function name to column names
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


## Custom dcast function to append function name to column names
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



# Data manipulation ------------------------------------------------------------

## Convert units to metric
weather_units <- c(rep("N", 7), "F", "%", "in", "mph", "F", "F", "F", "W/m^2")
weather_imp %=>% metric(.., weather_units) %->% weather_all

## Get date id to calculate daily averages
weather_all %=>% nrow %=>% seq_len %=>%
    lapply(.., function(x) {
        weather_all[x, ] %=>%
            paste0(..$Hour, ..$Min, ..$Meridian) %=>%
            ifelse(.. == "120AM", 1, 0) %->%
            diff
        weather_all[x, ] %=>%
            paste(..$Year, ..$Month, ..$Day, sep = "-") %=>%
            as.Date(.., "%Y-%b-%d") - diff
    }) %=>%
    do.call(c, ..) %->%
    weather_all$date

## Differentiate day and night temperatures
cond_night1 <- weather_all$Hour %in% 9:11 & weather_all$Meridian == "PM"
cond_night2 <- weather_all$Hour %in% c(1:6, 12) & weather_all$Meridian == "AM"

(cond_night1 | cond_night2) %=>%
    ifelse(.., "night", "day") %->%
    weather_all$time



# Daily aggregates -------------------------------------------------------------

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



# Merge aggregated dataframes---------------------------------------------------

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



# Scaling weather values -------------------------------------------------------

## Calculate scaled values and scale for reverting

weather_merge %=>%
    ..[, sapply(.., is.numeric)] %=>%
    data.frame(mean = colMeans(..), sd = apply(.., 2, sd)) %->%
    weather_scale

## Merge scaled value and unscaled date
weather_merge %=>%
    data.frame(
        date = ..[, !sapply(.., is.numeric)],
        scale(..[, sapply(.., is.numeric)])
    ) %->%
    weather %=>% head



# Weekly averages with daily lags ----------------------------------------------

## Function for calculation weekly values
sliding_average <- function(data, lag = 0, weeks) {
    dpi <- data$date - experiment_start[substring(data$date, 1, 4)]
    row0 <- which(dpi == 0) # Find the row which contains the day0
    data_weeks <- data[, sapply(data, is.numeric)] # Select numeric columns
    # Prepare vector of functions to apply by spliting colnames
    data_weeks %=>% names %=>% strsplit(.., "\\.") %=>%
        sapply(.., function(x) x[length(x)]) %->% func
    # Custom mean function because inbuilt function is stupid
    mean <- function(...) base::mean(c(...))
    # Per lag aggregation (by week and by columns)
    lapply(lag, function(i) {
        ((min(weeks) - 1) * 7 + 1):(max(weeks) * 7) %=>%
            c(.. - i + row0) %=>% # adjust to lag and experiment start date
            split(.., sort(.. %% (max(weeks) - min(weeks) + 1))) %=>%
            lapply(.., function(x) {
                sapply(seq_len(length(data_weeks)), function(y) {
                    do.call(func[y], list(data_weeks[x, y]))
                }) %=>% t
            }) %=>%
            do.call(rbind, ..) %=>%
            data.frame(data$date[row0 + weeks * 7], ..) %=>%
            col_names(.., names(data))
    })
}

## Split by year
weather_byyear <- split(weather, substring(weather$date, 1, 4))

## Combine years after processing
env <- mapply(rbind,
    weather_byyear[[1]] %=>% sliding_average(.., lag = 0:25, weeks = 3:9),
    weather_byyear[[2]] %=>% sliding_average(.., lag = 0:25, weeks = 3:9),
    weather_byyear[[3]] %=>% sliding_average(.., lag = 0:25, weeks = 3:9),
    SIMPLIFY = F
)



# Cleaning up ------------------------------------------------------------------

## Save dataframes as R object
c("weather", "env", "weather_scale", "weather_merge") %=>%
    save(list = .., file = paste0(getwd(), "/Data/bacteria.rda"))

## Remove old dataframes
rm(
    "weather_all", "weather_mean", "weather_sum", "weather_min",
    "weather_max", "weather_temp2m", "weather_temp60cm",
    "weather_byyear"
)

## Load saved .rda
"/Data/weather.rda" %=>% paste0(getwd(), ..) %=>% load(.., envir = globalenv())