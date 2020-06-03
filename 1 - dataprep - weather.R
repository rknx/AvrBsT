# Set up environment -----------------------------------------------------------

## Libraries
"openxlsx" %>=>% libInstall %!=>% library(.., char = T)
"reshape2" %>=>% libInstall %!=>% library(.., char = T)



# Import data ------------------------------------------------------------------

## Load excel files
datafile <- paste0(getwd(), "/Data/data.xlsx")
"w2015" %=>% read.xlsx(datafile, .., na.strings = "NA") %->% weather2015
"w2016" %=>% read.xlsx(datafile, .., na.strings = "NA") %->% weather2016
"w2017" %=>% read.xlsx(datafile, .., na.strings = "NA") %->% weather2017

## Experiment start date
experimentStart <- c(
    `2015` = as.Date("2015-8-26"),
    `2016` = as.Date("2016-3-15"),
    `2017` = as.Date("2017-4-3")
)

## Merge datasets from all experiments
list(weather2015, weather2016, weather2017) %>=>%
    {
        if (any(sapply(.., x ->> names(x) != names(..[[1]])))) stop("!")
    } %=>%
    do.call(rbind, ..) %=>%
    ..[complete.cases(..), ] %->%
    weatherImp %=>%
    rm("weather2015", "weather2016", "weather2017", envir = globalenv())



# Data manipulation ------------------------------------------------------------

## Function
metric <- function(data, unit) {
    data %=>% seq_along %=>% lapply(.., x ->> {
        switch(unit[x],
            "F" = data[, x] %=>% (.. - 32) * 5 / 9,
            "in" = data[, x] %=>% (.. * 25.4),
            "mph" = data[, x] %=>% (.. * 16 / 36),
            data[, x]
        )
    }) %=>%
        do.call(data.frame, ..) %=>%
        setNames(.., names(data))
}

## Convert units to metric
weatherUnits <- c(rep("N", 7), "F", "%", "in", "mph", "F", "F", "F", "W/m^2")
weatherImp %=>% metric(.., weatherUnits) %->% weatherAll



# Get date and time of day to calculate daily averages -------------------------
weatherAll %<=>%
    mutate(
        ..,
        date = as.Date(paste(Year, Month, Day), "%Y %b %d") -
            ifelse(paste0(Hour, Min, Meridian) == "120AM", 1, 0),
        time = ifelse(
            (Hour %in% 9:11 & Meridian == "PM") |
                (!Hour %in% c(7:11) & Meridian == "AM"),
            "night", "day"
        )
    )

# Daily aggregates -------------------------------------------------------------

## Daily weather Max
weatherMax <- gather(
    weatherAll,
    Rain + Temp.60cm + Temp.2m + Wind + Humidity + Dew.Point ~ date,
    max
)

## Daily weather Min
weatherMin <- gather(
    weatherAll,
    Temp.60cm + Temp.2m + Wind + Humidity + Dew.Point ~ date,
    min
)

## Daily weather Sum
weatherSum <- gather(weatherAll, Rain ~ date, sum)

## Daily weather Average
weatherMean <- gather(
    weatherAll,
    Temp.60cm + Temp.2m + Wind + Humidity + Dew.Point + Temp.Wet + Radiation ~
    date,
    mean
)

## Daily and Nightly Temperature means
weatherTemp2m <- cast(weatherAll, date ~ time, mean, "Temp.2m")

weatherTemp60cm <- cast(weatherAll, date ~ time, mean, "Temp.60cm")



# Merge aggregated dataframes---------------------------------------------------

weatherMerge <- Reduce(
    .(x, y) ->> merge(x, y, by = "date", all.x = T),
    list(
        weatherMean,
        weatherSum,
        weatherMin,
        weatherMax,
        weatherTemp2m,
        weatherTemp60cm
    )
)



# Scaling weather values -------------------------------------------------------

## Calculate scaled values and scale for reverting

weatherMerge %=>%
    ..[, sapply(.., is.numeric)] %=>%
    data.frame(mean = colMeans(..), sd = apply(.., 2, sd)) %->%
    weatherScale

## Merge scaled value and unscaled date
weatherMerge %=>%
    data.frame(
        date = ..[, !sapply(.., is.numeric)],
        scale(..[, sapply(.., is.numeric)])
    ) %->%
    weather



# Weekly averages with daily lags ----------------------------------------------

byWeek <- function(.x, .weeks) {
    .weeks %=>% names %=>% strsplit(.., "\\.") %=>>% tail(.., 1) %->% func
    mean <- function(...) base::mean(c(...)) #base mean doesn't handle input
    seq_along(.weeks) %=>%
        sapply(.., y ->> do.call(func[[y]], list(.weeks[.x, y]))) %=>%
        t
}

## Function for calculation weekly values
slidingAverage <- function(.data, lag = 0, weeks) {
    row0 <- which(.data$date == experimentStart[substring(.data$date, 1, 4)])
    dataWeeks <- .data[, sapply(.data, is.numeric)]
    lapply(lag, i ->> { # for each lag period
        ((min(weeks) - 1) * 7 + 1):(max(weeks) * 7) %=>%
            c(.. - i + row0) %=>%
            split(.., sort(.. %% (max(weeks) - min(weeks) + 1))) %=>>%
            byWeek(.., dataWeeks) %=>% # compute weekly values
            do.call(rbind.data.frame, ..) %=>%
            data.frame(.data$date[row0 + weeks * 7], .., row.names = NULL) %=>%
            setNames(.., names(.data))
    })
}

## Split by year and do sliding average, then merge
split(weather, substring(weather$date, 1, 4)) %=>>%
    slidingAverage(.., lag = 0:25, weeks = 3:9) %=>%
    do.call(mapply, c(list(rbind, SIMPLIFY = F), ..)) %->%
    env


# Cleaning up ------------------------------------------------------------------

## Save dataframes as R object
c("weather", "env", "weatherScale") %=>%
    save(list = .., file = paste0(getwd(), "/Data/weather.rda"))

## Remove old dataframes
rm(
    "weatherAll", "weatherMean", "weatherSum", "weatherMin",
    "weatherMax", "weatherTemp2m", "weatherTemp60cm", "weatherMerge"
)

## Load saved .rda
"/Data/weather.rda" %=>% paste0(getwd(), ..) %=>% load(.., envir = globalenv())