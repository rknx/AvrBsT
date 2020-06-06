# Set up environment -----------------------------------------------------------

## Libraries
"openxlsx" %>=>% libInstall %!=>% library(.., char = T)
"reshape2" %>=>% libInstall %!=>% library(.., char = T)



# Import data ------------------------------------------------------------------

## Load excel files
datafile <- paste0(getwd(), "/Data/data.xlsx")
"fCFU" %=>% read.xlsx(datafile, .., na.strings = "#N/A") %->% fieldCFU

## Experiment start date
experimentStart <- c(
    `2016` = as.Date("2016-3-15"),
    `2017` = as.Date("2017-4-3")
)


# Melt the weeks ---------------------------------------------------------------

## Function
meltWeeks <- function(data, val) {
    melt(
        data,
        id = names(data)[names(data) %!=>% as.numeric %=>% is.na],
        var = "week",
        value.name = val
    )
}

## Implementation
fieldCFU <- meltWeeks(fieldCFU, "lCFU")


# Modify data typoe and add some columns ---------------------------------------
fieldCFU %<=>%
    mutate(..,
        year = as.factor(year),
        rep = as.factor(rep),
        week = as.numeric(as.character(week)),
        date = as.Date(experimentStart[year]) + week * 7
    )



# Cleaning up ------------------------------------------------------------------

## Save dataframes as R object
c("fieldCFU") %=>%
    save(list = .., file = paste0(getwd(), "/Data/cfu.rda"))

## Load saved .rda
"/Data/cfu.rda" %=>% paste0(getwd(), ..) %=>% load(.., envir = globalenv())