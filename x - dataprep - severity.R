# Set up environment -----------------------------------------------------------

## Libraries
"openxlsx" %>=>% lib_install %!=>% library(.., char = T)
"reshape2" %>=>% lib_install %!=>% library(.., char = T)


# Import data ------------------------------------------------------------------

## Load excel files
datafile <- paste0(getwd(), "/Data/data.xlsx")
"fSev" %=>% read.xlsx(datafile, .., na.strings = "#N/A") %->% fieldSev

## Experiment start date
experimentStart <- c(
    `2016` = as.Date("2016-3-15"),
    `2017` = as.Date("2017-4-3")
)



# First appearance -------------------------------------------------------------

## Determine state change
firstInf <- function(data) {
    data %=>% names %!=>% as.numeric %=>% is.na %=>% `!` %=>% data[..] %!=>%
        apply(.., 1, function(x) names(..)[min(which(x > 1))] %=>% as.numeric)
}

## State change for each type of input
fieldSev$firstSev <- firstInf(fieldSev)



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
fieldSevMelt <- meltWeeks(fieldSev, "rating")



# Convert severity from HB scale to ordinal scale ------------------------------

horsfallBarratt <- function(rating) {
    ord = c(0, 0.015, 0.045, 0.09, 0.185, 0.375)
    rating %=>% sapply(.., x ->> c(ord, rev(1 - ord))[x])
}


# Modify data type and add some columns ----------------------------------------
fieldSevMelt %<=>%
    mutate(..,
        sev = horsfallBarratt(rating),
        year = as.factor(year),
        rep = as.factor(rep),
        week = as.numeric(as.character(week)),
        date = as.Date(experimentStart[year]) + week * 7
    )


# Merge with bacteria data -----------------------------------------------------

## Load bacteria file
load(paste0(getwd(), "/Data/bacteria.rda"))

fieldBacteria %=>%
    substring(..$date, 1, 4) %=>%
    field[.. %in% c("2016", "2017"), ] %->%
    fieldBacteria

field <- merge(fieldBacteria, fieldSevMelt, all = TRUE)



# Cleaning up ------------------------------------------------------------------

## Save dataframes as R object
field %=>% save(.., file = paste0(getwd(), "/Data/severity.rda"))

## Remove old dataframes
rm(
    "fieldSev", "fieldSevMelt", "fieldBacteria"
)

## Load saved .rda
"/Data/severity.rda" %=>% paste0(getwd(), ..) %=>% load(.., envir = globalenv())