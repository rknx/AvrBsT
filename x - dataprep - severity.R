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
"fSev" %=>% read.xlsx(datafile, .., na.strings = "#N/A") %->% field_sev

## Experiment start date
experiment_start <- c(
    `2016` = as.Date("2016-3-15"),
    `2017` = as.Date("2017-4-3")
)



# First appearance -------------------------------------------------------------

## Determine state change
first_inf <- function(data) {
    data %=>% names %!=>% as.numeric %=>% is.na %=>% `!` %=>% data[..] %!=>%
        apply(.., 1, function(x) names(..)[min(which(x > 1))] %=>% as.numeric)
}

## State change for each type of input
field_sev$first_sev <- first_inf(field_sev)



# Melt the weeks ---------------------------------------------------------------

## Function
melt_weeks <- function(data, val) {
    melt(
        data,
        id = names(data)[names(data) %!=>% as.numeric %=>% is.na],
        var = "week",
        value.name = val
    )
}

## Implementation
field_sev_m <- melt_weeks(field_sev, "sev")



# Convert severity from HB scale to ordinal scale ------------------------------

## Function
hb_ord <- function(rating) {
    rating %=>% sapply(.., function(x) {
        c(
            0, 0.015, 0.045, 0.09, 0.185, 0.375,
            0.625, 0.81, 0.905, 0.955, 0.985, 100
        )[x]
    })
}

## Implementation
field_sev_m$sev <- hb_ord(field_sev_m$sev)



# Select right value type ------------------------------------------------------
field_sev_m$year <- as.factor(field_sev_m$year)
field_sev_m$rep <- as.factor(field_sev_m$rep)
field_sev_m$week <- as.numeric(as.character(field_sev_m$week))



# Merge with bacteria data ---------------------------------------------------------

## Load bacteria file
load(paste0(getwd(), "/Data/bacteria.rda"))

field_bacteria %=>%
    substring(..$date, 1, 4) %=>%
    field[.. %in% c("2016", "2017"), ] %->%
    field_bacteria

field <- Reduce(
    function(x, y) merge(x, y, all = T),
    list(
        field_bacteria,
        field_sev_m
    )
)



# Cleaning up ------------------------------------------------------------------

## Save dataframes as R object
field %=>% save(.., file = paste0(getwd(), "/Data/severity.rda"))

## Remove old dataframes
rm(
    "field_sev", "field_sev_m", "field_bacteria"
)

## Load saved .rda
"/Data/severity.rda" %=>% paste0(getwd(), ..) %=>% load(.., envir = globalenv())