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
"fWT" %=>% read.xlsx(datafile, .., na.strings = "#N/A") %->% field_wt
"fMut" %=>% read.xlsx(datafile, .., na.strings = "#N/A") %->% field_mut

## Experiment start date
experiment_start <- c(
    `2015` = as.Date("2015-8-26"),
    `2016` = as.Date("2016-3-15"),
    `2017` = as.Date("2017-4-3")
)



# First appearance -------------------------------------------------------------

## Determine state change

first_inf <- function(data) {
    data %=>% names %!=>% as.numeric %=>% is.na %=>% `!` %=>% data[..] %!=>%
        apply(.., 1, function(x) names(..)[min(which(x > 0))] %=>% as.numeric)
}

## State change for each type of input
field_wt$first_wt <- first_inf(field_wt)
field_mut$first_mut <- first_inf(field_mut)



# Accumulation data for genotypes ----------------------------------------------

## Function
accu <- function(data) {
    data %=>% names %!=>% as.numeric %=>% is.na %=>% `!` %->% col_types
    data %=>% nrow %=>% seq_len %=>%
        lapply(.., function(z) {
            Reduce(
                function(x, y) ifelse(1 %in% x, 1, y),
                data[z, col_types],
                accumulate = TRUE
            ) %=>% t %=>% data.frame
        }) %=>%
        do.call(rbind, ..) %=>%
        col_names(.., names(data)[col_types]) %=>%
        data.frame(data[!col_types], .., check.names = F)
}

## Implementation
field_wt_a <- accu(field_wt)
field_mut_a <- accu(field_mut)



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
field_wt_m <- melt_weeks(field_wt_a, "wt")
field_mut_m <- melt_weeks(field_mut_a, "mut")



# Merge all predictors ---------------------------------------------------------
field_merge <- merge(field_wt_m, field_mut_m, all = T)



# Select right value type ------------------------------------------------------
field_merge$year <- as.factor(field_merge$year)
field_merge$rep <- as.factor(field_merge$rep)
field_merge$week <- as.numeric(as.character(field_merge$week))



# Some new columns -------------------------------------------------------------
field_merge$date <-
    as.Date(experiment_start[field_merge$year]) + field_merge$week * 7

field_merge$first_bac <- pmin(
    field_merge$first_wt, field_merge$first_mut,
    na.rm = T
)



# Melt genetype ----------------------------------------------------------------
field_bacteria <- melt(field_merge,
    id = names(field_merge)[!names(field_merge) %in% c("wt", "mut")],
    var = "gene",
    value.name = "presence"
)



# Cleaning up ------------------------------------------------------------------

## Save dataframes as R object
field_bacteria %=>% save(.., file = paste0(getwd(), "/Data/bacteria.rda"))

## Remove old dataframes
rm(
    "field_wt", "field_mut", "field_wt_a", "field_mut_a",
    "field_wt_m", "field_mut_m", "field_merge"
)

## Load saved .rda
"/Data/bacteria.rda" %=>% paste0(getwd(), ..) %=>% load(.., envir = globalenv())