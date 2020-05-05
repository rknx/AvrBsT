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
field_wt <- read.xlsx(datafile, "fWT", na.strings = "#N/A")
field_mut <- read.xlsx(datafile, "fMut", na.strings = "#N/A")
field_sev <- read.xlsx(datafile, "fSev", na.strings = "#N/A")

## Experiment start date
experiment_start <- c(
    `2015` = as.Date("2015-8-26"),
    `2016` = as.Date("2016-3-15"),
    `2017` = as.Date("2017-4-3")
)

# New cols, first appearance of bacteria / disease

## Function to determine state change

first_inf <- function(data) {
    data_weeks <- data[names(data)[!is.na(as.numeric(names(data)))]]
    min_score <- min(data_weeks, na.rm = T)
    first <- data.frame()
    for (i in seq_len(nrow(data_weeks))) {
        first[i, 1] <- names(data_weeks)[
            min(which(data_weeks[i, ] > min_score))
        ]
    }
    as.numeric(first[, 1])
}

field_sev$first_sev <- first_inf(field_sev)
field_wt$first_wt <- first_inf(field_wt)
field_mut$first_mut <- first_inf(field_mut)


# accumulation data for genotypes

## Function
accu <- function(data) {
    data_base <- data[names(data)[is.na(as.numeric(names(data)))]]
    data_weeks <- data[names(data)[!is.na(as.numeric(names(data)))]]
    for (i in seq_len(nrow(data_weeks))) {
        data_weeks[i, ] <- Reduce(
            function(x, y) ifelse(1 %in% x, 1, y),
            data_weeks[i, ],
            accumulate = TRUE
        )
    }
    data.frame(data_base, data_weeks, check.names = F)
}

## Implementation
field_wt_a <- accu(field_wt)
field_mut_a <- accu(field_mut)


# Melt

## Function
my_melt <- function(data, val) {
    melt(data,
        id = names(data)[is.na(as.numeric(names(data)))],
        var = "week",
        value.name = val
    )
}

## Implementation
field_wt_m <- my_melt(field_wt_a, "wt")
field_mut_m <- my_melt(field_mut_a, "mut")
field_sev_m <- my_melt(field_sev, "sev")




# Convert severity from HB scale to ordinal scale

## Function
hb_ord <- function(input) {
    hb_scale <- data.frame(
        rating = 1:12,
        ordinal = c(
            0, 0.015, 0.045, 0.09, 0.185, 0.375,
            0.625, 0.81, 0.905, 0.955, 0.985, 100
        )
    )
    sapply(
        input,
        function(x) unique(hb_scale$ordinal[hb_scale$rating == x])
    )
}

## Implementation
field_sev_m$sev <- hb_ord(field_sev_m$sev)



# Merge all predictors
field_merge <- Reduce(
    function(x, y) merge(x, y, all = T),
    list(
        field_wt_m,
        field_mut_m,
        field_sev_m
    )
)




# Select right value type
field_merge$year <- as.factor(field_merge$year)
field_merge$rep <- as.factor(field_merge$rep)
field_merge$week <- as.numeric(as.character(field_merge$week))




# Some new columns for later
field_merge$date <-
    as.Date(experiment_start[field_merge$year]) +
    field_merge$week * 7

field_merge$first_bac <- pmin(
    field_merge$first_wt, field_merge$first_mut,
    na.rm = T
)




# Melt genetype
field <- melt(field_merge,
    id = names(field_merge)[!names(field_merge) %in% c("wt", "mut")],
    var = "gene",
    value.name = "presence"
)




# Scaleing some factors for fitting

## Calculate scaled values and scale for reverting
# field_scale <- cbind(
#     mean = colMeans(field_all[, c("dis", "week")]),
#     sd = apply(field_all[, c("dis", "week")], 2, sd)
# )

## Merge scaled value and unscaled date
# field <- cbind(
#     field_all[, !names(field_all) %in% c("dis", "week")],
#     scale(field_all[, c("dis", "week")])
# )
# week, dis, first_everything
# generate scale parameters



# Cleaning up

## Save dataframes as R object
save(field, # field_scale,
    file = paste0(getwd(), "/Data/field.rda")
)

## Remove old dataframes
rm(
    "field_sev", "field_wt", "field_mut",
    "field_wt_a", "field_mut_a",
    "field_sev_m", "field_wt_m", "field_mut_m",
    "field_merge"
)

## Load saved .rda
load(paste0(getwd(), "/Data/weather.rda"))