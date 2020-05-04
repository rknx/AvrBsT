# Libraries
if (!require(openxlsx)) install.packages("openxlsx")
library(openxlsx)
if (!require(reshape2)) install.packages("reshape2")
library(reshape2)

# set the working directory
setwd(rprojroot::find_rstudio_root_file())

# import excel files
field_wt <- read.xlsx(paste0(getwd(), "/Data/data.xlsx"), "fWT", na.strings = "#N/A")
field_mut <- read.xlsx(paste0(getwd(), "/Data/data.xlsx"), "fMut", na.strings = "#N/A")
field_sev <- read.xlsx(paste0(getwd(), "/Data/data.xlsx"), "fSev", na.strings = "#N/A")
experiment_start <- c(
    `2015`=as.Date('2015-8-26'),
    `2016`=as.Date('2016-3-15'),
    `2017`=as.Date('2017-4-3')
)

# first appearance of bacteria / disease

## function

first_inf <- function(data, threshold) {
    data_weeks = data[names(data)[!is.na(as.numeric(names(data)))]]
    first=data.frame()
    for (i in seq_len(nrow(data_weeks))) {
        first[i,1] <- names(data_weeks)[min(which(data_weeks[i, ] > threshold))]
    }
    first[,1]
}

field_sev$first_sev = first_inf(field_sev, 1)
field_wt$first_wt = first_inf(field_wt, 0)
field_mut$first_mut = first_inf(field_mut, 0)


# accumulation data for genotypes

## function
accu <- function(data) {
    data_base = data[names(data)[is.na(as.numeric(names(data)))]]
    data_weeks = data[names(data)[!is.na(as.numeric(names(data)))]]
    for (i in seq_len(nrow(data_weeks))) {
        data_weeks[i, ] <- Reduce(
            function(x, y) ifelse(1 %in% x, 1, y),
            data_weeks[i,],
            accumulate=TRUE
        )
    }
    data.frame(data_base, data_weeks, check.names = F)
}

## implementation
field_wt_a <- accu(field_wt)
field_mut_a <- accu(field_mut)


# Melt
my_melt <- function(data, val) {
    melt(data,
        id = names(data)[is.na(as.numeric(names(data)))],
        var = "week",
        value.name = val
    )
}

field_wt_m <- my_melt(field_wt_a, "wt")
field_mut_m <- my_melt(field_mut_a, "mut")
field_sev_m <- my_melt(field_sev, "sev")

# Merge
field_merge <- Reduce(
    function (x, y) merge(x, y, all = T),
    list(
        field_wt_m,
        field_mut_m,
        field_sev_m
    )
)

# Select right value type
field_merge$year <- as.factor(field_merge$year)
field_merge$rep<- as.factor(field_merge$rep)
field_merge$wpi <- as.numeric(as.character(field_merge$week))

#Some new columns for later
field_merge$date <- as.Date(experiment_start[field_merge$year]) + field_merge$wpi*7
field_merge$first_bac <- pmin(field_merge$first_wt, field_merge$first_mut, na.rm=T)

# melt genetype
field_all <- melt(field_merge, 
    id = names(field_merge)[!names(field_merge) %in% c("wt","mut")],
    var="gene",
    value.name="presence"
)

head(field_merge)

# scale
