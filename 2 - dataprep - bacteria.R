# Set up environment -----------------------------------------------------------

## Libraries
"openxlsx" %>=>% libInstall %!=>% library(.., char = T)
"reshape2" %>=>% libInstall %!=>% library(.., char = T)



# Import data ------------------------------------------------------------------

## Load excel files
datafile <- paste0(getwd(), "/Data/data.xlsx")
"fWT" %=>% read.xlsx(datafile, .., na.strings = "#N/A") %->% fieldWt
"fMut" %=>% read.xlsx(datafile, .., na.strings = "#N/A") %->% fieldMut

## Experiment start date
experimentStart <- c(
    `2015` = as.Date("2015-8-26"),
    `2016` = as.Date("2016-3-15"),
    `2017` = as.Date("2017-4-3")
)



# First appearance -------------------------------------------------------------

## Determine state change
firstInfection <- function(data) {
    data %=>% names %!=>% as.numeric %=>% is.na %=>% `!` %=>% data[..] %!=>%
        apply(.., 1, function(x) names(..)[min(which(x > 0))] %=>% as.numeric)
}

## State change for each type of input
fieldWt$firstWt <- firstInfection(fieldWt)
fieldMut$firstMut <- firstInfection(fieldMut)



# Accumulation data for genotypes ----------------------------------------------

## Function
transformAccu <- function(.data) {
    .data %=>% names %!=>% as.numeric %=>% is.na %=>% `!` %->% colTypes
    .data %=>% nrow %=>% seq_len %=>%
        lapply(.., z ->> {
            Reduce(
                .(x, y) ->> ifelse(1 %in% x, 1, y),
                .data[z, colTypes],
                accumulate = TRUE
            ) %=>% t
        }) %=>%
        do.call(rbind.data.frame, ..) %=>%
        setNames(.., names(.data)[colTypes]) %=>%
        data.frame(.data[!colTypes], .., check.names = F)
}

## Implementation
fieldWtAccu <- transformAccu(fieldWt)
fieldMutAccu <- transformAccu(fieldMut)



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
fieldWtMelt <- meltWeeks(fieldWtAccu, "wt")
fieldMutMelt <- meltWeeks(fieldMutAccu, "mut")



# Merge all predictors ---------------------------------------------------------
fieldMerge <- merge(fieldWtMelt, fieldMutMelt, all = T)



# Modify data type and add some columns ----------------------------------------
fieldMerge %<=>%
    mutate(..,
        year = as.factor(year),
        rep = as.factor(rep),
        week = as.numeric(as.character(week)),
        date = as.Date(experimentStart[year]) + week * 7,
        firstBac = pmin(firstWt, firstMut, na.rm = T),
        dir = ifelse(dis == 0, 0, ifelse(dir == "up", 1, -1))
    )



# Melt genetype ----------------------------------------------------------------
fieldBacteria <- melt(fieldMerge,
    id = names(fieldMerge)[!names(fieldMerge) %in% c("wt", "mut")],
    var = "gene",
    value.name = "presence"
)



# Cleaning up ------------------------------------------------------------------

## Save dataframes as R object
c("fieldBacteria") %=>%
    save(list = .., file = paste0(getwd(), "/Data/bacteria.rda"))

## Remove old dataframes
rm(
    "fieldWt", "fieldMut", "fieldWtAccu", "fieldMutAccu",
    "fieldWtMelt", "fieldMutMelt", "fieldMerge"
)

## Load saved .rda
"/Data/bacteria.rda" %=>% paste0(getwd(), ..) %=>% load(.., envir = globalenv())