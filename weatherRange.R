# Set up environment -----------------------------------------------------------

## Libraries
"ggplot2" %>=>% libInstall %!=>% library(.., char = T)
"cowplot" %>=>% libInstall %!=>% library(.., char = T)



# Import data ------------------------------------------------------------------

## Load saved .rda
"/Data/weather.rda" %=>% paste0(getwd(), ..) %=>% load(.., envir = globalenv())

# Generating plots -------------------------------------------------------------

## Boxplot plotting function
plotIt = function(df, title, ylab) {
    cols2 = c("#D9717D", "#52c1dd", "#BECA55")
    cols = c("#a54b56", "#3f93a8", "#7db33f")
    ggplot(df, aes(Season, val, fill = Season, col = Season)) +
        stat_boxplot(geom='errorbar', width = 0.2) +
        geom_boxplot(width = 0.4) +
        stat_summary(
            geom = "point", fun = "mean",
            pch = 23, size = 3, col = "grey10", fill = cols
        ) +
        theme_classic() +
        labs(x = "", y = ylab, title = scales::wrap_format(18)(title)) +
        scale_color_manual(values = cols, guide = F) +
        scale_fill_manual(values = cols2, guide = F) +
        theme(plot.title = element_text(hjust = 0.5, size = 12))
}

# Data processing --------------------------------------------------------------

## Select weather vars to be used
lab = setNames(list(
    c("Average weekly temperature", '°C'),
    c("Average weekly relative humidity", '%'),
    c("Total weekly rain", 'mm'),
    c("Average weekly wind speed", parse(text = "ms^-1")),
    c("Average weekly dew point", '°C'),
    c("Average weekly solar radiation", parse(text = "Wm^-2"))
), rownames(weatherScale)[c(1, 4, 8, 3, 5, 7)])

## Call the function
names(lab) %=>%
    lapply(.., .(x, y, z) ->> {
        data.frame(val = y[, x] * z[x, 2] + z[x, 1]) %=>%
        cbind.data.frame(.., Season = paste0(
            ifelse(substring(weather$date, 1, 4) == "2015", "Fall", "Spring"),
            "\n", substring(weather$date, 1, 4)
        )) %=>%
        plotIt(.., lab[[x]][1], lab[[x]][2])
    }, weather, weatherScale
    ) %=>%
    cowplot::plot_grid(plotlist = ..) %=>%
    ggsave(filename = paste0(getwd(), "/Output/weather.png"),
    .., width = 6, device = "png", height = 7, units = "in", dpi = 300)