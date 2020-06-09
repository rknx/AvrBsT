# Set up environment -----------------------------------------------------------

## Libraries
"reshape2" %>=>% libInstall %=>% library(.., char = T)
"ggplot2" %>=>% libInstall %=>% library(.., char = T)
"extrafont" %>=>% libInstall %=>% library(.., char = T)

## Import fonts for plots
if (!"Open Sans" %in% fonts()) {
    font_import(
        path = FONT_PATH,
        pattern = "OpenSans",
        prompt = F
    )
}


## Prepare dataframe
fidelityPlotVars = function(.model, ...) {
    if (! "glmerMod" %in% class(.model)) stop("Model should be class glmerMod.")
    frame = model.frame(.model)
    df = data.frame(
        targ = .model@resp$y,
        pred = fitted(.model),
        gene = frame$gene,
        dis = frame$dis,
        week = frame$week
    )
    df %<=>% melt(.., id = c("week", "dis", "gene"), var = "type")
    df %<=>% aggregate(value ~ week + dis + gene + type, .., FUN = mean)
}

## Plotting
fidelityPlot = function(.df) {
    ggplot(.df, aes(x = week, y = dis, fill = value)) +
        geom_raster(interpolate = T) +
        facet_grid(
            type ~ gene,
            labeller = labeller(
                gene = c(wt = "XopJ2+", mut = "XopJ2-"),
                type = c(targ = "Observed", pred = "Predicted")
            )
        ) +
        scale_fill_gradient2(
            "Pathogen incidence",
            low = "forestgreen",
            mid = "yellow",
            high = "red2",
            midpoint = 0.5
        ) +
        scale_y_continuous(
            "Distance (m)",
            breaks = seq(0, 4.8, 0.6),
            expand = c(0.02, 0.02)
        ) +
        scale_x_continuous(
            "Time (Weeks)",
            breaks = 3:9
            ) +
        theme_classic() +
        theme(
            plot.background = element_blank(),
            text = element_text(family = "Open Sans"),
            strip.background = element_blank(),
            panel.spacing.x = unit(0.5, "lines"),
            panel.spacing.y = unit(0, "lines"),
            axis.line = element_blank(),
            strip.text = element_text(size = 20),
            plot.title = element_text(hjust = 0.5, size = 24),
            legend.position = "bottom",
            legend.justification = 0.5,
            axis.title = element_text(size = 20),
            axis.text = element_text(size = 16),
            legend.title = element_blank(),
            legend.text = element_text(size = 18),
            legend.key.width = unit(1, "in"),
            legend.key.height = unit(0.3, "in"),
            legend.spacing = unit(0.05, "in")
        )
}

fidelity = function(.model) fidelityPlot(fidelityPlotVars(.model))