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
    filt = ifelse(!is.null(list(...)$filter), list(...)$filter, 0)
    data.frame(
        targ = .model@resp$y,
        pred = fitted(.model),
        gene = frame$gene,
        dis = frame$dis,
        week = frame$week,
        year = ifelse(substr(frame$id, 1, 1) == 2, 2016, 2017),
        strain = ifelse(substr(frame$id, 1, 1) == 1, "GEV872", "GEV1001")
    ) %=>%
        # filter(.., year == 2016) %=>% ###implement filter with standard eval
        filter(.., dis < 4.8) %=>% ### 4.8 is an anomaly
        melt(.., id = names(..)[-c(1:2)], var = "type") %=>%
        aggregate(value ~ week + dis + gene + type, .., FUN = mean)
}

## Plotting
fidelityPlot = function(.df) {
    ggplot(.df, aes(x = week, y = dis, fill = value)) +
        geom_raster(interpolate = T) +
        facet_grid(
            gene ~ type,
            labeller = labeller(
                gene = c(wt = "XopJ2+", mut = "XopJ2–"),
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
            breaks = seq(0, 4.2, 0.6),
            expand = c(0, 0)
        ) +
        scale_x_continuous(
            "Time (Weeks)",
            breaks = 3:9,
            expand = c(0, 0)
            ) +
        theme_classic() +
        theme(
            text = element_text(family = "Open Sans"),
            strip.background = element_blank(),
            panel.spacing.x = unit(0.5, "lines"),
            panel.spacing.y = unit(1, "lines"),
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
            legend.spacing = unit(0.05, "in"),
            plot.margin = unit(c(1, 1, 1, 1), "lines"),
            plot.tag = element_text(size = 22),
            plot.background = element_rect(color = "black", size = 2)
        )
}

## Main wrapper function

fidelity = function(.model, ...) fidelityPlot(fidelityPlotVars(.model, ...))