# Set up environment -----------------------------------------------------------

## Libraries
"lme4" %>=>% libInstall %!=>% library(.., char = T)
"ggplot2" %>=>% libInstall %!=>% library(.., char = T)
"extrafont" %>=>% libInstall %!=>% library(.., char = T)

## Import fonts for plots
if (!"Open Sans" %in% fonts()) {
    font_import(
        path = "FONT_PATH",
        pattern = "OpenSans",
        prompt = F
    )
}

# Functions for generating frames ----------------------------------------------

## Plotting animations frames
getFrames = function(.data, res, spp = "org", gene = "gene", ...) {
    .plot = ggplot(.data, aes(xDis, yDis, fill = pred)) +
        ggtitle(bquote(Spread~of~italic(.(spp)))) +
        geom_raster(interpolate = T) +
        geom_point(aes(0, 0), size = 2) +
        scale_fill_gradientn(
            "Percentage",
            colors = c("#2dcc70", "#f1c40f", "#e84c3d"),
            limits = c(0, 1),
            breaks = seq(0, 1, 0.2),
            labels = c("0%", "20%", "40%", "60%", "80%", "100%")
        ) +
        scale_x_continuous(
            "Distance (m)",
            breaks = scales::pretty_breaks(n = 7),
            labels = x ->> x / res,
            expand = c(0, 0)
        ) +
        scale_y_continuous(
            NULL,
            breaks = scales::pretty_breaks(n = 7),
            labels = x ->> x / res,
            expand = c(0, 0)
        ) +
        theme_classic() +
        theme(aspect.ratio = 1,
            text = element_text(family = "Open Sans"),
            axis.line = element_blank(),
            strip.background = element_blank(),
            plot.title = element_text(hjust = 0.5, size = 24),
            plot.subtitle = element_text(hjust = 0.5, size = 18),
            axis.title = element_text(size = 16),
            axis.text = element_text(size = 14),
            strip.text = element_text(size = 18),
            legend.title = element_text(size = 18),
            legend.text = element_text(size = 14),
            panel.spacing = unit(1, "lines"),
            legend.key.height = unit(3, "lines"),
            plot.margin = unit(c(1, 1, 1, 1), "lines"),
            plot.tag = element_text(size = 22),
            plot.background = element_rect(color = "black", size = 2)
        )
    return(.plot)
}

## Getting animation frames
plotAnimFrames = function(.data, res, gene = "gene",...) {
    return(
        getFrames(.data, res, ...) +
        facet_grid(
            gene ~ week,
            labeller = labeller(
                gene = c(
                    "wt" = paste0(gene, "+"),
                    "mut" = paste0(gene, "–")
                ),
                week = label_both
            )
        )
    )
}

## Plotting function for animation
plotAnimate = function(.data, res, frame, gene = "gene", ...) {
    return(
        getFrames(.data, res, ...) +
        labs(
            subtitle = paste("Week: ", frame)
        ) +
        facet_wrap(
            . ~ gene,
            labeller = labeller(
                gene = c(
                    "wt" = paste0(gene, "+"),
                    "mut" = paste0(gene, "–")
                )
            )
        )
    )        
}

## PLotting frames for distribution function
plotDist = function(.data, spp = "bacteria", gene = "gene", ...) {
    cols = c("wt" = "#a54b56", "mut" = "#3f93a8")
    fills = c("wt" = "#D9717Dcc", "mut" = "#52c1ddcc")
    .plot = ggplot(.data, aes(week, ymax = fit, group = -xtfrm(gene))) +
        ggtitle(bquote("Distrubution of"~italic(.(spp)))) +
        geom_ribbon(aes(ymin = 0, fill = gene, col = gene), size = 0.8) +
        scale_fill_manual(
            "Genotype",
            values = fills,
            labels = c(wt = paste0(gene, "+"), mut = paste0(gene, "–"))
        ) +
        scale_color_manual("",
            values = cols,
            labels = c(wt = paste0(gene, "+"), mut = paste0(gene, "–")),
            guide = F
        ) +
        scale_x_continuous(
            "Time (weeks)",
            breaks = scales::pretty_breaks(n = 10),
            expand = expansion(add = c(0, 0.2))
        ) +
        scale_y_continuous(
            "Area covered (%)",
            breaks = scales::pretty_breaks(n = 10),
            limits = c(0, 1),
            labels = x ->> x * 100,
            expand = c(0, 0)
        ) +
        theme_classic() +
        theme(text = element_text(family = "Open Sans"),
            plot.title = element_blank(),
            plot.subtitle = element_text(hjust = 0.5, size = 18),
            axis.title = element_text(size = 20),
            axis.text = element_text(size = 16),
            legend.title = element_text(size = 20),
            legend.text = element_text(size = 20),
            legend.position = c(0.35, 0.75),
            plot.margin = unit(c(1, 1, 1, 1), "lines"),
            plot.tag = element_text(size = 22),
            plot.background = element_rect(color = "black", size = 2)
        )
    return(.plot)
}


## Generate coordinates of probabiity
coordinates = function(.model, .data, pos = c("c", "m"), res = 1, ...) {

    xMargins = switch(pos[1], "c" = c(-1, 1), "l" = c(0, 1), "r" = c(-1, 0))
    yMargins = switch(pos[2], "m" = c(-1, 1), "t" = c(0, 1), "b" = c(-1, 0))

    if (res != 1) .data$grid == .data$grid * res
    if (length(.data$grid) == 1) .data$grid = rep(.data$grid, 2)

    coord = expand.grid(
        xDis = (.data$grid[1] * xMargins[1]):(.data$grid[1] * xMargins[2]),
        yDis = (.data$grid[2] * yMargins[1]):(.data$grid[2] * yMargins[2]),
        week = .data$week,
        gene = .data$gene
    )

    coord %<=>% mutate(..,
        dis = sqrt(coord$xDis ^ 2 + coord$yDis ^ 2) / res,
        dir = ifelse(xDis == 0 & yDis == 0, 0, yDis / (dis * res))
    )

    colnames(model.frame(.model)) %=>%
        ..[!.. %in% colnames(coord)] %=>%
        lapply(.., x ->> setNames(data.frame(0), x)) %=>%
        do.call(cbind.data.frame, c(list(coord), ..)) %->%
        coord

    coord$pred = predict(
        .model,
        coord,
        type = "resp",
        allow.new.levels = T,
        re.form = NA
    )

    coord
}

## Main wrapper function for animation frames

animFrames = function(.model, .data, pos = c("c", "m"), res = 1, ...) {
    coordinates(.model, .data, pos, res, ...) %=>%
        plotAnimFrames(.., res, ...)
}

## Main wrapper function for animation

animation = function(.model, .data, out = "", pos = c("c", "m"), res = 1, ...) {
    imgdir = tempfile('tmppng')
    dir.create(imgdir)
    coordinates(.model, .data, pos, res, ...) %=>%
        split(.., ..$week) %=>>%
        plotAnimate(.., res, frame = floor(..$week[1]), ...) %=>%
        lapply(seq_along(..), x ->> {
            imgPath = file.path(imgdir, paste0(sprintf("%03d", x), ".png"))
            png(imgPath, 1100, 600)
            print(..[[x]])
            dev.off()
            imgPath
        }) %=>%
        paste(unlist(..), collapse = " ") %=>%
        system(paste0("convert -delay 10 ", .., " ", imgdir, "/animation.gif"))
    utils::browseURL(file.path(imgdir, "animation.gif"))
    if (out != "") file.copy(file.path(imgdir, "animation.gif"), out)
    unlink(imgdir)
}

## Main wrapper function fro aggregate distribution

distribution = function(.model, .data, pos = c("c", "m"), res = 1, ...) {
    .data$week = seq(min(.data$week), max(.data$week), length.out = 50)
    coordinates(.model, .data, pos = c("c", "m"), res = 1, ...) %=>%
        mutate(.., predBin = ifelse(pred < 0.5, 0, 1)) %=>%
        aggregate(predBin ~ week + gene, .., mean) %=>%
        split(.., ..$gene) %=>>%
        glm(predBin ~ week, .., family = binomial()) %=>>%
        data.frame(fit = fitted(..), model.frame(..)) %=>%
        do.call(rbind, ..) %=>%
        mutate(..,
            gene = lapply(strsplit(row.names(..), "\\."), '[[', 1) %=>% unlist
        ) %=>%
        plotDist(.., ...)
}