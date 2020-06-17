# Set up environment -----------------------------------------------------------

## Libraries
"pbapply" %>=>% libInstall %!=>% library(.., char = T)
"ggplot2" %>=>% libInstall %!=>% library(.., char = T)
"RColorBrewer" %>=>% libInstall %!=>% library(.., char = T)

## Color palette
cols = c(
    "#834cde", "#65c33a", "#c047d8", "#4bc576", "#5765e1",
    "#adb839", "#8f48a8", "#439436", "#cb7bde", "#7c8e31",
    "#7369c3", "#d4a038", "#4977bb", "#dd4529", "#4dc6c6",
    "#df4260", "#3b9b82", "#d3732f", "#57a6d3", "#a6513a",
    "#7ebb82", "#ab4860", "#317649", "#b29cdc", "#5a7132",
    "#856199", "#afae6b", "#e4858b", "#906c2d", "#d99970"
)


# Simulate the frequency oif the gene over generation --------------------------

## Simulation function
simulGeneFreq = function(
    ne = 1000,
    p0 = 1 / 2,
    nrep = 30,
    ngen = 15,
    w = c(1, 1),
    ...
) {
    e1 = new.env()
    y0 = ifelse(!is.null(list(...)$y0), list(...)$y0, 0)
    out = data.frame(gen = y0, rep = seq_len(nrep), freq = p0)
    lapply(seq_len(nrep), i ->> {
        e1$p = p0
        message(paste("Rep", i))
        pblapply(seq_len(ngen), j ->> {
            if (e1$p > 0 & e1$p < 1) {
                c(
                    rep(1, w[1] * e1$p * ne * rlnorm(1, 0, 0.25)),
                    rep(0, w[2] * (1 - e1$p) * ne * rlnorm(1, 0, 0.25))
                ) %=>%
                    mean(sample(.., min(length(..), ne))) %->%
                    e1$p
            }
            data.frame(gen = y0 + j, rep = i, freq = e1$p)
        }) %=>%
            do.call(rbind, ..)
    }) %=>%
        do.call(rbind, c(list(out), ..))
}

## Plot the output
plotSimul = function(.data, gene = "gene", ...) {
    print(summary(.data))
    ggplot(.data, aes(gen, freq, col = as.character(rep))) +
        geom_line(
            stat = "smooth", method = "glm",
            method.args = list(family = "binomial"),
            alpha = 0.7, size = 0.8, span = 0.1, se = F
        ) +
        scale_x_continuous(
            "Number of Generations",
            breaks = x ->> x[1]:x[2],
            labels = ifelse(exists("y0"), x ->> x + y0, x ->> x),
            expand = c(0, 0)
        ) +
        scale_y_continuous(
            bquote("Frequency of"~italic(.(gene))),
            limits = c(0, 1),
            breaks = seq(0, 1, 0.25),
            expand = expansion(mult = 0.02)
        ) +
        scale_color_manual(
            values = rainbow(length(unique(.data$rep))),
            guide = F
        ) +
        theme_classic() +
        theme(
            plot.background = element_blank(),
            text = element_text(family = "Open Sans"),
            plot.title = element_text(hjust = 0.5, size = 24),
            axis.title = element_text(size = 20),
            axis.text = element_text(size = 14),
            axis.text.x = element_text(angle = 45, vjust = 0.5, hjust = 0.5),
            plot.margin = unit(c(0, 1, 0, 1), "lines")
    )
}

logPlot = function(.plot, gene = "gene", ...) {
    .data = ggplot_build(.plot)$data[[1]]
    .plot$layers[[1]] = NULL
    .plot +
        geom_line(
            aes(x, y, col = as.character(group)),
            data = .data,
            se = F
        ) +
        scale_y_continuous(
            bquote("Frequency of"~italic(.(gene))),
            expand = expansion(mult = 0.02),
            trans = "log10",
            breaks = 10 ^ (-6:0),
            labels = c(parse(text = paste0(10, "^-", 6:1)), 0)
        )
}


## Cast data for saving
simData = function(.data, ...) {
    arrange(.data, gen, rep) %=>%
        split(.., as.factor(..$gen)) %=>>%
        data.frame(..$gen[1], t(..$freq)) %=>%
        do.call(rbind, ..) %=>%
        setNames(.., c("gen", paste0("rep", unique(.data$rep))))
}


## Main wrapper function
simulField = function(ne = 1e6, p0 = .5, nrep = 1, ngen = 5, w = c(1, 1), ...) {
    par(mfrow = c(1, 2))
    simulGeneFreq(ne, p0, nrep, ngen, w, ...) %>=>%
        {if ("exportData" %in% names(list(...))) print(simData(..))} %!=>%
        plot(plotSimul(.., ...)) %=>%
        list(.., logPlot(.., ...)) %=>%
        gridExtra::grid.arrange(grobs = .., layout_matrix = rbind(c(1, 2)))
}