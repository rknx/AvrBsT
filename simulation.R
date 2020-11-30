# Set up environment -----------------------------------------------------------

## Libraries
"pbapply" %>=>% libInstall %!=>% library(.., char = T)
"ggplot2" %>=>% libInstall %!=>% library(.., char = T)
"grid" %>=>% libInstall %!=>% library(.., char = T)
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
    # y0 = ifelse(!is.null(list(...)$y0), list(...)$y0, 0)
    out = data.frame(gen = 0, rep = seq_len(nrep), freq = p0)
    lapply(seq_len(nrep), i ->> {
        e1$p = p0
        message(paste("Rep", i))
        pblapply(seq_len(ngen), j ->> {
            if (e1$p > 0 & e1$p < 1) {
                c(
                    rep(1, w[1] * e1$p * ne * rlnorm(1, 0, 0.25)),
                    rep(0, w[2] * (1 - e1$p) * ne * rlnorm(1, 0, 0.25))
                ) %>=>%
                    set.seed(sample(1:100, 1)) %=>%
                    sample(.., min(length(..), ne)) %=>%
                    mean %->%
                    e1$p
            }
            data.frame(gen = j, rep = i, freq = e1$p)
        }) %=>%
            do.call(rbind, ..)
    }) %=>%
        do.call(rbind, c(list(out), ..))
}

## Plot the output
plotSimul = function(.data, gene = "gene", ...) {
    .plot = ggplot(.data, aes(gen, freq, col = as.character(rep))) +
        geom_line(alpha = 0.7, size = 0.8) +
        scale_x_continuous(
            "Number of Generations",
            breaks = x ->> x[1]:x[2],
            labels = ifelse(exists("y0"), x ->> x + y0, x ->> x),
            expand = expansion(add = c(0, 0.2))
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
            axis.title = element_text(size = 18),
            axis.text = element_text(size = 14),
            plot.margin = unit(c(1, 2, 1, 1), "lines"),
            plot.tag = element_text(size = 22)
        )
    plot(.plot)
}

# Log scale graph with smoothing

logPlot = function(.plot, gene = "gene", ...) {
    .data = ggplot_build(.plot)$data[[1]]
    .plot$layers[[1]] = NULL
    .plot + labs(tag = "B") +
    geom_line(
        aes(x, log10(y) / 6 + 1, col = as.character(group)),
        data = .data,
        stat = "smooth", method = "glm",
        method.args = list(family = "binomial"),
        alpha = 0.7, size = 0.8, span = 0.1, se = F
    ) +
    scale_y_continuous(
        bquote("Frequency of"~italic(.(gene))),
        limits = c(0, 1),
        breaks = seq(0, 1, 1 / 6),
        expand = expansion(add = c(0.005, 0.01)),
        labels = c(parse(text = paste0(10, "^-", 6:1)), "1")
    )
}

# Probability scale graph with smoothing

simPlot = function(.plot, gene = "gene", ...) {
    .data = ggplot_build(.plot)$data[[1]]
    .plot$layers[[1]] = NULL
    .plot + labs(tag = "A") +
    geom_line(
        stat = "smooth", method = "glm",
        method.args = list(family = "binomial"),
        alpha = 0.7, size = 0.8, span = 0.1, se = F
    ) +
    scale_y_continuous(
        bquote("Frequency of"~italic(.(gene))),
        limits = c(0, 1),
        breaks = seq(0, 1, 0.25),
        expand = expansion(add = c(0.005, 0.01))
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
    simulGeneFreq(ne, p0, nrep, ngen, w, ...) %>=>% {
            if ("exportData" %in% names(list(...))) print(simData(..))
        } %=>%
        plotSimul(.., ...) %=>%
        list(simPlot(.., ...), logPlot(.., ...)) %=>%
        gridExtra::grid.arrange(grobs = .., layout_matrix = rbind(c(1, 2))) %=>%
        gTree(children = gList(.., grid.rect(
            width = .99, height = .99,
            gp = gpar(lwd = 2, col = "black", fill = NA)
        )))
}