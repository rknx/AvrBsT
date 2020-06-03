# Set up environment -----------------------------------------------------------

## Libraries
"pbapply" %>=>% libInstall %!=>% library(.., char = T)
"ggplot2" %>=>% libInstall %!=>% library(.., char = T)
"RColorBrewer" %>=>% libInstall %!=>% library(.., char = T)



# Import data ------------------------------------------------------------------

## Load saved R objects
"/Data/fitness.rda" %=>% paste0(getwd(), ..) %=>% load(.., envir = globalenv())



# Simulate the frequency oif the gene over generation --------------------------

## Simulation function

simulGeneFreq = function(
    ne = 1000,
    p0 = 1 / 2,
    nrep = 30,
    ngen = 15,
    w = c(1, 1)
) {
    e1 = new.env()
    out = data.frame(gen = 0, rep = seq_len(nrep), freq = p0)
    lapply(seq_len(nrep), i ->> {
        e1$p = p0
        message(paste("Rep", i))
        pblapply(seq_len(ngen), j ->> {
            if (e1$p > 0 & e1$p < 1) {
                c(
                    rep(1, w[1] * e1$p * ne * rlnorm(1, 0, 0.2)),
                    rep(0, w[2] * (1 - e1$p) * ne * rlnorm(1, 0, 0.2))
                ) %=>%
                    mean(sample(.., min(length(..), ne))) %->%
                    e1$p
            }
            data.frame(gen = j, rep = i, freq = e1$p)
        }) %=>%
            do.call(rbind, ..)
    }) %=>%
        do.call(rbind, c(list(out), ..))
}

## Pass the arguments

df = simulGeneFreq(ne = 1e6, p0 = 1e-6, nrep = 5, ngen = 12, w = c(10, 1))

## Plot the output
df %!=>%
    (ggplot(.., aes(gen, freq, col = as.character(rep))) +
        geom_line(
            stat = "smooth", method = "glm",
            method.args = list(family = "binomial"),
            alpha = 0.8, size = 1, span = 0.4, se = F
        ) +
        scale_x_continuous(
            "Number of Generations",
            breaks = function(x) x[1]:x[2],
            expand = c(0, 0)
        ) +
        scale_y_continuous(expression("Frequency of"~italic(XopJ2))) +
        scale_color_manual(
            values = brewer.pal(length(unique(df$rep)), "Dark2"),
            guide = F
        ) +
        theme_classic() +
        theme(
            plot.background = element_blank(),
            text = element_text(family = "Open Sans"),
            plot.title = element_text(hjust = 0.5, size = 24),
            axis.title = element_text(size = 20),
            axis.text = element_text(size = 16),
            plot.margin = unit(c(0, 1, 0, 1), "lines")
    )) %->%
    simPlot %!=>%
    ggsave(
        filename = paste0(getwd(), "/outputs/simulation.png"),
        plot = .., width = 8, height = 6, units = "in", dpi = 300
    )

## Cast data for saving
df %=>%
    arrange(.., gen, rep) %=>%
    split(.., as.factor(..$gen)) %=>>%
    data.frame(..$gen[1], t(..$freq)) %=>%
    do.call(rbind, ..) %=>%
    setNames(.., c("gen", paste0("rep", unique(df$rep)))) %->%
    simData


# Cleaning up ------------------------------------------------------------------

## Save dataframes as R object
c("simData") %=>%
    save(list = .., file = paste0(getwd(), "/Data/simData.rda"))

## Remove old dataframes
rm(
    "df", "simPlot", "simData"
)

## Load saved .rda
"/Data/simData.rda" %=>% paste0(getwd(), ..) %=>% load(.., envir = globalenv())