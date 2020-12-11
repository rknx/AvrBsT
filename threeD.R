# Set up environment -----------------------------------------------------------

## Libraries
"plotly" %>=>% libInstall %!=>% library(.., char = T)
"reshape2" %>=>% libInstall %!=>% library(.., char = T)
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

# Functions for generating 3d plot ---------------------------------------------

## Plotly plotting function
surfaceplot = function(.data, spp = "bacteria", gene = "gene", ...) {
    
    cols = c("forestgreen","yellowgreen","orange","red")
    f = list(family = "Open Sans",size = 18)
    levels = levels(.data$gene)
    genes = c("wt" = paste0(gene, "+"), "mut" = paste0(gene, "-"))[levels]

    split(.data, .data$gene) %=>>% 
        acast(.., week ~ dis, mean, value.var = "pred") %=>%
        lapply(seq_along(..), x ->> layout(
            plot_ly(
                z = ..[[x]],
                type = "surface",
                scene = paste0("scene", x),
                colors = cols,
                showscale = F
            ),
            annotations = list(
                text = unname(genes[x]),
                font = list(family = "Open Sans",size = 18),
                xref = "paper", yref = "paper", x = 0.5, y = 0.95,
                xanchor = "center", yanchor = "bottom", align = "center",
                showarrow = F
            )
        )) %=>% 
        subplot %=>%
        layout(.., 
            annotations = list(
                text = paste0("Spread of <i>", spp, "</i>"),
                font = list(family = "Open Sans", size = 24),
                xref = "paper", yref = "paper", x = 0.5, y = 1,
                xanchor = "center", yanchor = "bottom",
                align = "center",
                showarrow = F
            ),
            autosize = F,
            scene = list(
                xaxis = list(title = "Distance", font = f),
                yaxis = list(title = "Week", font = f),
                zaxis = list(title = "Probability", font = f)
            )
        )
}

## Wrapper function for data transformation 
plot3d = function(.model, .data = NULL, ...) {
    if (is.null(.data)) .data = model.frame(.model)
    if (nrow(.data) == 1) stop("At least 2 data points required for plotting.")
    colnames(model.frame(.model)) %=>%
        ..[!.. %in% colnames(.data)] %=>%
        lapply(.., x ->> setNames(data.frame(0), x)) %=>%
        do.call(cbind.data.frame, c(list(.data), ..)) %=>%
        data.frame(..,
            pred = predict(.model, .., re.form = NA, type = "resp")
        ) %=>%
        surfaceplot(.., ...)
}
