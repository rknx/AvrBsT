# Set up environment -----------------------------------------------------------

## Libraries
"reshape2" %>=>% libInstall %!=>% library(.., char = T)
"lme4" %>=>% libInstall %!=>% library(.., char = T)
"ggplot2" %>=>% libInstall %!=>% library(.., char = T)
"grid" %=>% libInstall
"extrafont" %>=>% libInstall %!=>% library(.., char = T)

## Import fonts for plots
if (!"Open Sans" %in% fonts()) {
    font_import(
        path = "FONT_PATH",
        pattern = "OpenSans",
        prompt = F
    )
}

# Prediction functions

# Calculate parameters
seCalc = function(.model, .data, α = 0.05, ...) {
    nobars(formula(.model))[-2] %=>%
        model.matrix(.., .data) %=>%
        diag(.. %*% tcrossprod(vcov(.model), ..)) %->%
        pVar
    tVar = pVar + Reduce(`+`, VarCorr(.model))[1]
    .data$pred = predict(.model, .data, re.form = NA, allow.new.levels = T)
    link2Resp = x ->> family(.model)$linkinv(x)
    zScore = qnorm(1 - α / 2)
    .data %=>% mutate(..,
        fit = link2Resp(pred),
        plo = link2Resp(pred - zScore * sqrt(pVar)),
        phi = link2Resp(pred + zScore * sqrt(pVar)),
        rlo = link2Resp(pred - zScore * sqrt(pVar + sigma(.model))),
        rhi = link2Resp(pred + zScore * sqrt(pVar + sigma(.model))),
        tlo = link2Resp(pred - zScore * sqrt(tVar)),
        thi = link2Resp(pred + zScore * sqrt(tVar))
    )
}

## Plotting of predictions
plotPred = function(.data, spp = "bacteria", gene = "gene", ...) {
    cols = c("#2dcc70", "#f1c40f", "#e84c3d")
    nDis = length(unique(.data$dis)); nWeek = length(unique(.data$week))

    if (nDis > 1 & nWeek > 1) {
        plot = ggplot(.data) +
            aes(x = week, y = dis, fill = fit, label = round(fit, 2), z = fit) +
            geom_tile(linetype = 0) +
            geom_contour(binwidth = 0.5, col = "#00FFFF", size = 1.5,
                linetype = 5, alpha = 0.75
            ) +
            geom_text(size = 5.5, col = "black") +
            scale_fill_gradientn("", colors = cols, limits = c(0, 1)) +
            scale_x_continuous("Week", unique(.data$week), expand = c(0, 0)) +
            scale_y_continuous("Distance", unique(.data$dis), expand = c(0, 0),
                sec.axis = dup_axis()
            ) +
            theme(
                legend.key.width = unit(5, "lines")
            )
    } else {
        col = c(f = "black", t = "dodgerblue", r = "brown", p = "blue")
        fill = c(f = "black", t = "deepskyblue", r = "brown", p = "blue")
        alpha = c(f = 1, t = 0.4, r = 0.3, p = 0.1)
        linetype = c(f = 1, p = 5, t = 3, r = 2)
        label = c(
            f = "Fitted",
            t = "Confidence Interval (total)",
            r = "Prediction Interval (fixed)",
            p = "Confidence Interval (fixed)"
        )
        plot = ggplot(.data) +
            aes_string(ifelse(nWeek > 1, "week", "dis")) +
            annotation_custom(
                grid::rasterGrob(
                    image = matrix(adjustcolor(cols, alpha.f = 0.4), ncol = 1),
                    width = unit(1, "npc"),
                    height = unit(1, "npc"),
                    interpolate = F
                ),
                xmin = -Inf, xmax = Inf, ymin = 0, ymax = 1
            ) +
            geom_ribbon(aes(ymin = tlo, ymax = thi, #Replace with var
                fill = "t", col = "t", linetype = "t", alpha = "t")
            ) +
            geom_ribbon(aes(ymin = rlo, ymax = rhi, #Replace with var
                fill = "r", col = "r", linetype = "r", alpha = "r")
            ) +
            geom_ribbon(aes(ymin = plo, ymax = phi, #Replace with var
                fill = "p", col = "p", linetype = "p", alpha = "p")
            ) +
            geom_ribbon(aes(ymin = fit, ymax = fit,
                fill = "f", col = "f", linetype = "f", alpha = "f")
            ) +
            scale_x_continuous(
                ifelse(nWeek > 1, "Week", "Dis"), #Replace with var
                breaks = scales::pretty_breaks(n = 8),
                expand = c(0, 0)
            ) +
            scale_y_continuous(
                "Probability",
                breaks = seq(0, 1, 0.25),
                expand = c(0, 0),
                sec.axis = dup_axis(),
                limits = c(0, 1)
            ) +
            scale_linetype_manual(values = linetype, guide = F) +
            scale_color_manual("", values = col, guide = F) +
            scale_fill_manual("", values = fill, labels = label) +
            scale_alpha_manual("", values = alpha, guide = F)
    }
    plot = plot +
        ggtitle(bquote(Spread~of~italic(.(spp)))) +
        facet_wrap(~ gene, labeller = labeller(gene = c(
                wt = paste0(gene, "+"), mut = paste0(gene, "–")
            ))
        ) +
        theme_classic() +
        theme(
            text = element_text(family = "Open Sans"),
            strip.background = element_blank(),
            panel.spacing = unit(1, "lines"),
            axis.line = element_blank(),
            strip.text = element_text(size = 22),
            plot.title = element_text(hjust = 0.5, size = 24),
            legend.position = "bottom",
            legend.justification = 0.5,
            legend.text = element_text(size = 16),
            axis.title = element_text(size = 20),
            axis.text = element_text(size = 16),
            plot.margin = unit(c(1, 1, 1, 1), "lines"),
            plot.tag = element_text(size = 22)#,
            # plot.background = element_rect(color = "black", size = 2)
        )
    plot
}

## Smoothing
expandDF = function(.data) {
    if (min(.data) == max(.data)) {
        unique(.data)
    } else {
        seq(min(.data), max(.data), length.out = 50)
    }
}

## Main wrapper function
dispersalPlus = function(.model, .newdata = NULL, ...) {
    if (is.null(.newdata)) .newdata = model.frame(.model)
    if (nrow(.newdata) < 2) stop("At least 2 datapoints are required.")
    .gene = unique(.newdata$gene)
    expand.grid(
        week = expandDF(.newdata$week),
        dis = expandDF(.newdata$dis),
        gene = c("mut","wt")
    ) %!=>%
    cbind(..,
        .newdata[1, !names(.newdata) %in% c("week", "dis", "gene")]
    ) %->%
    .newdata
    colnames(model.frame(.model)) %=>%
        ..[!.. %in% colnames(.newdata)] %=>%
        lapply(.., x ->> setNames(data.frame(0), x)) %=>%
        do.call(cbind.data.frame, c(list(.newdata), ..)) %=>%
        seCalc(.model, .., ...) %=>%
        ..[..$gene %in% .gene, ] %=>%
        aggregate(
            cbind(fit, plo, phi, rlo, rhi, tlo, thi) ~ week + dis + gene,
            ..,
            mean
        ) %=>%
        plotPred(.., ...)
}

## Main wrapper function
dispersal = function(.model, .newdata = NULL, ...) {
    if (is.null(.newdata)) .newdata = model.frame(.model)
    if (nrow(.newdata) < 2) stop("At least 2 datapoints are required.")
    colnames(model.frame(.model)) %=>%
        ..[!.. %in% colnames(.newdata)] %=>%
        lapply(.., x ->> setNames(data.frame(0), x)) %=>%
        do.call(cbind.data.frame, c(list(.newdata), ..)) %=>%
        cbind(..,
            fit = predict(.model, .., type = "resp", allow.new.levels = T)
        ) %=>%
        plotPred(.., ...)
}

dispersal2 = function(.model, .newdata = NULL, ...) {
    if (is.null(.newdata)) .newdata = model.frame(.model)
    if (nrow(.newdata) < 2) stop("At least 2 datapoints are required.")
    cols = c("wt" = "#a54b56", "mut" = "#3f93a8")
    fills = c("wt" = "#D9717Dcc", "mut" = "#52c1ddcc")
    expand.grid(
            week = expandDF(.newdata$week),
            dis = .newdata$dis,
            gene = unique(.newdata$gene)
        ) %!=>%
        cbind(..,
            .newdata[1, !names(.newdata) %in% c("week", "dis", "gene")]
        ) %->% .newdata
    colnames(model.frame(.model)) %=>%
        ..[!.. %in% colnames(.newdata)] %=>%
        lapply(.., x ->> setNames(data.frame(0), x)) %=>%
        do.call(cbind.data.frame, c(list(.newdata), ..)) %=>%
        cbind(..,
            fit = predict(.model, .., type = "resp", allow.new.levels = T)
        ) %=>%
        merge(..,
            do.call(data.frame, aggregate(
                dis ~ week + fit + gene, ..[..$week == max(..$week), ] ,
                function(x) c(lab = paste0(max(x), ".0 m"), lab2 = min(x))
            )), all.x = T
        ) %=>%
        merge(..,
            do.call(data.frame, aggregate(
                fit ~ week + gene, .. ,
                function(x) c(max = max(x), min = min(x))
            )), all.x = T
        ) %=>% 
        unique(..) %=>%
        ggplot(.., aes(week, fit, col = gene)) +
            geom_ribbon(aes(fill = gene, ymin = fit.min, ymax = fit.max)) +
            geom_line(aes(group = interaction(gene, dis)), size = 0.8) +
            ggrepel::geom_text_repel(
                aes(label = as.character(dis.lab), col = gene), 
                direction = "y", hjust = 0, nudge_x = 0.8, segment.size = 0.5,
                box.padding=unit(0.2,'lines')
            ) +
            scale_x_continuous( "Weeks post inoculation",
                breaks = scales::pretty_breaks(n = 5),
                expand = expansion(add = c(0, 1))
            ) +
            scale_y_continuous( "Propability of presence",
                breaks = scales::pretty_breaks(n = 5),
                expand = expansion(add = c(0.005, 0.12))
            ) +
            scale_color_manual("xopJ2",
                values = cols,
                labels = c("wt" = "Present", "mut" = "Absent")
            ) +
            scale_fill_manual(values = fills, guide = F) +
            theme_classic() +
            theme(
                legend.title = element_text(face = 'italic'),
            legend.position = c(0.25, 0.75)
            ) +
            guides(color = guide_legend(override.aes = list(
                fill = fills,
                color = cols,
                label = c("wt" = "+", "mut" = "-")
            )))
}