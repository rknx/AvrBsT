# Set up environment -----------------------------------------------------------

## Libraries
"ggplot2" %>=>% libInstall %!=>% library(.., char = T)
"gridExtra" %=>% libInstall
"lme4" %>=>% libInstall %!=>% library(.., char = T)
"extrafont" %>=>% libInstall %!=>% library(.., char = T)
"pbapply" %>=>% libInstall %!=>% library(.., char = T)

## Import fonts for plots
if (!"Open Sans" %in% fonts()) {
    font_import(
        path = "FONT_PATH",
        pattern = "OpenSans",
        prompt = F
    )
}

# Final model validation -------------------------------------------------------

## Final model with coraviates
final = function(.model, ...) {
    targ = .model@resp$y
    pred = fitted(.model)
    accu = modelAccuracy(targ, pred, ...)
    p = plotRoc(targ, pred, title = paste("Final model"), ...)
    list(accu, p)
}

## Final model without covariates
finalNoCov = function(.model, ...) {
    targ = .model@resp$y
    pred = predict(.model, model.frame(.model), type = "resp", re.form = NA)
    accu = modelAccuracy(targ, pred, ...)
    p = plotRoc(targ, pred, title = paste("Fixed effects model"), ...)
    list(accu, p)
}



# Cross validation models ------------------------------------------------------

## Leave-p-out internal cross validation
leavePOut = function(.model, p = 10, ...) {
    data = randomize(model.frame(.model), ...)
    ncv = split(rownames(data), seq_len(nrow(data)) %% ceiling(nrow(data) / p))
    df = trainModel(.model, data, ncv[1:10], "leave-p-out")
    accu = modelAccuracy(df$targ, df$pred)
    p = plotRoc(df$targ, df$pred, paste0("Leave-", p, "-out CV"))
    list(accu, p)
}

# k-fold internal cross validation
kFold = function(.model, k = 10, ...) {
    data = randomize(model.frame(.model), ...)
    ncv = split(rownames(data), seq_len(nrow(data)) %% k)
    df = trainModel(.model, data, ncv, "k-fold")
    accu = modelAccuracy(df$targ, df$pred)
    p = plotRoc(df$targ, df$pred, title = paste0(k, "-fold CV"))
    list(accu, p)
}

## external cross validation
extVal = function(.model, .data, ...) {
    .data = .data[, colnames(.data) %in% colnames(model.frame(.model))]
    .data = .data[complete.cases(.data), ]
    targ = .data[, deparse(formula(.model)[[2]])]
    pred = predict(.model, .data, type = "resp",
        allow.new.levels = T, re.form = NA)
    accu = modelAccuracy(targ, pred, ...)
    p = plotRoc(targ, pred, title = paste("External CV"), ...)
    assign("rocey", p, envir = globalenv())
    list(accu, p)
}

# Accuracy and ROC functions ---------------------------------------------------

## Goodness of fit and model accuracy stats
modelAccuracy = function(targ, pred, thres = 0.5, ...) {
    if ((n = length(targ)) != length(pred)) stop("targ and pred length vary.")
    pos = targ == 1; neg = targ == 0
    a = sum(pred >= thres & pos); c = sum(pos) - a
    d = sum(pred < thres & neg); b = sum(neg) - d
    pExp = ((a + c) * (a + b) + (b + d) * (c + d)) / n^2
    mccDen = sqrt(as.numeric((a + b)) * (a + c) * (b + d) * (c + d))
    data.frame(
        "Sensitivity" = a / sum(pos),
        "Specificity" = d / sum(neg),
        "Correct class" = (a + d) / n,
        "Cohen's kappa" = ((a + d) / n - pExp) / (1 - pExp),
        "Matthew's Correlation Coefficient" = (a * d - b * c) / mccDen,
        "True Skill Statistic" = (a * d - b * c) / ((a + c) * (b + d)),
        check.names = FALSE
    )
}

## Receiver Operator Characteristics Curve
plotRoc = function(targ, pred, title = "", bin = 20, ...) {
    roc = data.frame(th = seq(0, 1, length.out = bin + 1))
    pos = targ == 1; neg = targ == 0
    roc %<=>% mutate(..,
        tpr = sapply(th, x ->> sum(pred >= x & pos) / sum(pos)),
        fpr = sapply(th, x ->> sum(pred >= x & neg) / sum(neg)),
        type = title
    )
}

## Actual plotting function for ROC
plotROCFunc = function(roc, thres = 0.5, ...) {
    roc %=>%
        split(.., ..$type) %=>>%
        cbind(..,
            hline = ..$tpr[..$th == thres],
            vline = ..$fpr[..$th == thres],
            auc = sum(sapply(2:nrow(..),
                    .(x, y) ->> (y$tpr[x] + y$tpr[x - 1]) / 2,
                ..) * -1 * diff(..$fpr)
            )
        ) %=>%
            do.call(rbind, ..) %->%
            roc
    .plot = ggplot(roc, aes(x = fpr, y = tpr, ymin = 0, ymax = tpr)) +
        geom_ribbon(fill = "#f0ab7d", alpha = 0.3) +
        geom_line(col = "#f55252", size = 1.5) +
        geom_point(size = 2,  alpha = 0.75) +
        coord_fixed() +
        geom_line(aes(th, th),  col = "blue",  size = 1) +
        geom_hline(aes(yintercept = hline), linetype = 2) +
        geom_vline(aes(xintercept = vline), linetype = 2) +
        facet_wrap(. ~ type) +
        scale_x_continuous(
            "False Positive Rate",
            breaks = seq(0, 1, 0.2),
            limits = c(0, 1),
            expand = expansion(add = 0.01)
        ) +
        scale_y_continuous(
            "True Positive Rate",
            breaks = seq(0, 1, 0.2),
            limits = c(0, 1),
            expand = expansion(add = c(0, 0.01))
        ) +
        geom_text(aes(label = paste("AUC =", round(auc, 2))),
            size = 10, x = .65, y = .2) +
        theme_classic() +
        theme(
            plot.title = element_text(hjust = 0.5, size = 0),
            text = element_text(family = "Open Sans"),
            axis.title = element_text(size = 20),
            axis.text = element_text(size = 16),
            strip.background = element_blank(),
            panel.spacing.x = unit(4, "lines"),
            panel.spacing.y = unit(2, "lines"),
            strip.text = element_text(size = 20),
            plot.margin = unit(c(1, 2, 1, 1), "lines"),
            plot.tag = element_text(size = 22),
            plot.background = element_rect(color = "black", size = 2)
        )
    .grob = ggplotGrob(.plot)
    .grob[[1]][[12]] = .grob[[1]][[10]]
    .grob[[1]][[13]] = .grob[[1]][[11]]
    .grob[[1]][[14]] = .grob[[1]][[16]]
    .grob[[1]][[15]] = .grob[[1]][[17]]
    .grob
}



# Helper functions -------------------------------------------------------------

## Row randomization for cross validation
randomize = function(.data, seed = 10, ...) {
    set.seed(seed)
    .data = .data[sample(nrow(.data)), ]
    row.names(.data) = NULL
    .data
}

## Training for internal validation
trainModel = function(.model, .data, splitList, message = "training") {
    message(paste("Processing", message, ":", length(splitList), "iterations."))
    pblapply(splitList, x ->> {
        glmer(formula(.model), .data[-as.numeric(x), ], family(.model)) %=>%
            predict(.., .data[x, ], type = "resp", allow.new.levels = T) %=>%
            data.frame(targ = .data[x, 1], pred = ..)
    }) %=>%
        do.call(rbind, ..) %=>%
        ..[complete.cases(..), ]
}



# Cross validation parent function ---------------------------------------------
xVal = function(.model, p = NULL, k = NULL, extdata = NULL, noCov = F, ...) {
    if (! "glmerMod" %in% class(.model)) stop("Model should be class glmerMod.")

    roc = list()
    table = data.frame()

    if (exists(".model")) {
      out = final(.model, ...)
      table = rbind(table, "Final model" = out[[1]])
      roc[["Final model"]] = out[[2]]
    }

    if (noCov) {
      out = finalNoCov(.model, ...)
      table = rbind(table, "Fixed effect model" = out[[1]])
      roc[["Fixed effect model"]] = out[[2]]
    }

    for (i in unique(p)) {
      out = leavePOut(.model, i, ...)
      table = rbind(table, "Leave-p-out CV" = out[[1]])
      roc[[paste0("Leave-", i, "-out CV")]] = out[[2]]
    }

    for (i in unique(k)) {
      out = kFold(.model, i, ...)
      table = rbind(table, "k-fold CV" = out[[1]])
      roc[[paste0(i, "-fold CV")]] = out[[2]]
    }

    if (!is.null(extdata)) {
      out = extVal(.model, extdata, ...)
      table = rbind(table, "External CV" = out[[1]])
      roc[["External CV"]] = out[[2]]
    }

    round(table, 4) %=>%
        as.data.frame(t(..)) %=>%
        format(.., scientific = F, digits = 4) %=>%
        print

    do.call(rbind, roc) %=>%
        mutate(.., type = factor(type, levels = names(roc))) %=>%
        plotROCFunc
}