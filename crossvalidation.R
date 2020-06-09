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
        path = "/mnt/c/Users/rknx/AppData/Local/Microsoft/Windows/Fonts/",
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
# finalNoCov = function(model) {
#     fit = glm(nobars(formula(model)), family(model), model.frame(model))
#     df = data.frame(
#         targ = unname(fit$y),
#         pred = predict(fit, model.frame(fit), "resp", allow.new.levels = T)
#     )
#     accu = modelAccuracy(df$targ, df$pred)
#     p = plotRoc(df$targ, df$pred, title = paste("Final model - fixed"))
#     list(accu, p)
# }

## Final model without covariates
finalNoCov = function(model, ...) {
    targ = model@resp$y
    pred = predict(model, model.frame(model), "resp", re.form = NA)
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
    p = plotRoc(df$targ, df$pred, paste0("Leave-", p, "-out cross validation"))
    list(accu, p)
}

# k-fold internal cross validation
kFold = function(.model, k = 10, ...) {
    data = randomize(model.frame(.model), ...)
    ncv = split(rownames(data), seq_len(nrow(data)) %% k)
    df = trainModel(.model, data, ncv, "k-fold")
    accu = modelAccuracy(df$targ, df$pred)
    p = plotRoc(df$targ, df$pred, title = paste0(k, "-fold cross validation"))
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
    p = plotRoc(targ, pred, title = paste("External cross validation"), ...)
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
plotRoc = function(targ, pred, title = NULL, bin = 20, ...) {
    roc = data.frame(th = seq(0, 1, length.out = bin + 1), tpr = NA, fpr = NA)
    pos = targ == 1; neg = targ == 0
    roc %<=>% mutate(..,
        tpr = sapply(th, x ->> sum(pred >= x & pos) / sum(pos)),
        fpr = sapply(th, x ->> sum(pred >= x & neg) / sum(neg))
    )
    roc  %<=>% arrange(.., -th)
    sapply(2:nrow(roc), x ->> (roc$tpr[x] + roc$tpr[x - 1]) / 2) %=>%
        sum(.. * diff(roc$fpr)) %->%
        auc
    plotROCFunc(roc, auc, ...)
}

## Actual plotting function for ROC
plotROCFunc = function(roc, auc, thres = 0.5, title = "", ...) {
    ggplot(roc,  aes(x = fpr, y = tpr, ymin = 0, ymax = tpr)) +
        geom_ribbon(fill = "#f0ab7d") +
        geom_line(col = "#f55252", size = 1.5) +
        geom_point(size = 2,  alpha = 0.75) +
        coord_fixed() +
        geom_line(aes(th, th),  col = "blue",  size = 1) +
        geom_hline(yintercept = roc$tpr[roc$th == thres], linetype = 2) +
        geom_vline(xintercept = roc$fpr[roc$th == thres], linetype = 2) +
        ggtitle(title) +
        scale_x_continuous("FPR", breaks = roc$th, limits = c(0, 1)) +
        scale_y_continuous("TPR", breaks = roc$th, limits = c(0, 1)) +
        annotate("text", size = 10, x = .65, y = .2,
          label = paste("AUC =", round(auc, 2))) +
        theme_classic() +
        theme(plot.title = element_text(hjust = 0.5, size = 24),
            text = element_text(family = "Open Sans"),
            axis.title = element_text(size = 20),
            axis.text = element_text(size = 16),
            plot.margin = unit(c(1, 1, 1, 1), "lines"),
            axis.text.x = element_text(angle = 90)
        )
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
xVal = function(.model, p = NULL, k = NULL, extdata = NULL, re = F, ...) {
    if (! "glmerMod" %in% class(.model)) stop("Model should be class glmerMod.")

    plot = list()
    table = data.frame()

    if (exists(.model)) {
      out = final(.model, ...)
      table = rbind(table, "Final model" = out[[1]])
      plot[["Final model"]] = out[[2]]
    }

    if (re) {
      out = finalNoCov(.model, ...)
      table = rbind(table, "Fixed effect model" = out[[1]])
      plot[["Fixed effect model"]] = out[[2]]
    }

    for (i in unique(p)) {
      out = leavePOut(.model, i, ...)
      table = rbind(table, "Leave-p-out CV" = out[[1]])
      plot[[paste0("Leave-", i, "-out cross validation")]] = out[[2]]
    }

    for (i in unique(k)) {
      out = kFold(.model, i, ...)
      table = rbind(table, "k-fold CV" = out[[1]])
      plot[[paste0(i, "-fold cross validation")]] = out[[2]]
    }

    if (!is.null(extdata)) {
      out = extVal(.model, extdata, ...)
      table = rbind(table, "External CV" = out[[1]])
      plot[["External cross validation"]] = out[[2]]
    }

    gridExtra::grid.arrange(grobs = plot, ncol = 3) %>=>%
        assign("xPlot", .., envir = globalenv()) %=>%
        plot

    format(as.data.frame(t(round(table, 4))), scientific = F, digits = 4)
}