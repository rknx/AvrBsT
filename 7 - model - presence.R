# Set up environment -----------------------------------------------------------

## Libraries
"lme4" %>=>% libInstall %=>% library(.., char = T)
"car" %>=>% libInstall %=>% library(.., char = T)

## Helper functions
"/crossvalidation.R" %=>% paste0(getwd(), ..) %=>% source
"/goftest.R" %=>% paste0(getwd(), ..) %=>% source
"/fidelity.R" %=>% paste0(getwd(), ..) %=>% source
"/prediction.R" %=>% paste0(getwd(), ..) %=>% source
"/animation.R" %=>% paste0(getwd(), ..) %=>% source
"/selection.R" %=>% paste0(getwd(), ..) %=>% source
"/simulation.R" %=>% paste0(getwd(), ..) %=>% source

## Message
message("***\n",
    "The steps in this sripts are not automated. I tried several automated\n",
    "ways, but they all had drawbacks. Go through starting model then factor\n",
    "reduction. If some factors can be removed, go back, rinse and repeat.\n",
    "***")

# Import data ------------------------------------------------------------------

## Load saved R objects
"/Data/data.rda" %=>% paste0(getwd(), ..) %=>% load(.., envir = globalenv())
"/Data/formula.rda" %=>% paste0(getwd(), ..) %=>% load(.., envir = globalenv())

## Change reference levels for genetype
dataBinom = within(dataBinom, (gene = relevel(gene, ref = "mut")))
tedataBinom = within(tedataBinom, (gene = relevel(gene, ref = "mut")))


# Actaul modeling --------------------------------------------------------------

bForm = presence ~ week + dis + gene +
    Temp.60cm.mean + Humidity.mean + Rain.sum +
    gene:week + gene:dis + gene:Humidity.mean +
    gene:Temp.60cm.mean +
    (1 | year / rep / id) + (1 | year : dir)

## Generate model
fit = glmer(
    formula = bForm,
    data = dataBinom,
    family = binomial(link = "logit"),
    control = glmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 2e5))
)

## Save the model as R object
c("fit") %=>%
    save(list = .., file = paste0(getwd(), "/Data/model.rda"))

## Load the model for easy access
# "/Data/model.rda" %=>% paste0(getwd(), ..) %=>% load(.., envir = globalenv())

## Model summary
summary(fit)

## Factor reduction ------------------------------------------------------------

## Wald's Chi Sq
options(contrasts = c("contr.sum", "contr.poly"))
Anova(fit, type = 2)
Anova(fit, type = 3)

## Drop1 method
drop1(fit)

# Likelihood ratio test --------------------------------------------------------

modelSelection(fit, nullVar = "id")


##Godness of fit ---------------------------------------------------------------

gofTest(fit)

fidelity(fit) %=>%
    ggsave(filename = paste0(getwd(), "/Output/fidelity.png"),
    .., width = 10, device = "png", height = 8, units = "in", dpi = 300)


# Cross validation -------------------------------------------------------------

xVal(fit, extdata = tedataBinom)

# Ratio of velocity ------------------------------------------------------------
est = x ->> fixef(fit)[x]

rate = round(
    ((est("week") + est("week:genewt")) / est("week")) *
    (est("dis") / (est("dis") + est("dis:genewt")))
, 2)

cat("XopJ2+ is", rate, "times faster than XopJ2-.")


# Prediction -------------------------------------------------------------------

## Tile output
expand.grid(
    week = 0:12,
    dis = 0:6,
    gene = c("mut", "wt")
) %=>%
    dispersal(
        fit,
        ..,
        spp = "X. perforans",
        gene = "XopJ2"
    ) %=>%
    ggsave(filename = paste0(getwd(), "/Output/prediction.pdf"),
    .., width = 18, device = cairo_pdf, height = 8, units = "in", dpi = 300)


## Multi-line output
expand.grid( 
    week = 0:10, 
    dis = seq(2, 8, 2), 
    gene = c("mut", "wt") 
) %=>% 
    dispersal2( 
        fit, 
        .., 
        spp = "X. perforans", 
        gene = "XopJ2" 
    ) %=>% 
    ggsave(filename = paste0(getwd(), "/Output/revival.pdf"), 
    .., width = 12, device = cairo_pdf, height = 8, units = "in", dpi = 300)    

# Animation --------------------------------------------------------------------
list(
    grid = c(15, 15),
    week = seq(0, 10.9, 0.1),
    gene = c("mut", "wt")
) %=>%
animation(
    fit,
    ..,
    out = "Output/animation.gif",
    pos = c("c", "m"),
    spp = "X. perforans",
    gene = "XopJ2"
)

# Field area simulation --------------------------------------------------------
list(
    grid = c(15, 15),
    week = c(0, 3, 6, 9),
    gene = c("mut", "wt")
) %=>%
animFrames(
    fit,
    ..,
    pos = c("c", "m"),
    spp = "X. perforans",
    gene = "XopJ2"
) %=>%
    ggsave(filename = paste0(getwd(), "/Output/anim-frames.pdf"),
    .., width = 15, device = cairo_pdf, height = 8, units = "in", dpi = 300)

# Distribution -----------------------------------------------------------------

list(
    grid = c(15, 15),
    week = c(0, 10),
    gene = c("mut", "wt")
) %=>%
    distribution(
        fit,
        ..,
        pos = c("c", "m"),
        spp = "X. perforans",
        gene = "XopJ2"
    ) %=>%
    ggsave(filename = paste0(getwd(), "/Output/distribution.pdf"),
    .., width = 5, device = cairo_pdf, height = 8, units = "in", dpi = 300)

# Simulation -------------------------------------------------------------------
simulField(
    ne = 1e8,
    p0 = 1e-7,
    nrep = 100,
    ngen = 14,
    w = c(9.13, 1),
    gene = "xopJ2"
) %!=>%
    ggsave(filename = paste0(getwd(), "/Output/simulation.pdf"),
    .., width = 15, device = cairo_pdf, height = 8, units = "in", dpi = 300)