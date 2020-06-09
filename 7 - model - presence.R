# Set up environment -----------------------------------------------------------

## Libraries
"lme4" %>=>% libInstall %=>% library(.., char = T)
"car" %>=>% libInstall %=>% library(.., char = T)

## Helper functions
"/crossvalidation.R" %=>% paste0(getwd(), ..) %=>% source
"/goftest.R" %=>% paste0(getwd(), ..) %=>% source

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
# tedataBinom  %<=>% filter(.., week < 8)


# Actaul modeling --------------------------------------------------------------

## Generate model
fit = glmer(
    presence ~
        week + dis + gene + dir +
        Temp.60cm.mean + Wind.max + Humidity.mean + Radiation.mean + Rain.sum +
        gene:week + gene:dis + gene:Humidity.mean + gene:Radiation.mean +
        (1 | year / rep / id) + (1 | year : dir),
    data = dataBinom,
    family = binomial(link = "logit"),
    control = glmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 2e5))
)

## Model summary
summary(fit)



## Factor reduction ------------------------------------------------------------

## Wald's Chi Sq
Anova(fit)

## Drop1 method
drop1(fit)

# Cross validation -------------------------------------------------------------

