# packages needed

remotes::install_github("https://github.com/ncn-foreigners/singleRcapture")

library(tidyverse)
library(singleRcapture)

police_drunk_or_no_licence <- readRDS("data/policja-alko-bezpraw.rds")

# No licence
df_no_licence <- police_drunk_or_no_licence |> 
  filter(rok_wpis == 2021) |>
  filter(kwalifikacja == "Art. 180a") |>
  count(id, gender, age=2021-rok_ur, dzien) |>
  count(id, gender, age, sort = T, name = "counts") |>
  filter(age >= 18) |>
  mutate(age_cut = cut(age, c(18, 30, 40, 50, 60, Inf), include.lowest = T, right = T)) |>
  rename(c(gender = "gender"))

# Under the influence
df_drunk <- police_drunk_or_no_licence |> 
  filter(rok_wpis == 2021) |>
  filter(str_detect(kwalifikacja, "178")) |>
  count(id, gender, age=2021-rok_ur, dzien) |>
  count(id, gender, age, sort = T, name = "counts") |>
  filter(age >= 18) |>
  mutate(age_cut = cut(age, c(18, 30, 40, 50, 60, Inf), include.lowest = T, right = T)) |>
  rename(c(gender = "gender"))

# documentation for main funciton
?estimatePopsize

# code for loading final models without the need for running the code: ####

# The names of variables are in polish which is the only change with respect
# to objects created by code in this file

# for df_drunk data frame
load("data/EsraModel1_bootstrap.Rdata")

# get rootogram
plot(modelZtoiGeom, "rootogram")

# get fitting log
modelZtoiGeom$fittingLog

# omit gradient from fitting log
modelZtoiGeom$fittingLog[, 1:3]

# get full summary of results
summary(modelZtoiGeom)

# get histogram of bootstrap values
plot(modelZtoiGeom, "bootHist", labels = TRUE, ylim = c(0, 1200))

## verify normality of hat(N)

qqnorm(modelZtoiGeom$populationSize$boot)
qqline(modelZtoiGeom$populationSize$boot)




# for df_no_licence data frame
load("data/EsraModel2_bootstrap.RData")

# get rootogram
plot(modelZtoiGeom, "rootogram")

# get fitting log
modelZtoiGeom$fittingLog

# omit gradient from fitting log
modelZtoiGeom$fittingLog[, 1:3]

# get full summary of results
summary(modelZtoiGeom)

# get histogram of bootstrap values
plot(modelZtoiGeom, "bootHist", labels = TRUE, ylim = c(0, 1100))

## verify normality of hat(N)

qqnorm(modelZtoiGeom$populationSize$boot)
qqline(modelZtoiGeom$populationSize$boot)


# Model which we ultimately decided against

load("data/EsraModel2_bootstrap_worse.RData")

# get rootogram
plot(modelOiztGeom, "rootogram")

# get fitting log
modelOiztGeom$fittingLog

# omit gradient from fitting log
modelOiztGeom$fittingLog[, 1:3]

# get full summary of results
summary(modelOiztGeom)

# get histogram of bootstrap values
plot(modelOiztGeom, "bootHist", labels = TRUE, ylim = c(0, 1300))

## verify normality of hat(N)

qqnorm(modelOiztGeom$populationSize$boot)
qqline(modelOiztGeom$populationSize$boot)


# df_drunk ####
model1 <- estimatePopsize(
  formula = counts ~ age_cut + gender,
  model = ztpoisson(),
  data = df_drunk,
  method = "IRLS",
  controlMethod = controlMethod(verbose = 5)
)

model2 <- estimatePopsize(
  formula = counts ~ age_cut + gender,
  model = oiztpoisson(omegaLink = "cloglog"),
  data = df_drunk,
  method = "IRLS",
  controlMethod = controlMethod(verbose = 5),
  controlModel = controlModel(omegaFormula = ~ age_cut + gender)
)

model3 <- estimatePopsize(
  formula = counts ~ age_cut + gender,
  model = oiztgeom(omegaLink = "cloglog"),
  data = df_drunk,
  method = "IRLS",
  controlMethod = controlMethod(verbose = 5),
  controlModel = controlModel(omegaFormula = ~ age_cut + gender)
)

model4 <- estimatePopsize(
  formula = counts ~ age_cut + gender,
  model = ztoipoisson(omegaLink = "cloglog"),
  data = df_drunk,
  method = "IRLS",
  controlMethod = controlMethod(verbose = 5, stepsize = .2),
  controlModel = controlModel(omegaFormula = ~ age_cut + gender)
)

model5 <- estimatePopsize(
  formula = counts ~ age_cut + gender,
  model = ztoigeom(omegaLink = "cloglog"),
  data = df_drunk,
  method = "IRLS",
  controlMethod = controlMethod(verbose = 5),
  controlModel = controlModel(omegaFormula = ~ age_cut + gender)
)

model6 <- estimatePopsize(
  formula = counts ~ age_cut + gender,
  model = Hurdleztpoisson(piLink = "probit"),
  data = df_drunk,
  method = "IRLS",
  controlMethod = controlMethod(verbose = 5),
  controlModel = controlModel(piFormula = ~ age_cut + gender)
)

model7 <- estimatePopsize(
  formula = counts ~ age_cut + gender,
  model = Hurdleztgeom(piLink = "probit"),
  data = df_drunk,
  method = "IRLS",
  controlMethod = controlMethod(verbose = 5, stepsize = .5),
  controlModel = controlModel(piFormula = ~ age_cut + gender)
)

model8 <- estimatePopsize(
  formula = counts ~ age_cut + gender,
  model = ztHurdlepoisson(piLink = "probit"),
  data = df_drunk,
  method = "IRLS",
  controlMethod = controlMethod(verbose = 5, 
                                maxiter = 1000, 
                                momentumFactor = 1),
  controlModel = controlModel(piFormula = ~ age_cut + gender)
)

model9 <- estimatePopsize(
  formula = counts ~ age_cut + gender,
  model = ztHurdlepoisson(piLink = "probit"),
  data = df_drunk,
  method = "IRLS",
  controlMethod = controlMethod(verbose = 5, 
                                maxiter = 1000, 
                                momentumFactor = 1),
  controlModel = controlModel(piFormula = ~ age_cut + gender)
)

list(model1, model2, model3, model4, model5, model6, model7, model8, model9) %>% 
  lapply(BIC) %>% 
  unlist

list(model1, model2, model3, model4, model5, model6, model7, model8, model9) %>% 
  lapply(popSizeEst)

# ztoigeom
modelZtoiGeom <- estimatePopsize(
  formula = counts ~ gender,
  model = ztoigeom(omegaLink = "cloglog"),
  data = df_drunk,
  method = "IRLS",
  controlMethod = controlMethod(verbose = 5),
  controlModel = controlModel(omegaFormula = ~ age_cut)
)

# oiztgeom
modelOiztGeom <- estimatePopsize(
  formula = counts ~ gender,
  model = oiztgeom(omegaLink = "cloglog"),
  data = df_drunk,
  method = "IRLS",
  controlMethod = controlMethod(verbose = 5),
  controlModel = controlModel(omegaFormula = ~ age_cut)
)

# final model
modelZtoiGeom <- estimatePopsize(
  formula = counts ~ gender,
  model = ztoigeom(omegaLink = "cloglog"),
  data = df_drunk,
  method = "IRLS",
  popVar = "bootstrap",
  controlMethod = controlMethod(verbose = 5, saveIRLSlogs = TRUE),
  controlModel  = controlModel(omegaFormula = ~ age_cut),
  controlPopVar = controlPopVar(
    bootType = "semiparametric",
    B = 5000,
    bootstrapVisualTrace = TRUE,
    traceBootstrapSize   = TRUE
  )
)

# df_no_licence ####
model1 <- estimatePopsize(
  formula = counts ~ age_cut + gender,
  model = ztpoisson(),
  data = df_no_licence,
  method = "IRLS",
  controlMethod = controlMethod(verbose = 5)
)

model2 <- estimatePopsize(
  formula = counts ~ age_cut + gender,
  model = oiztpoisson(omegaLink = "cloglog"),
  data = df_no_licence,
  method = "IRLS",
  controlMethod = controlMethod(verbose = 5),
  controlModel = controlModel(omegaFormula = ~ age_cut + gender),
  controlPopVar = controlPopVar(covType = "Fisher")
)

model3 <- estimatePopsize(
  formula = counts ~ age_cut + gender,
  model = oiztgeom(omegaLink = "cloglog"),
  data = df_no_licence,
  method = "IRLS",
  controlMethod = controlMethod(verbose = 5),
  controlModel = controlModel(omegaFormula = ~ age_cut + gender)
)

model4 <- estimatePopsize(
  formula = counts ~ age_cut + gender,
  model = ztoipoisson(omegaLink = "cloglog"),
  data = df_no_licence,
  method = "IRLS",
  controlMethod = controlMethod(verbose = 5, stepsize = 1),
  controlModel = controlModel(omegaFormula = ~ age_cut + gender)
)

model5 <- estimatePopsize(
  formula = counts ~ age_cut + gender,
  model = ztoigeom(omegaLink = "cloglog"),
  data = df_no_licence,
  method = "IRLS",
  controlMethod = controlMethod(verbose = 5),
  controlModel = controlModel(omegaFormula = ~ age_cut + gender)
)

model6 <- estimatePopsize(
  formula = counts ~ age_cut + gender,
  model = Hurdleztpoisson(piLink = "probit"),
  data = df_no_licence,
  method = "IRLS",
  controlMethod = controlMethod(verbose = 5),
  controlModel = controlModel(piFormula = ~ age_cut + gender)
)

model7 <- estimatePopsize(
  formula = counts ~ age_cut + gender,
  model = Hurdleztgeom(piLink = "probit"),
  data = df_no_licence,
  method = "IRLS",
  controlMethod = controlMethod(verbose = 5, stepsize = 1),
  controlModel = controlModel(piFormula = ~ age_cut + gender)
)

model8 <- estimatePopsize(
  formula = counts ~ age_cut + gender,
  model = ztHurdlepoisson(piLink = "probit"),
  data = df_no_licence,
  method = "IRLS",
  controlMethod = controlMethod(verbose = 5, 
                                momentumFactor = 1),
  controlModel = controlModel(piFormula = ~ age_cut + gender)
)

model9 <- estimatePopsize(
  formula = counts ~ age_cut + gender,
  model = ztHurdlepoisson(piLink = "probit"),
  data = df_no_licence,
  method = "IRLS",
  controlMethod = controlMethod(verbose = 5,  
                                momentumFactor = 2),
  controlModel = controlModel(piFormula = ~ age_cut + gender)
)

list(model1, model2, model3, model4, model5, model6, model7, model8, model9) %>% 
  lapply(BIC) %>% 
  unlist

list(model1, model2, model3, model4, model5, model6, model7, model8, model9) %>% 
  lapply(popSizeEst)

# ztoigeom
modelZtoiGeom <- estimatePopsize(
  formula = counts ~ 1,
  model = ztoigeom(omegaLink = "cloglog"),
  data = df_no_licence,
  method = "IRLS",
  popVar = "bootstrap",
  controlMethod = controlMethod(verbose = 5, saveIRLSlogs = TRUE),
  controlModel  = controlModel(omegaFormula = ~ age_cut),
  controlPopVar = controlPopVar(
    bootType = "semiparametric",
    B = 5000,
    bootstrapVisualTrace = TRUE,
    traceBootstrapSize = TRUE
  )
)

# oiztgeom
modelOiztGeom <- estimatePopsize(
  formula = counts ~ 1,
  model = oiztgeom(omegaLink = "cloglog"),
  data = df_no_licence,
  method = "IRLS",
  popVar = "bootstrap",
  controlMethod = controlMethod(verbose = 5, saveIRLSlogs = TRUE),
  controlModel  = controlModel(omegaFormula = ~ age_cut),
  controlPopVar = controlPopVar(
    bootType = "semiparametric",
    B = 5000,
    bootstrapVisualTrace = TRUE,
    traceBootstrapSize = TRUE
  )
)

summary(modelOiztGeom)
summary(modelZtoiGeom)

cbind(dfbeta(modelOiztGeom), dfbeta(modelZtoiGeom)) %>% summary
