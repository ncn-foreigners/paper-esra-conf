library(tidyverse)
remotes::install_github("https://github.com/ncn-foreigners/singleRcapture/tree/0.2-development")
library(singleRcapture)

`policja-alko-bezpraw` <- readRDS("policja-alko-bezpraw.rds")
# No licence
ex2 <- `policja-alko-bezpraw` |> 
  filter(rok_wpis == 2021) |>
  filter(kwalifikacja == "Art. 180a") |>
  count(id, plec, wiek=2021-rok_ur, dzien) |>
  count(id, plec, wiek, sort = T, name = "counts") |>
  filter(wiek >= 18) |>
  mutate(wiek_cut = cut(wiek, c(18, 30, 40, 50, 60, Inf), include.lowest = T, right = T)) 

# Under the influence
ex1 <- `policja-alko-bezpraw` |> 
  filter(rok_wpis == 2021) |>
  filter(str_detect(kwalifikacja, "178")) |>
  count(id, plec, wiek=2021-rok_ur, dzien) |>
  count(id, plec, wiek, sort = T, name = "counts") |>
  filter(wiek >= 18) |>
  mutate(wiek_cut = cut(wiek, c(18, 30, 40, 50, 60, Inf), include.lowest = T, right = T)) 

# documentation for main funciton
?estimatePopsize

# ex1 ####
model1 <- estimatePopsize(
  formula = counts ~ wiek_cut + plec,
  model = ztpoisson(),
  data = ex1,
  method = "IRLS",
  controlMethod = controlMethod(verbose = 5)
)

model2 <- estimatePopsize(
  formula = counts ~ wiek_cut + plec,
  model = oiztpoisson(omegaLink = "cloglog"),
  data = ex1,
  method = "IRLS",
  controlMethod = controlMethod(verbose = 5),
  controlModel = controlModel(omegaFormula = ~ wiek_cut + plec)
)

model3 <- estimatePopsize(
  formula = counts ~ wiek_cut + plec,
  model = oiztgeom(omegaLink = "cloglog"),
  data = ex1,
  method = "IRLS",
  controlMethod = controlMethod(verbose = 5),
  controlModel = controlModel(omegaFormula = ~ wiek_cut + plec)
)

model4 <- estimatePopsize(
  formula = counts ~ wiek_cut + plec,
  model = ztoipoisson(omegaLink = "cloglog"),
  data = ex1,
  method = "IRLS",
  controlMethod = controlMethod(verbose = 5, stepsize = .2),
  controlModel = controlModel(omegaFormula = ~ wiek_cut + plec)
)

model5 <- estimatePopsize(
  formula = counts ~ wiek_cut + plec,
  model = ztoigeom(omegaLink = "cloglog"),
  data = ex1,
  method = "IRLS",
  controlMethod = controlMethod(verbose = 5),
  controlModel = controlModel(omegaFormula = ~ wiek_cut + plec)
)

model6 <- estimatePopsize(
  formula = counts ~ wiek_cut + plec,
  model = Hurdleztpoisson(piLink = "probit"),
  data = ex1,
  method = "IRLS",
  controlMethod = controlMethod(verbose = 5),
  controlModel = controlModel(piFormula = ~ wiek_cut + plec)
)

model7 <- estimatePopsize(
  formula = counts ~ wiek_cut + plec,
  model = Hurdleztgeom(piLink = "probit"),
  data = ex1,
  method = "IRLS",
  controlMethod = controlMethod(verbose = 5, stepsize = .5),
  controlModel = controlModel(piFormula = ~ wiek_cut + plec)
)

model8 <- estimatePopsize(
  formula = counts ~ wiek_cut + plec,
  model = ztHurdlepoisson(piLink = "probit"),
  data = ex1,
  method = "IRLS",
  controlMethod = controlMethod(verbose = 5, 
                                maxiter = 1000, 
                                momentumFactor = 1),
  controlModel = controlModel(piFormula = ~ wiek_cut + plec)
)

model9 <- estimatePopsize(
  formula = counts ~ wiek_cut + plec,
  model = ztHurdlepoisson(piLink = "probit"),
  data = ex1,
  method = "IRLS",
  controlMethod = controlMethod(verbose = 5, 
                                maxiter = 1000, 
                                momentumFactor = 1),
  controlModel = controlModel(piFormula = ~ wiek_cut + plec)
)

list(model1, model2, model3, model4, model5, model6, model7, model8, model9) %>% 
  lapply(BIC) %>% 
  unlist

list(model1, model2, model3, model4, model5, model6, model7, model8, model9) %>% 
  lapply(popSizeEst)

# ztoigeom
modelZtoiGeom <- estimatePopsize(
  formula = counts ~ plec,
  model = ztoigeom(omegaLink = "cloglog"),
  data = ex1,
  method = "IRLS",
  controlMethod = controlMethod(verbose = 5),
  controlModel = controlModel(omegaFormula = ~ wiek_cut)
)

# oiztgeom
modelOiztGeom <- estimatePopsize(
  formula = counts ~ plec,
  model = oiztgeom(omegaLink = "cloglog"),
  data = ex1,
  method = "IRLS",
  controlMethod = controlMethod(verbose = 5),
  controlModel = controlModel(omegaFormula = ~ wiek_cut)
)

# final model
modelZtoiGeom <- estimatePopsize(
  formula = counts ~ plec,
  model = ztoigeom(omegaLink = "cloglog"),
  data = ex1,
  method = "IRLS",
  popVar = "bootstrap",
  controlMethod = controlMethod(verbose = 5, saveIRLSlogs = TRUE),
  controlModel  = controlModel(omegaFormula = ~ wiek_cut),
  controlPopVar = controlPopVar(
    bootType = "semiparametric",
    B = 5000,
    bootstrapVisualTrace = TRUE,
    traceBootstrapSize = TRUE
  )
)

save(modelZtoiGeom, file = "EsraModel1_bootstrap.RData")

# ex2 ####
model1 <- estimatePopsize(
  formula = counts ~ wiek_cut + plec,
  model = ztpoisson(),
  data = ex2,
  method = "IRLS",
  controlMethod = controlMethod(verbose = 5)
)

model2 <- estimatePopsize(
  formula = counts ~ wiek_cut + plec,
  model = oiztpoisson(omegaLink = "cloglog"),
  data = ex2,
  method = "IRLS",
  controlMethod = controlMethod(verbose = 5),
  controlModel = controlModel(omegaFormula = ~ wiek_cut + plec),
  controlPopVar = controlPopVar(covType = "Fisher")
)

model3 <- estimatePopsize(
  formula = counts ~ wiek_cut + plec,
  model = oiztgeom(omegaLink = "cloglog"),
  data = ex2,
  method = "IRLS",
  controlMethod = controlMethod(verbose = 5),
  controlModel = controlModel(omegaFormula = ~ wiek_cut + plec)
)

model4 <- estimatePopsize(
  formula = counts ~ wiek_cut + plec,
  model = ztoipoisson(omegaLink = "cloglog"),
  data = ex2,
  method = "IRLS",
  controlMethod = controlMethod(verbose = 5, stepsize = 1),
  controlModel = controlModel(omegaFormula = ~ wiek_cut + plec)
)

model5 <- estimatePopsize(
  formula = counts ~ wiek_cut + plec,
  model = ztoigeom(omegaLink = "cloglog"),
  data = ex2,
  method = "IRLS",
  controlMethod = controlMethod(verbose = 5),
  controlModel = controlModel(omegaFormula = ~ wiek_cut + plec)
)

model6 <- estimatePopsize(
  formula = counts ~ wiek_cut + plec,
  model = Hurdleztpoisson(piLink = "probit"),
  data = ex2,
  method = "IRLS",
  controlMethod = controlMethod(verbose = 5),
  controlModel = controlModel(piFormula = ~ wiek_cut + plec)
)

model7 <- estimatePopsize(
  formula = counts ~ wiek_cut + plec,
  model = Hurdleztgeom(piLink = "probit"),
  data = ex2,
  method = "IRLS",
  controlMethod = controlMethod(verbose = 5, stepsize = 1),
  controlModel = controlModel(piFormula = ~ wiek_cut + plec)
)

model8 <- estimatePopsize(
  formula = counts ~ wiek_cut + plec,
  model = ztHurdlepoisson(piLink = "probit"),
  data = ex2,
  method = "IRLS",
  controlMethod = controlMethod(verbose = 5, 
                                momentumFactor = 1),
  controlModel = controlModel(piFormula = ~ wiek_cut + plec)
)

model9 <- estimatePopsize(
  formula = counts ~ wiek_cut + plec,
  model = ztHurdlepoisson(piLink = "probit"),
  data = ex2,
  method = "IRLS",
  controlMethod = controlMethod(verbose = 5,  
                                momentumFactor = 2),
  controlModel = controlModel(piFormula = ~ wiek_cut + plec)
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
  data = ex2,
  method = "IRLS",
  popVar = "bootstrap",
  controlMethod = controlMethod(verbose = 5, saveIRLSlogs = TRUE),
  controlModel  = controlModel(omegaFormula = ~ wiek_cut),
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
  data = ex2,
  method = "IRLS",
  popVar = "bootstrap",
  controlMethod = controlMethod(verbose = 5, saveIRLSlogs = TRUE),
  controlModel  = controlModel(omegaFormula = ~ wiek_cut),
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

save(modelZtoiGeom, file = "EsraModel2_bootstrap.RData")
save(modelOiztGeom, file = "EsraModel2_bootstrap_worse.RData")
