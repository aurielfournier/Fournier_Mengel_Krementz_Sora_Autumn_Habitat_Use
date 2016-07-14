# needed packages

library(unmarked)

# loading the data, these are the models that I ran, which you can get from the gdistsamp_model_run.R script

load("./poisson_models.Rdata")


list15  = fitList(density.modelsP)
(mm15 = modSel(list15))