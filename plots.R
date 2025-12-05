library(tidyverse)
library(jtools)
library(mice)

plot_coefs(
  #combined_us$pooled,
  model_us_allsample$pooled,
  model_us$pooled,
  model_allsample$pooled,
  model.names = c(
    #"USA + BRA imputed",
    "Sample of all countries used for imputation model",
    "USA imputed alone",
    "Regression fit to sample of all countries"
  )
) +
  labs(title = "US financial literacy model coefficients")

plot_coefs(
  #combined_bra$pooled,
  model_bra_allsample$pooled,
  model_bra$pooled,
  model_allsample$pooled,
  model.names = c(
    #"USA + BRA imputed",
    "Sample of all countries used for imputation model",
    "BRA imputed alone",
    "Regression fit to sample of all countries"
  )
) +
  labs(title = "BRA financial literacy model coefficients")

plot_coefs(
  model_us$pooled,
  attempt1$pooled,
  model_us_allPV$pooled,
  model.names = c(
    "USA imputed alone - separate for each PV",
    "USA imputed alone - same imputed values used with each PV",
    "USA imputed alone - all PV used for imputation,\nimputed data not reused across PVs"
  )
) +
  labs(title = "US financial literacy model coefficients")

plot_coefs(
  model_us$pooled,
  model_us_1$pooled,
  model.names = c(
    "USA alone - separate for each FLIT PV,\nusing all MATH + READ PVs",
    "USA alone - one set of FLIT/MATH/READ PVs\nper imputation"
  )
) +
  labs(title = "US financial literacy model coefficients")

plot_coefs(
  model_us$pooled,
  combined_us_bra$pooled,
  model_us_noimpute,
  model.names = c(
    "USA imputed alone",
    "No covariates imputed (but PV pooled)",
    "No imputation (PVs averaged)"
  )
) +
  labs(title = "US financial literacy model coefficients")

plot_coefs(
  combined_us_bra$pooled,
  combined_us$pooled,
  combined_bra$pooled,
  model.names = c(
    "USA + BRA imputed",
    "USA + BRA imputed - USA fit",
    "USA + BRA imputed - BRA fit"
  )
) +
  labs(title = "Combined financial literacy model coefficients")

plot_coefs(
  combined_us_bra$pooled,
  combined_us_bra_qqq$pooled,
  model_combined_from_indiv$pooled,
  model.names = c(
    "USA + BRA imputed",
    "USA + BRA imputed - questionnaire respondents only",
    "Countries imputed separately, model fit together"
  )
) +
  labs(title = "Combined financial literacy model coefficients")

#flextable(combined_us$pooled)
