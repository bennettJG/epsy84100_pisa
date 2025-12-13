source("FLIT_imputation_no_repwt.R")
n_imp = 5
n_iter = 40

data_us_small_numeric <- select_country_data(dataQQQ, "USA")

# Attempt 1: impute with all PVs in imputation model, then combine each of those imputed datasets with each PV
# So total number of datasets that get fit & pooled is 10*n_imp
# But I think this may be wrong, because I'm not convinced it's valid to reuse the same imputed values for each PV.

# There's no mice function for weighted imputation with methods other than pmm or norm (that I could find.)
# van Buuren says that "Imputation of categorical data is more difficult than continuous data [...]
# In many datasets, especially those with many categories, the ratio of the number of fitted parameters
# relative to the number of events easily drops below 10, which may lead to estimation problems.
# In those cases, the advice is to specify more robust methods, like pmm, cart or rf."
# https://stefvanbuuren.name/fimd/sec-categorical.html
# Need to run with more iterations and more imputations when doing this for real. But I'm just trying to get things
# working right now
shared_imputation_across_PVs <- function(c, n_imp = 5, maxit = 40) {
  country_data <- select_country_data(c)
  # Would ideally switch this to futuremice for parallel execution, but since not many are needed (given that each is reused with each PV)
  # it's OK for now
  weighted_pmm <- mice(
    country_data |>
      select(-SchoolID, -CNTSTUID, starts_with("W_FSTURWT")),
    maxit = maxit,
    method = "weighted.pmm",
    m = n_imp,
    imputationWeights = country_data$W_FSTUWT
  )
  plot(weighted_pmm)
  densityplot(weighted_pmm)
  
  tall_complete <- complete(weighted_pmm, action = "stacked") |>
    pivot_longer(PV1FLIT:PV10FLIT, values_to = "plausible_FLIT") |>
    mutate(imp = unlist(lapply(1:n_imp, function(x) {
      rep(10 * (x - 1) + (1:10), nrow(tall_complete))
    }))) |>
    convert_bg_vars_factor()
  
  tall_complete <- split(tall_complete, tall_complete$imp)
  fit_tall_complete <- lapply(tall_complete, function(x) {
    svyglm(
      plausible_FLIT ~
        Gender +
        Books.Home +
        Home.Cars +
        Home.Computer +
        Siblings +
        Immigrant +
        Father.Ed +
        Familiar.Fin.Concept +
        Home.Devices +
        Grade.Repeat,
      design = svydesign(
        ids = ~ 1,
        weights = ~ W_FSTUWT,
        # Uncomment these lines (and don't filter out W_FSTURWT above) to use replicate weights
        #   repweights = "W_FSTURWT[0-9]+",
        #  type = "BRR",
        data = x,
        #  combined.weights = TRUE
      )
    )
  })
  
  model <- pool(fit_tall_complete)
  list(imputations = weighted_pmm,
       fitted = fit_tall_complete,
       pooled = model)
}

############# Try imputing with all PVs and fitting to one
impute_country_with_all_pvs <- function(country_data_numeric,
                                        n_imp = 5,
                                        n_iter = 40) {
  print("Imputing...")
  weighted_pmm_all_pvs <- impute_data(country_data_numeric, 10 * n_imp, n_iter)
  # plot(weighted_pmm_all_pvs)
  # densityplot(
  #   weighted_pmm_all_pvs,
  #   data = ~ Books.Home +
  #     Home.Cars +
  #     Home.Computer +
  #     Siblings +
  #     Immigrant +
  #     Father.Ed +
  #     Grade.Repeat +
  #     Familiar.Fin.Concept +
  #     Home.Devices
  # )
  
  print("Constructing complete data sets...")
  
  PVs_keep <- cbind(seq_len(10 * n_imp * nrow(weighted_pmm_all_pvs$data)), 1:10)
  complete_by_pv <- complete(weighted_pmm_all_pvs, action = "long") |>
    convert_bg_vars_factor()
  PVs <- complete_by_pv |> select(starts_with("PV") &
                                    ends_with("FLIT"))
  complete_by_pv$plausible_FLIT <- PVs[PVs_keep]
  complete_by_pv <- split(complete_by_pv, complete_by_pv$.imp)
  
  print("Fitting models...")
  fit_by_pv <- lapply(complete_by_pv, function(x) {
    svyglm(
      plausible_FLIT ~
        Gender +
        Books.Home +
        Home.Cars +
        Home.Computer +
        Siblings +
        Immigrant +
        Father.Ed +
        Familiar.Fin.Concept +
        Home.Devices +
        Grade.Repeat,
      design = svydesign(
        ids = ~ 1,
        weights = ~ W_FSTUWT,
        #  repweights = "W_FSTURWT[0-9]+",
        #  type = "BRR",
        data = x,
        #  combined.weights = TRUE
      )
    )
  })
  print("Pooling...")
  model2 <- pool(fit_by_pv)
  results <- list(imputations = weighted_pmm_all_pvs,
                  fitted = fit_by_pv,
                  pooled = model2)
  results
}

model_us_allPV <- impute_country_with_all_pvs(data_us_small_numeric, n_imp)
save(model_us_allPV, file = "models/no_repwt/model_us_allPV.rda")

######## At Horacio's request, try imputing based on a single PV from each subject
impute_country_with_each_pv <- function(country_data_numeric, n_imp, n_iter = 40) {
  data_by_pv <- country_data_numeric |>
    pivot_longer(
      starts_with("PV"),
      names_pattern = "PV([0-9]*)([A-Z]*)",
      values_to = "plausible_val",
      names_to = c("PV", "domain")
    ) |>
    pivot_wider(
      names_from = domain,
      values_from = plausible_val,
      names_prefix = "plausible_"
    ) |>
    split( ~ PV)
  
  print("Imputing...")
  weighted_pmm_by_pv <- lapply(data_by_pv, impute_data, n_imp, n_iter)
  plot(weighted_pmm_by_pv[[1]])
  densityplot(
    weighted_pmm_by_pv[[1]],
    data = ~ Books.Home +
      Home.Cars +
      Home.Computer +
      Siblings +
      Immigrant +
      Father.Ed +
      Grade.Repeat +
      Familiar.Fin.Concept +
      Home.Devices
  )
  
  print("Constructing complete data sets...")
  complete_by_pv <- do.call(rbind, lapply(weighted_pmm_by_pv, function(x)
    complete(x, action = "stacked"))) |>
    convert_bg_vars_factor()
  complete_by_pv$`.imp` <- unlist(lapply(1:(n_imp * 10), function(x) {
    rep(x, nrow(country_data_numeric))
  }))
  
  complete_by_pv <- split(complete_by_pv, complete_by_pv$.imp)
  
  print("Fitting models...")
  fit_by_pv <- lapply(complete_by_pv, function(x) {
    svyglm(
      plausible_FLIT ~
        Gender +
        Books.Home +
        Home.Cars +
        Home.Computer +
        Siblings +
        Immigrant +
        Father.Ed +
        Familiar.Fin.Concept +
        Home.Devices +
        Grade.Repeat,
      design = svydesign(
        ids = ~ 1,
        weights = ~ W_FSTUWT,
        #  repweights = "W_FSTURWT[0-9]+",
        #  type = "BRR",
        data = x,
        #  combined.weights = TRUE
      )
    )
  })
  print("Pooling...")
  model2 <- pool(fit_by_pv)
  results <- list(imputations = weighted_pmm_by_pv,
                  fitted = fit_by_pv,
                  pooled = model2)
  results
}

model_us_1 <- impute_country_with_each_pv(data_us_small_numeric, n_imp = n_imp)
summary(model_us$pooled)
save(model_us_1, file = "models/no_repwt/model_us_1.rda")

### Ignore PV completely (average the values instead -- most wrong approach, for comparison only)

model_us_ignorePV <- svyglm(
  FLIT_Ave ~
    Gender +
    Books.Home +
    Home.Cars +
    Home.Computer +
    Siblings +
    Immigrant +
    Father.Ed +
    Familiar.Fin.Concept +
    Home.Devices +
    Grade.Repeat,
  design = svydesign(
    ids = ~ 1,
    weights = ~ W_FSTUWT,
    #  repweights = "W_FSTURWT[0-9]+",
    #  type = "BRR",
    #  combined.weights = TRUE,
    data = data_us_small_numeric |>
      convert_bg_vars_factor() |>
      mutate(FLIT_Ave = rowMeans(across(
        starts_with("PV") & ends_with("FLIT")
      ), na.rm = TRUE))
  )
)
save(model_us_ignorePV, file = "models/no_repwt/us_ignorePV.rda")