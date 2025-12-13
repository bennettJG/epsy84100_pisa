source("FLIT_imputation_no_repwt.R")
n_imp = 5
n_iter = 40

attempt1 <- shared_imputation_across_PVs("USA")
save(attempt1, file = "models/no_repwt/attempt1.rda")
summary(model)

data_us_small_numeric <- select_country_data(dataQQQ, "USA")
data_us_small <- data_us_small_numeric |>
  convert_bg_vars_factor()
data_bra_small_numeric <- select_country_data(dataQQQ, "BRA")
data_bra_small <- data_bra_small_numeric |>
  convert_bg_vars_factor()

model_us <- impute_country_with_each_pv(data_us_small_numeric, n_imp = n_imp, n_iter = n_iter)
summary(model_us$pooled)
save(model_us, file = "models/no_repwt/model_us.rda")

model_bra <- impute_country_with_each_pv(data_bra_small_numeric, n_imp = n_imp, n_iter = n_iter)
save(model_bra, file = "models/no_repwt/model_bra.rda")

# Individually imputed datasets from USA + BRA put together, with model fit to combined data
combined_indiv_imputations <- rbind(
  cbind(
    do.call(rbind, lapply(model_us$imputations, function(x)
      complete(x, action = "long"))) |>
      convert_bg_vars_factor() |>
      mutate(`.imp` = unlist(lapply(1:(n_imp * 10), function(x) {
        rep(x, nrow(data_us_small_numeric))
      }))),
    data.frame(Country = "USA")
  ),
  cbind(
    do.call(rbind, lapply(model_bra$imputations, function(x)
      complete(x, action = "long"))) |>
      convert_bg_vars_factor() |>
      mutate(`.imp` = unlist(lapply(1:(n_imp * 10), function(x) {
        rep(x, nrow(data_bra_small_numeric))
      }))),
    data.frame(Country = "BRA")
  )
)

combined_indiv_imputations <- split(combined_indiv_imputations, combined_indiv_imputations$.imp)
fit_combined_from_indiv <- lapply(combined_indiv_imputations, function(x) {
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
      #   repweights = "W_FSTURWT[0-9]+",
      #   type = "BRR",
      data = x,
      #   combined.weights = TRUE
    )
  )
})
pooled_combined_from_indiv <- pool(fit_combined_from_indiv)
model_combined_from_indiv <- list(imputations = combined_indiv_imputations,
                                  fitted = fit_combined_from_indiv,
                                  pooled = pooled_combined_from_indiv)
save(model_combined_from_indiv, file = "models/no_repwt/combined_from_indiv.rda")


############## Impute together, fit models separately #################
data_combined_by_pv <- rbind(
  data_us_small_numeric |> mutate(Country = "USA"),
  data_bra_small_numeric |> mutate(Country = "BRA")
) |>
  pivot_longer(PV1FLIT:PV10FLIT, values_to = "plausible_FLIT", names_to = "PV") |>
  split(~ PV)

weighted_pmm_combined_data <- lapply(data_combined_by_pv, impute_data, n_imp, n_iter)

plot(weighted_pmm_combined_data[[1]])
densityplot(
  weighted_pmm_combined_data[[1]],
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

complete_combined_by_pv <- do.call(rbind, lapply(weighted_pmm_combined_data, function(x) {
  complete(x, action = "stacked")
})) |>
  convert_bg_vars_factor()
complete_combined_by_pv$`.imp` <- unlist(lapply(1:(n_imp * 10), function(x) {
  rep(x, nrow(rbind(
    data_us_small_numeric, data_bra_small_numeric
  )))
}))

complete_us_from_combined <- complete_combined_by_pv |>
  filter(Country == "USA")
complete_us_from_combined <- split(complete_us_from_combined, complete_us_from_combined$`.imp`)
complete_bra_from_combined <- complete_combined_by_pv |>
  filter(Country == "BRA")
complete_bra_from_combined <- split(complete_bra_from_combined,
                                    complete_bra_from_combined$`.imp`)

fit_us_by_pv <- lapply(complete_us_from_combined, function(x) {
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
      #   repweights = "W_FSTURWT[0-9]+",
      #   type = "BRR",
      data = x,
      #   combined.weights = TRUE
    )
  )
})
fit_bra_by_pv <- lapply(complete_bra_from_combined, function(x) {
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
fit_combined_by_pv <- lapply(complete_combined_by_pv |> split(complete_combined_by_pv$.imp), function(x) {
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
      # repweights = "W_FSTURWT[0-9]+",
      # type = "BRR",
      data = x,
      #  combined.weights = TRUE
    )
  )
})
model_us_from_combined <- pool(fit_us_by_pv)
model_bra_from_combined <- pool(fit_bra_by_pv)
model_combined <- pool(fit_combined_by_pv)

combined_us <- list(imputations = weighted_pmm_combined_data,
                    fitted = fit_us_by_pv,
                    pooled = model_us_from_combined)
combined_bra <- list(imputations = weighted_pmm_combined_data,
                     fitted = fit_bra_by_pv,
                     pooled = model_bra_from_combined)
combined_us_bra <- list(imputations = weighted_pmm_combined_data,
                        fitted = fit_combined_by_pv,
                        pooled = model_combined)
save(combined_us, combined_bra, combined_us_bra, file = "models/no_repwt/model_us_bra.rda")

######## Sample from each country and use resulting imputation model to fill in remainder of USA/BRA missing data; compare results
data_sample_small <- slice_sample(dataQQQ, by = "Country", prop = 0.1) |>
  select(
    vars_bg,
    Inc.Level,
    Inc.Expect,
    Own.Room,
    starts_with("PV"),
    starts_with("W_FSTUWT"),
    "SchoolID",
    "CNTSTUID",
    "Country"
  )

allcountry_sample_by_pv <- data_sample_small |>
  pivot_longer(PV1FLIT:PV10FLIT, values_to = "plausible_FLIT", names_to = "PV") |>
  split( ~ PV)

print("Imputing based on sample of all countries...")
allcountry_pmm_by_pv <- lapply(allcountry_sample_by_pv, impute_data, n_imp, n_iter)
plot(allcountry_pmm_by_pv[[1]])
densityplot(
  allcountry_pmm_by_pv[[1]],
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
save(allcountry_pmm_by_pv, file = "models/no_repwt/allcountry_sample_mids.rda")

country_fromsample_by_pv <- function(all_country_imputed_by_pv,
                                     specific_country_data) {
  print("Constructing complete data sets...")
  specific_by_pv <- specific_country_data |>
    pivot_longer(PV1FLIT:PV10FLIT,
                 values_to = "plausible_FLIT",
                 names_to = "PV") |>
    split( ~ PV) |>
    lapply(function(x)
      x |> select(-SchoolID, -CNTSTUID, -PV))
  
  mids_country <- lapply(1:10, function(i) {
    mice.mids(all_country_imputed_by_pv[[i]],
              newdata = specific_by_pv[[i]],
              maxit = 1)
  })
  complete_by_pv <- do.call(rbind, lapply(mids_country, function(x)
    complete(x, action = "stacked"))) |>
    convert_bg_vars_factor()
  complete_by_pv$`.imp` <- unlist(lapply(1:(n_imp * 10), function(x) {
    rep(x, nrow(specific_country_data))
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
  results <- list(imputations = mids_country,
                  fitted = fit_by_pv,
                  pooled = model2)
  results
}

model_us_allsample <- country_fromsample_by_pv(allcountry_pmm_by_pv, data_us_small_numeric)
save(model_us_allsample, file = "models/no_repwt/model_us_allsample.rda")

model_bra_allsample <- country_fromsample_by_pv(allcountry_pmm_by_pv, data_bra_small_numeric)
save(model_bra_allsample, file = "models/no_repwt/model_bra_allsample.rda")

complete_allsample_by_pv <- do.call(rbind, lapply(allcountry_pmm_by_pv, function(x) {
  complete(x, action = "stacked")
})) |>
  convert_bg_vars_factor()
complete_allsample_by_pv$`.imp` <- unlist(lapply(1:(n_imp * 10), function(x) {
  rep(x, nrow(data_sample_small))
}))
complete_allsample_by_pv <- split(complete_allsample_by_pv, complete_allsample_by_pv$.imp)

allsample_fit <- lapply(complete_allsample_by_pv, function(x) {
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
model_allsample <- list(
  imputations = allcountry_pmm_by_pv,
  fitted = allsample_fit,
  pooled = pool(allsample_fit)
)
save(model_allsample, file = "models/no_repwt/model_allsample.rda")

# Full USA and BRA data with no imputation
us_noimpute <- use_pvs_noimpute(data_us_small_numeric)
save(us_noimpute, file = "models/no_repwt/noimpute/us_noimpute.rda")
bra_noimpute <- use_pvs_noimpute(data_bra_small_numeric)
save(bra_noimpute, file = "models/no_repwt/noimpute/bra_noimpute.rda")
us_bra_noimpute <- use_pvs_noimpute(rbind(data_us_small_numeric, data_bra_small_numeric))
save(us_bra_noimpute, file = "models/no_repwt/noimpute/us_bra_noimpute.rda")