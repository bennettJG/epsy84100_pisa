# Don't re-run this every time, since there's no way to set seed for
# replicability
# Instead, use code below to recreate the original df from the saved mids object
data_sample_small_c <- slice_sample(dataQQQ, by = "Country", prop = 0.1) |>
  select(
    Gender,
    Own.Room,
    Books.Home,
    Home.Cars,
    Home.Computer,
    Siblings,
    Immigrant,
    Father.Ed,
    Inc.Level,
    Inc.Expect,
    Familiar.Fin.Concept,
    Grade.Repeat,
    Home.Devices,
    starts_with("PV") & !ends_with("Ave"),
    starts_with("W_FSTUWT"),
    "SchoolID",
    "CNTSTUID",
    "Country"
  ) |>
  filter(Country != "NOR")

allcountry_sample_by_pv_c <- data_sample_small_c |>
  pivot_longer(
    PV1FLIT:PV10FLIT,
    values_to = "plausible_FLIT",
    names_to = "PV"
  ) |>
  split(~PV)

print("Imputing based on sample of all countries...")
allcountry_pmm_by_pv_c <- lapply(
  allcountry_sample_by_pv_c,
  impute_data,
  n_imp,
  n_iter
)
plot(allcountry_pmm_by_pv_c[[1]])
densityplot(
  allcountry_pmm_by_pv_c[[1]],
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
save(
  allcountry_pmm_by_pv_c,
  file = "models/no_repwt/allcountry_sample_mids_c.rda"
)

### Load saved data
load("~/PISA data/epsy84100_pisa/models/no_repwt/allcountry_sample_mids_c.rda")
data_sample_small_c <- do.call(
  rbind,
  lapply(allcountry_pmm_by_pv_c, function(x) x$data)
) |>
  rownames_to_column() |>
  mutate(rowname = str_replace(rowname, "\\.[0-9]*", "")) |>
  pivot_wider(names_from = rowname, values_from = plausible_FLIT)

library(parallelly)
library(parallel)
country_fromsample_by_pv <- function(
  specific_country_data
) {
  print("Constructing complete data sets...")
  specific_by_pv <- specific_country_data |>
    pivot_longer(
      PV1FLIT:PV10FLIT,
      values_to = "plausible_FLIT",
      names_to = "PV"
    ) |>
    split(~PV) |>
    lapply(function(x) x |> select(-SchoolID, -CNTSTUID, -PV))

  cl <- makeClusterPSOCK(4)
  clusterExport(
    cl,
    c("mice.mids", "allcountry_pmm_by_pv_c", "mice.impute.weighted.pmm")
  )
  mids_country <- parLapply(cl, 1:10, function(i) {
    mice.mids(
      allcountry_pmm_by_pv_c[[i]],
      newdata = specific_by_pv[[i]],
      maxit = 1
    )
  })
  parallel::stopCluster(cl)
  complete_by_pv <- do.call(
    rbind,
    lapply(mids_country, function(x) complete(x, action = "stacked"))
  ) |>
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
        ids = ~1,
        weights = ~W_FSTUWT,
        #  repweights = "W_FSTURWT[0-9]+",
        #  type = "BRR",
        data = x,
        #  combined.weights = TRUE
      )
    )
  })
  print("Pooling...")
  model2 <- pool(fit_by_pv)
  results <- list(
    imputations = mids_country,
    fitted = fit_by_pv,
    pooled = model2
  )
  results
}

for (c in unlist(data_sample_small_c |> distinct(Country))) {
  model_country_allsample <- country_fromsample_by_pv(
    data_sample_small_c |> filter(Country == c)
  )
  saveRDS(
    model_country_allsample,
    file = paste0("models/fromsamp/", c, "_small_allsample.rds")
  )
}

for (c in unlist(data_sample_small_c |> distinct(Country))) {
  model_country_alone <- impute_country_with_each_pv(
    data_sample_small_c |> filter(Country == c),
    n_imp = 5
  )
  saveRDS(
    model_country_alone,
    file = paste0("models/fromsamp/", c, "_small_alone.rds")
  )
}

for (c in unlist(data_sample_small_c |> distinct(Country))) {
  model_noimpute <- use_pvs_noimpute(
    data_sample_small_c |> filter(Country == c)
  )
  saveRDS(
    model_noimpute,
    file = paste0("models/fromsamp/", c, "_noimpute.rds")
  )
}
