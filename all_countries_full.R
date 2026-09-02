source("FLIT_imputation_no_repwt.R")
n_imp = 5
n_iter = 40
# Using a library for timing so it's a little cleaner
library(tictoc)
tic.clearlog()

### After you've imputed the all-country data once and saved it once, you can uncomment this code to reload that object into memory
# and comment out lines 19-68 so you don't re-run that very time consuming part.
# load("~/PISA data/epsy84100_pisa/models/no_repwt/allcountry_mids_c.rda")
# dataQQQ <- do.call(
#   rbind,
#   lapply(allcountry_pmm_by_pv_c, function(x) x$data)
# ) |>
#   rownames_to_column() |>
#   mutate(rowname = str_replace(rowname, "\\.[0-9]*", "")) |>
#   pivot_wider(names_from = rowname, values_from = plausible_FLIT)

dataQQQ <- dataQQQ |>
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
  )

allcountry_by_pv_c <- dataQQQ |>
  pivot_longer(
    PV1FLIT:PV10FLIT,
    values_to = "plausible_FLIT",
    names_to = "PV"
  ) |>
  split(~PV)

print("Imputing with all countries...")
tic("All countries combined imputation")
allcountry_pmm_by_pv_c <- lapply(allcountry_by_pv_c, impute_data, n_imp, n_iter)
toc(log = T)
save(allcountry_pmm_by_pv_c, file = "models/allcountry_mids_c.rda")

# imputation checks
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

# Code to fit model for a specified country using the data imputed with all countries
library(parallelly)
library(parallel)
country_fromsample_by_pv <- function(specific_country_data) {
  print("Constructing complete data sets...")
  specific_by_pv <- specific_country_data |>
    pivot_longer(
      PV1FLIT:PV10FLIT,
      values_to = "plausible_FLIT",
      names_to = "PV"
    ) |>
    split(~PV) |>
    lapply(function(x) {
      x |> select(-SchoolID, -CNTSTUID, -PV)
    })
  tic(paste(
    specific_country_data$Country[[1]],
    "get data from imputed all countries"
  ))
  cl <- makeClusterPSOCK(4)
  clusterExport(
    cl,
    c(
      "mice.mids",
      "allcountry_pmm_by_pv_c",
      "mice.impute.weighted.pmm"
    )
  )
  mids_country <- parLapply(cl, 1:10, function(i) {
    mice.mids(
      allcountry_pmm_by_pv_c[[i]],
      newdata = specific_by_pv[[i]],
      maxit = 1
    )
  })
  parallel::stopCluster(cl)
  toc(log = T)
  complete_by_pv <- do.call(
    rbind,
    lapply(mids_country, function(x) {
      complete(x, action = "stacked")
    })
  ) |>
    convert_bg_vars_factor()
  complete_by_pv$`.imp` <- unlist(lapply(1:(n_imp * 10), function(x) {
    rep(x, nrow(specific_country_data))
  }))

  complete_by_pv <- split(complete_by_pv, complete_by_pv$.imp)

  print("Fitting models...")
  tic(paste(specific_country_data$Country[[1]], "model fitting"))
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
  toc(log = T)
  print("Pooling...")
  tic(paste(specific_country_data$Country[[1]], "pooling"))
  model2 <- pool(fit_by_pv)
  toc(log = T)
  results <- list(
    imputations = mids_country,
    fitted = fit_by_pv,
    pooled = model2
  )
  results
}

# Loop through countries and save model for each of them fitted on data from
# all-country imputation
for (c in unlist(dataQQQ |> distinct(Country))) {
  model_country_all <- country_fromsample_by_pv(dataQQQ |> filter(Country == c))
  saveRDS(model_country_all, file = paste0("models/", c, "_all.rds"))
}

# Loop through countries and impute data for just that country, then fit model and save
for (c in unlist(dataQQQ |> distinct(Country))) {
  tic(paste(c, "alone, impute and fit models"))
  model_country_alone <- impute_country_with_each_pv(
    dataQQQ |> filter(Country == c),
    n_imp = 5
  )
  toc(log = T)
  saveRDS(
    model_country_alone,
    file = paste0("models/", c, "_small_alone.rds")
  )
}

saveRDS(tic.log, "time logging.rds")

# Models for each country with no imputation
for (c in unlist(dataQQQ |> distinct(Country))) {
  model_noimpute <- use_pvs_noimpute(dataQQQ |> filter(Country == c))
  saveRDS(model_noimpute, file = paste0("models/", c, "_noimpute.rds"))
}
