# This file loads and processes the data, and defines functions used in `full_usa_bra.R` (comparison of separate vs. combined imputation for
# the full USA and BRA datasets), `alternate_imputation_strategies.R` (alternative ways of using PVs in imputation), and `country_from_sample.R`
# (Imputation for 10% sample of the dataset, for each country alone vs. imputed alongside all other countries)
library(tidyverse)
library(foreign)
library(naniar)
library(visdat)
library(gridExtra)
library(grid)
library(mice)
library(miceadds)
library(survey)

### Load data
# Update this filepath or set your working directory to the folder where the questionnaire data is saved
path_qqq <- "CY08MSP_FLT_QQQ.SAV"

full_qqq <- read.spss(path_qqq,
                      to.data.frame = TRUE,
                      use.value.labels = F)

qqq_keep <- c(
  "CNT",
  "CNTSCHID",
  "CNTSTUID",
  "ST250Q01JA",
  # Own room
  "ST250Q02JA",
  # Computer for schoolwork
  "ST251Q01JA",
  # Cars in home
  "ST253Q01JA",
  # Devices in home
  "ST004D01T",
  # Gender
  "ST255Q01JA",
  # Books in home
  "ST230Q01JA",
  # Siblings
  "ST259Q01JA",
  # Current income level
  "ST259Q02JA",
  # Expected future income level
  "IMMIG",
  # Immigrant status
  "REPEAT",
  # Repeated grade
  "FISCED",
  # Father's education
  "FCFMLRTY",
  # Familiarity with financial concepts
  "W_FSTUWT",
  #19
  paste0("PV", 1:10, "MATH"),
  paste0("PV", 1:10, "READ"),
  paste0("PV", 1:10, "FLIT"),
  paste0("W_FSTURWT", 1:80),
  "SENWT"
)

###############################################################################
# 3. DEFINE NEW COLUMN LABELS (MATCHES QQQ_KEEP)
###############################################################################

new_labels <- c(
  "Country",
  "SchoolID",
  "CNTSTUID",
  "Own.Room",
  "Home.Computer",
  "Home.Cars",
  "Home.Devices",
  "Gender",
  "Books.Home",
  "Siblings",
  "Inc.Level",
  "Inc.Expect",
  "Immigrant",
  "Grade.Repeat",
  "Father.Ed",
  "Familiar.Fin.Concept",
  "W_FSTUWT",
  paste0("PV", 1:10, "MATH"),
  paste0("PV", 1:10, "READ"),
  paste0("PV", 1:10, "FLIT"),
  paste0("W_FSTURWT", 1:80),
  "SENWT"
)

dataQQQ <- full_qqq |> select(any_of(qqq_keep))
names(dataQQQ) <- new_labels

dataQQQ <- dataQQQ |> filter(Country != "CRI" & Country != "NOR")

######## Utility functions ############
vars_bg <- c(
  "Gender",
  "Books.Home",
  "Home.Cars",
  "Home.Computer",
  "Siblings",
  "Immigrant",
  "Father.Ed",
  "Grade.Repeat",
  "Home.Devices",
  "Familiar.Fin.Concept"
)
convert_bg_vars_factor <- function(x) {
  x |>
    mutate(
      `Books.Home` = factor(
        `Books.Home`,
        levels = 1:7,
        labels = c(
          "None",
          "1-10",
          "11-25",
          "26-100",
          "101-200",
          "201-500",
          "More than 500"
        )
      ),
      Gender = factor(Gender, labels = c("Female", "Male")),
      # In original var, 1 = Yes, 2 = No, so revalue to 0 = No, 1 = Yes
      `Own.Room` = factor(2 - `Own.Room`, labels = c("No", "Yes")),
      `Home.Cars` = factor(
        `Home.Cars`,
        levels = 1:4,
        labels = c("None", "One", "Two", "3 or more")
      ),
      `Home.Computer` = factor(2 - `Home.Computer`, labels = c("No", "Yes")),
      Siblings = factor(
        Siblings,
        levels = 1:4,
        labels = c("None", "One", "Two", "3 or more")
      ),
      Immigrant = factor(
        Immigrant,
        levels = 1:3,
        labels = c("Non-immigrant", "Second-gen", "First-gen")
      ),
      `Grade.Repeat` = factor(
        `Grade.Repeat`,
        levels = c(0, 1),
        labels = c("No", "Yes")
      ),
      `Home.Devices` = factor(
        `Home.Devices`,
        levels = 1:8,
        labels = c(
          "None",
          "One",
          "Two",
          "Three",
          "Four",
          "Five",
          "6-10",
          "More than 10"
        )
      )
    )
}

select_country_data <- function(df, c) {
  df |>
    filter(Country == c) |>
    select(
      vars_bg,
      c("Inc.Level", "Inc.Expect", "Own.Room"),
      starts_with("PV"),
      starts_with("W_FSTUWT"),
      "SchoolID",
      "CNTSTUID",
    )
}

#### IMPUTE DATA FOR ONE COUNTRY (or one combined group)
# Following Huang & Keller (2025), create a separate set of imputations for each PV, then pool everything
# Also results in 10*n_imp models to pool, but each PV gets its own set of imputed values
# Not sure if it's kosher to use the other PVs in the imputation model if we do things this way (Huang & Keller don't)
impute_data <- function(x, n_imp, n_iter) {
  futuremice(
    x |> select(-any_of(c(
      "SchoolID", "CNTSTUID", "PV"
    ))),
    maxit = n_iter,
    method = "weighted.pmm",
    m = n_imp,
    imputationWeights = x$W_FSTUWT,
    parallelseed = 1701,
    packages = c('miceadds', 'dplyr')
  )
}

impute_country_with_each_pv <- function(country_data_numeric,
                                        n_imp = 5,
                                        n_iter = 40) {
  data_by_pv <- country_data_numeric |>
    pivot_longer(PV1FLIT:PV10FLIT,
                 values_to = "plausible_FLIT",
                 names_to = "PV") |>
    split( ~ PV)
  
  print("Imputing...")
  weighted_pmm_by_pv <- lapply(data_by_pv, impute_data, n_imp, n_iter)
  # Uncomment to output trace and density plots for the imputations of the first FLIT PV
  # plot(weighted_pmm_by_pv[[1]])
  # densityplot(
  #   weighted_pmm_by_pv[[1]],
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

### No imputations
use_pvs_noimpute <- function(country_data) {
  tall_noimpute <- country_data |>
    pivot_longer(starts_with("PV") & ends_with("FLIT"), values_to = "plausible_FLIT") |>
    convert_bg_vars_factor() |>
    select(c(vars_bg, "W_FSTUWT", "name", "plausible_FLIT")) |>
    na.omit()
  tall_noimpute <- split(tall_noimpute, tall_noimpute$name)
  
  fit_noimpute <- lapply(tall_noimpute, function(x) {
    # Need to construct the formula because with the sample, all the responses for a variable may have the same value
    # This will cause the function to fail, because the factor will have only one level
    ok_vars <- lapply(vars_bg, function(y)
      ifelse(n_distinct(x[, y]) > 1, y, NA)) |>
      unlist() |>
      na.omit()
    svyglm(
      as.formula(paste0(
        "plausible_FLIT~", paste0(ok_vars, collapse = "+")
      )),
      design = svydesign(
        ids = ~ 1,
        weights = ~ W_FSTUWT,
        #  repweights = "W_FSTURWT[0-9]+",
        #  type = "BRR",
        data = x |>
          mutate(id = row_number()),
        rescale = T
        #  combined.weights = TRUE
      )
    )
  })
  
  model_noimpute <- pool(fit_noimpute)
  list(fitted = fit_noimpute, pooled = model_noimpute)
}