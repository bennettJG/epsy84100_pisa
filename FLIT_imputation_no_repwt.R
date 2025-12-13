library(tidyverse)
library(foreign)
library(naniar)
library(visdat)
library(gridExtra)
library(grid)
library(mice)
library(miceadds)
library(survey)

###############################################################################
# 1. LOAD FULL 20-COUNTRY FILES
###############################################################################

path_qqq <- "CY08MSP_FLT_QQQ.SAV"

full_qqq <- read.spss(path_qqq, to.data.frame = TRUE, use.value.labels = F)

country_list <- sort(unique(full_qqq$CNT))
country_list <- country_list[country_list != "CRI"]
#message("Countries detected: ", paste(country_list, collapse = ", "))

###############################################################################
# 2. DEFINE QQQ VARIABLES TO KEEP (REDUCE FROM ~1000 → ~90 VARIABLES)
###############################################################################

qqq_keep <- c(
  "CNT",
  "CNTSCHID",
  "CNTSTUID",
  "REGION",
  "OECD", #1
  "LANGTEST_QQQ",
  "ST250Q01JA",
  "ST250Q02JA",
  "ST251Q01JA",
  "ST253Q01JA", #2
  "Option_FL",
  "Option_ICTQ",
  "Option_WBQ",
  "Option_PQ", #3
  "Option_TQ",
  "ST001D01T",
  "ST003D02T",
  "ST003D03T", #4
  "ST004D01T",
  "ST255Q01JA",
  "ST230Q01JA",
  "ST005Q01JA", #5
  "ST007Q01JA",
  "ST019AQ01T",
  "ST019BQ01T", #6
  "ST019CQ01T",
  "ST021Q01TA",
  "ST022Q01TA",
  "ST259Q01JA",
  "ST259Q02JA", #7
  "ST347Q01JA",
  "ST347Q02JA",
  "PROGN", #8
  "AGE",
  "IMMIG",
  "COBN_S", #9
  "COBN_M",
  "COBN_F", #10
  "LANGN",
  "REPEAT",
  "MISSSC", #11
  "SISCO",
  "INFOSEEK",
  "FAMSUP", #12
  "MISCED",
  "FISCED",
  "HISCED",
  "ICTRES", #13
  "HOMEPOS",
  "ESCS",
  "FCFMLRTY", #14
  "FLSCHOOL",
  "FLMULTSB",
  "FLFAMILY", #15
  "ACCESSFP",
  "FLCONFIN", #16
  "FLCONICT",
  "ACCESSFA", #17
  "ATTCONFM",
  "FRINFLFM", #18
  "W_FSTUWT", #19
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
  "REGION",
  "OECD", #1
  "Q.Language",
  "Own.Room",
  "Home.Computer",
  "Home.Cars",
  "Home.Devices", #2
  "FL_Option",
  "ICT_Option",
  "Well_Being_Option",
  "Parent_Option", #3
  "Teacher_Option",
  "Grade",
  "Birth.Month",
  "Birth.Year", #4
  "Gender",
  "Books.Home",
  "Siblings",
  "Mother.Schooling", #5
  "Father.Schooling",
  "Student.Birth.C",
  "Mother.Birth.C", #6
  "Father.Birth.C",
  "Arrival.Age",
  "Home.Lang.Test_Oth",
  "Inc.Level",
  "Inc.Expect", #7
  "Sch.Close.Covid",
  "Sch.Close.Other",
  "School.Type", #8
  "Student.Age",
  "Immigrant",
  "stu.birth.country", #9
  "mother.birth.country",
  "father.birth.country", #10
  "Home.Language",
  "Grade.Repeat",
  "Sch.Missing.3mo", #11
  "SISCO",
  "Info.Fut.Career.WLE",
  "Fam.Support.WLE", #12
  "Mother.Ed",
  "Father.Ed",
  "Parents.Ed",
  "ICT.Res.WLE", #13
  "Home.Poss.WLE",
  "ESCS",
  "Familiar.Fin.Concept", #14
  "FinEd.Sch.WLE",
  "FinEd.Sch.Multiple.WLE", #15
  "Parent.Invol.WLE",
  "Access.to.Money.WLE",
  "Conf.FinMat.WLE", #16
  "Conf.FinMat.Digital.WLE",
  "Access.to.FinAct.WLE", #17
  "Att.Conf.FinMat.WLE",
  "Friend.Inf.FinMat.WLE", #18
  "W_FSTUWT", #19
  paste0("PV", 1:10, "MATH"),
  paste0("PV", 1:10, "READ"),
  paste0("PV", 1:10, "FLIT"),
  paste0("W_FSTURWT", 1:80),
  "SENWT"
)

###############################################################################
# 4. APPLY SELECTION + RENAME
###############################################################################

dataQQQ <- full_qqq |> select(any_of(qqq_keep))
names(dataQQQ) <- new_labels
unique(dataQQQ$Country)
dataQQQ <- dataQQQ |> filter(Country != "CRI")

# Compute PV averages
dataQQQ <- dataQQQ %>%
  mutate(
    PV_FLIT_Ave = rowMeans(
      across(
        starts_with("PV") &
          ends_with("FLIT")
      ),
      na.rm = TRUE
    )
  )
names(dataQQQ)
dim(dataQQQ)

######## Utility functions ############
vars_bg <- c(
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


###############

data_us_small_numeric <- dataQQQ |>
  filter(Country == "USA") |>
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
    "CNTSTUID"
  )

data_us_small <- data_us_small_numeric |>
  convert_bg_vars_factor()

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
n_imp = 5
weighted_pmm <- mice(
  data_us_small_numeric |>
    select(-SchoolID, -CNTSTUID, starts_with("W_FSTURWT")),
  maxit = 20,
  method = "weighted.pmm",
  m = n_imp,
  imputationWeights = data_us_small_numeric$W_FSTUWT
)
plot(weighted_pmm)
densityplot(weighted_pmm)
# Distribution for Gender is off, but it's almost never missing so I'm not too concerned about that.
# Siblings is not good either, but I'm not sure how much can be done about that because a lot of the
# people missing it are also missing all or most of the other questionnaire variables, so the only information
# we have about them are their plausible values

# I tried running it with the default imputation as well (so logreg for binary/polyreg for multi-category)
# But it was a lot slower, and I'm not sure it was better?
default_imp <- mice(data_us_small |> select(-SchoolID, -CNTSTUID), maxit = 20)
plot(default_imp)
densityplot(default_imp)

tall_us <- complete(weighted_pmm, action = "stacked") |>
  #cbind(data_us_small_numeric |> select(starts_with("W_FSTURWT"))) |>
  pivot_longer(PV1FLIT:PV10FLIT, values_to = "plausible_FLIT") |>
  mutate(
    imp = unlist(lapply(1:n_imp, function(x) {
      rep(10 * (x - 1) + (1:10), nrow(data_us_small_numeric))
    }))
  ) |>
  convert_bg_vars_factor()

tall_us <- split(tall_us, tall_us$imp)
fit_tall_us <- lapply(tall_us, function(x) {
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
      #   repweights = "W_FSTURWT[0-9]+",
      #  type = "BRR",
      data = x,
      #  combined.weights = TRUE
    )
  )
})

model <- pool(fit_tall_us)
attempt1 <- list(
  imputations = weighted_pmm,
  fitted = fit_tall_us,
  pooled = model
)
save(attempt1, file = "models/no_repwt/attempt1.rda")
summary(model)

# Attempt 2: Following Huang & Keller (2025), create a separate set of imputations for each PV, then pool everything
# Also results in 10*n_imp models to pool, but each PV gets its own set of imputed values
# Not sure if it's kosher to use the other PVs in the imputation model if we do things this way (Huang & Keller don't)
impute_data <- function(x, n_imp, n_iter) {
  futuremice(
    x |> select(-any_of(c("SchoolID", "CNTSTUID", "PV"))),
    maxit = n_iter,
    method = "weighted.pmm",
    m = n_imp,
    imputationWeights = x$W_FSTUWT,
    parallelseed = 1701,
    packages = c('miceadds', 'dplyr')
  )
}

impute_country_with_each_pv <- function(
  country_data_numeric,
  n_imp,
  n_iter = 40
) {
  data_by_pv <- country_data_numeric |>
    #cbind(country_data_numeric |> select(starts_with("W_FSTURWT"))) |>
    pivot_longer(
      PV1FLIT:PV10FLIT,
      values_to = "plausible_FLIT",
      names_to = "PV"
    ) |>
    split(~PV)

  print("Imputing...")
  weighted_pmm_by_pv <- lapply(data_by_pv, impute_data, n_imp, n_iter)
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
  complete_by_pv <- do.call(
    rbind,
    lapply(weighted_pmm_by_pv, function(x) complete(x, action = "stacked"))
  ) |>
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
    imputations = weighted_pmm_by_pv,
    fitted = fit_by_pv,
    pooled = model2
  )
  results
}
model_us <- impute_country_with_each_pv(data_us_small_numeric, n_imp = n_imp)
summary(model_us$pooled)
save(model_us, file = "models/no_repwt/model_us.rda")

data_bra_small_numeric <- dataQQQ |>
  filter(Country == "BRA") |>
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
    "W_FSTUWT",
    "SchoolID",
    "CNTSTUID"
  )
model_bra <- impute_country_with_each_pv(
  data_bra_small_numeric,
  n_imp = n_imp,
  n_iter = 40
)
save(model_bra, file = "models/no_repwt/model_bra.rda")

combined_indiv_imputations <- rbind(
  cbind(
    do.call(
      rbind,
      lapply(model_us$imputations, function(x) complete(x, action = "long"))
    ) |>
      convert_bg_vars_factor() |>
      mutate(
        `.imp` = unlist(lapply(1:(n_imp * 10), function(x) {
          rep(x, nrow(data_us_small_numeric))
        }))
      ),
    data.frame(Country = "USA")
  ),
  cbind(
    do.call(
      rbind,
      lapply(model_bra$imputations, function(x) complete(x, action = "long"))
    ) |>
      convert_bg_vars_factor() |>
      mutate(
        `.imp` = unlist(lapply(1:(n_imp * 10), function(x) {
          rep(x, nrow(data_bra_small_numeric))
        }))
      ),
    data.frame(Country = "BRA")
  )
)

combined_indiv_imputations <- split(
  combined_indiv_imputations,
  combined_indiv_imputations$.imp
)
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
      ids = ~1,
      weights = ~W_FSTUWT,
      #   repweights = "W_FSTURWT[0-9]+",
      #   type = "BRR",
      data = x,
      #   combined.weights = TRUE
    )
  )
})
pooled_combined_from_indiv <- pool(fit_combined_from_indiv)
model_combined_from_indiv <- list(
  imputations = combined_indiv_imputations,
  fitted = fit_combined_from_indiv,
  pooled = pooled_combined_from_indiv
)
save(
  model_combined_from_indiv,
  file = "models/no_repwt/combined_from_indiv.rda"
)

############## Impute together, fit models separately #################
data_combined_by_pv <- rbind(
  data_us_small_numeric |> mutate(Country = "USA"),
  data_bra_small_numeric |> mutate(Country = "BRA")
) |>
  pivot_longer(
    PV1FLIT:PV10FLIT,
    values_to = "plausible_FLIT",
    names_to = "PV"
  ) |>
  split(~PV)

weighted_pmm_combined_data <- lapply(
  data_combined_by_pv,
  impute_data,
  n_imp,
  40
)

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

complete_combined_by_pv <- do.call(
  rbind,
  lapply(weighted_pmm_combined_data, function(x) {
    complete(x, action = "stacked")
  })
) |>
  convert_bg_vars_factor()
complete_combined_by_pv$`.imp` <- unlist(lapply(1:(n_imp * 10), function(x) {
  rep(x, nrow(rbind(data_us_small_numeric, data_bra_small_numeric)))
}))

complete_us_from_combined <- complete_combined_by_pv |>
  filter(Country == "USA")
complete_us_from_combined <- split(
  complete_us_from_combined,
  complete_us_from_combined$`.imp`
)
complete_bra_from_combined <- complete_combined_by_pv |>
  filter(Country == "BRA")
complete_bra_from_combined <- split(
  complete_bra_from_combined,
  complete_bra_from_combined$`.imp`
)

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
      ids = ~1,
      weights = ~W_FSTUWT,
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
      ids = ~1,
      weights = ~W_FSTUWT,
      #  repweights = "W_FSTURWT[0-9]+",
      #  type = "BRR",
      data = x,
      #  combined.weights = TRUE
    )
  )
})
fit_combined_by_pv <- lapply(
  complete_combined_by_pv |> split(complete_combined_by_pv$.imp),
  function(x) {
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
        # repweights = "W_FSTURWT[0-9]+",
        # type = "BRR",
        data = x,
        #  combined.weights = TRUE
      )
    )
  }
)
model_us_from_combined <- pool(fit_us_by_pv)
model_bra_from_combined <- pool(fit_bra_by_pv)
model_combined <- pool(fit_combined_by_pv)

combined_us <- list(
  imputations = weighted_pmm_combined_data,
  fitted = fit_us_by_pv,
  pooled = model_us_from_combined
)
combined_bra <- list(
  imputations = weighted_pmm_combined_data,
  fitted = fit_bra_by_pv,
  pooled = model_bra_from_combined
)
combined_us_bra <- list(
  imputations = weighted_pmm_combined_data,
  fitted = fit_combined_by_pv,
  pooled = model_combined
)
save(
  combined_us,
  combined_bra,
  combined_us_bra,
  file = "models/no_repwt/model_us_bra.rda"
)

############# Try imputing with all PVs and fitting to one
impute_country_with_all_pvs <- function(
  country_data_numeric,
  n_imp,
  n_iter = 40
) {
  # data_by_pv <- country_data_numeric |>
  #   #cbind(country_data_numeric |> select(starts_with("W_FSTURWT"))) |>
  #   pivot_longer(
  #     PV1FLIT:PV10FLIT,
  #     values_to = "plausible_FLIT",
  #     names_to = "PV"
  #   ) |>
  #   split(~PV)

  print("Imputing...")
  weighted_pmm_all_pvs <- impute_data(
    country_data_numeric,
    10 * n_imp,
    n_iter
  )
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
  PVs <- complete_by_pv |> select(starts_with("PV") & ends_with("FLIT"))
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
    imputations = weighted_pmm_all_pvs,
    fitted = fit_by_pv,
    pooled = model2
  )
  results
}

model_us_allPV <- impute_country_with_all_pvs(data_us_small_numeric, n_imp)
save(model_us_allPV, file = "models/no_repwt/model_us_allPV.rda")

######## Sample from each country
data_sample_small <- slice_sample(dataQQQ, by = "Country", prop = 0.1) |>
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

allcountry_sample_by_pv <- data_sample_small |>
  pivot_longer(
    PV1FLIT:PV10FLIT,
    values_to = "plausible_FLIT",
    names_to = "PV"
  ) |>
  split(~PV)

print("Imputing based on sample of all countries...")
allcountry_pmm_by_pv <- lapply(
  allcountry_sample_by_pv,
  impute_data,
  n_imp,
  n_iter
)
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

country_fromsample_by_pv <- function(
  all_country_imputed_by_pv,
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

  mids_country <- lapply(1:10, function(i) {
    mice.mids(
      all_country_imputed_by_pv[[i]],
      newdata = specific_by_pv[[i]],
      maxit = 1
    )
  })
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

model_us_allsample <- country_fromsample_by_pv(
  allcountry_pmm_by_pv,
  data_us_small_numeric
)
save(model_us_allsample, file = "models/no_repwt/model_us_allsample.rda")

model_bra_allsample <- country_fromsample_by_pv(
  allcountry_pmm_by_pv,
  data_bra_small_numeric
)
save(model_bra_allsample, file = "models/no_repwt/model_bra_allsample.rda")

complete_allsample_by_pv <- do.call(
  rbind,
  lapply(allcountry_pmm_by_pv, function(x) {
    complete(x, action = "stacked")
  })
) |>
  convert_bg_vars_factor()
complete_allsample_by_pv$`.imp` <- unlist(lapply(1:(n_imp * 10), function(x) {
  rep(x, nrow(data_sample_small))
}))
complete_allsample_by_pv <- split(
  complete_allsample_by_pv,
  complete_allsample_by_pv$.imp
)

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
      ids = ~1,
      weights = ~W_FSTUWT,
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

######## At Horacio's request, try imputing based on a single PV from each subject
impute_country_with_each_pv <- function(
  country_data_numeric,
  n_imp,
  n_iter = 40
) {
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
    split(~PV)

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
  complete_by_pv <- do.call(
    rbind,
    lapply(weighted_pmm_by_pv, function(x) complete(x, action = "stacked"))
  ) |>
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
    imputations = weighted_pmm_by_pv,
    fitted = fit_by_pv,
    pooled = model2
  )
  results
}

model_us_1 <- impute_country_with_each_pv(data_us_small_numeric, n_imp = n_imp)
summary(model_us$pooled)
save(model_us_1, file = "models/no_repwt/model_us_1.rda")

### No imputations
use_pvs_noimpute <- function(country_data) {
  tall_noimpute <- country_data |>
    pivot_longer(
      starts_with("PV") & ends_with("FLIT"),
      values_to = "plausible_FLIT"
    ) |>
    convert_bg_vars_factor() |>
    select(c(
      "Gender",
      "Books.Home",
      "Home.Cars",
      "Home.Computer",
      "Siblings",
      "Immigrant",
      "Father.Ed",
      "Familiar.Fin.Concept",
      "Home.Devices",
      "Grade.Repeat",
      "W_FSTUWT",
      "name",
      "plausible_FLIT"
    )) |>
    na.omit()
  tall_noimpute <- split(tall_noimpute, tall_noimpute$name)

  fit_noimpute <- lapply(tall_noimpute, function(x) {
    ok_vars <- lapply(
      c(
        "Gender",
        "Books.Home",
        "Home.Cars",
        "Home.Computer",
        "Siblings",
        "Immigrant",
        "Father.Ed",
        "Familiar.Fin.Concept",
        "Home.Devices",
        "Grade.Repeat"
      ),
      function(y) ifelse(n_distinct(x[, y]) > 1, y, NA)
    ) |>
      unlist() |>
      na.omit()
    svyglm(
      as.formula(paste0("plausible_FLIT~", paste0(ok_vars, collapse = "+"))),
      # plausible_FLIT ~
      #   Gender +
      #     Books.Home +
      #     Home.Cars +
      #     Home.Computer +
      #     Siblings +
      #     Immigrant +
      #     Father.Ed +
      #     Familiar.Fin.Concept +
      #     Home.Devices +
      #     Grade.Repeat,
      design = svydesign(
        ids = ~1,
        weights = ~W_FSTUWT,
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
us_noimpute <- use_pvs_noimpute(data_us_small_numeric)
save(us_noimpute, file = "models/no_repwt/noimpute/us_noimpute.rda")
bra_noimpute <- use_pvs_noimpute(data_bra_small_numeric)
save(bra_noimpute, file = "models/no_repwt/noimpute/bra_noimpute.rda")
us_bra_noimpute <- use_pvs_noimpute(rbind(
  data_us_small_numeric,
  data_bra_small_numeric
))
save(us_bra_noimpute, file = "models/no_repwt/noimpute/us_bra_noimpute.rda")

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
    ids = ~1,
    weights = ~W_FSTUWT,
    #  repweights = "W_FSTURWT[0-9]+",
    #  type = "BRR",
    #  combined.weights = TRUE,
    data = data_us_small_numeric |>
      convert_bg_vars_factor() |>
      mutate(
        FLIT_Ave = rowMeans(
          across(starts_with("PV") & ends_with("FLIT")),
          na.rm = TRUE
        )
      )
  )
)
save(model_us_ignorePV, file = "models/no_repwt/us_ignorePV.rda")
