rm(list = ls())

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
  #paste0("W_FSTURWT", 1:80),
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
  #paste0("W_FSTURWT", 1:80),
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
        labels = c("None", "One", "Two", "3 or more")
      ),
      `Home.Computer` = factor(2 - `Home.Computer`, labels = c("No", "Yes")),
      Siblings = factor(
        Siblings,
        labels = c("None", "One", "Two", "3 or more")
      ),
      Immigrant = factor(
        Immigrant,
        labels = c("Non-immigrant", "Second-gen", "First-gen")
      ),
      `Grade.Repeat` = factor(
        `Grade.Repeat`,
        levels = c(0, 1),
        labels = c("No", "Yes")
      ),
      `Home.Devices` = factor(
        `Home.Devices`,
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
    starts_with("W_FSTU"),
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
  data_us_small_numeric |> select(-SchoolID, -CNTSTUID),
  maxit = 40,
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
default_imp <- mice(data_us_small |> select(-SchoolID, -CNTSTUID), maxit = 40)
plot(default_imp)
densityplot(default_imp)

tall_us <- complete(weighted_pmm, action = "stacked") |>
  #cbind(data_us_small_numeric |> select(starts_with("W_FSTURWT"))) |>
  pivot_longer(PV1FLIT:PV10FLIT, values_to = "plausible_FLIT") |>
  mutate(
    imp = unlist(lapply(1:(10 * n_imp), function(x) {
      rep(x, nrow(data_us_small_numeric))
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
    design = svydesign(ids = ~1, weights = ~W_FSTUWT, data = x)
  )
})

model <- pool(fit_tall_us)
attempt1 <- list(
  imputation_sample = weighted_pmm[[1]],
  fitted_sample = fit_tall_us[1:5],
  pooled = model
)
save(attempt1, file = "models/attempt1.rda")
summary(model)

# Attempt 2: Following Huang & Keller (2025), create a separate set of imputations for each PV, then pool everything
# Also results in 10*n_imp models to pool, but each PV gets its own set of imputed values
# Not sure if it's kosher to use the other PVs in the imputation model if we do things this way (Huang & Keller don't)
impute_data <- function(x, n_imp, n_iter) {
  futuremice(
    x |> select(-SchoolID, -CNTSTUID, -starts_with("W_FSTURWT"), -PV),
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
    pivot_longer(
      PV1FLIT:PV10FLIT,
      values_to = "plausible_FLIT",
      names_to = "PV"
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
      design = svydesign(ids = ~1, weights = ~W_FSTUWT, data = x)
    )
  })
  print("Pooling...")
  model2 <- pool(fit_by_pv)
  results <- list(
    imputation_sample = weighted_pmm_by_pv[[1]],
    fitted_sample = fit_by_pv[1:5],
    pooled = model2
  )
  results
}
model_us <- impute_country_with_each_pv(data_us_small_numeric, n_imp = n_imp)
summary(model_us$pooled)
save(model_us, file = "models/model_us.rda")

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
    starts_with("W_FSTU"),
    "SchoolID",
    "CNTSTUID"
  )
model_bra <- impute_country_with_each_pv(
  data_bra_small_numeric,
  n_imp = n_imp,
  n_iter = 40
)
save(model_bra, file = "models/model_bra.rda")

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
    design = svydesign(ids = ~1, weights = ~W_FSTUWT, data = x)
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
    design = svydesign(ids = ~1, weights = ~W_FSTUWT, data = x)
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
        strata = ~Country,
        weights = ~W_FSTUWT,
        data = x
      )
    )
  }
)
model_us_from_combined <- pool(fit_us_by_pv)
model_bra_from_combined <- pool(fit_bra_by_pv)
model_combined <- pool(fit_combined_by_pv)

combined_us <- list(
  imputation_sample = weighted_pmm_combined_data[[1]],
  fitted_sample = fit_us_by_pv[1:5],
  pooled = model_us_from_combined
)
combined_bra <- list(
  imputation_sample = weighted_pmm_combined_data[[1]],
  fitted_sample = fit_bra_by_pv[1:5],
  pooled = model_bra_from_combined
)
combined_us_bra <- list(
  imputation_sample = weighted_pmm_combined_data[[1]],
  fitted_sample = fit_combined_by_pv[1:5],
  pooled = model_combined
)
save(
  combined_us,
  combined_bra,
  combined_us_bra,
  file = "models/model_us_bra.rda"
)

####### Remove responses with no questionnaire answers whatsoever before fitting
us_with_qqq <- data_us_small_numeric |>
  mutate(
    missing_qs = rowSums(
      data_us_small_numeric |> select(any_of(vars_bg)) |> is.na()
    )
  ) |>
  filter(missing_qs < 9) |>
  select(-missing_qs)

model_us_2 <- impute_country_with_each_pv(us_with_qqq, n_imp = n_imp)
save(model_us_2, file = "models/model_us_qqq.rda")

bra_with_qqq <- data_bra_small_numeric |>
  mutate(
    missing_qs = rowSums(
      data_bra_small_numeric |> select(any_of(vars_bg)) |> is.na()
    )
  ) |>
  filter(missing_qs < 9) |>
  select(-missing_qs)

n_iter = 40
model_bra_2 <- impute_country_with_each_pv(
  bra_with_qqq,
  n_imp = n_imp,
  n_iter = n_iter
)
save(model_bra_2, file = "models/model_bra_qqq.rda")

### combined without no-questionnaire responses
data_combined_with_qqq <- rbind(
  us_with_qqq |> mutate(Country = "USA"),
  bra_with_qqq |> mutate(Country = "BRA")
) |>
  pivot_longer(
    PV1FLIT:PV10FLIT,
    values_to = "plausible_FLIT",
    names_to = "PV"
  ) |>
  split(~PV)

weighted_pmm_combined_qqq <- lapply(
  data_combined_with_qqq,
  impute_data,
  n_imp
)

plot(weighted_pmm_combined_qqq[[1]])
densityplot(
  weighted_pmm_combined_qqq[[1]],
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

complete_combined_qqq <- do.call(
  rbind,
  lapply(weighted_pmm_combined_qqq, function(x) complete(x, action = "stacked"))
) |>
  convert_bg_vars_factor()
complete_combined_qqq$`.imp` <- unlist(lapply(1:(n_imp * 10), function(x) {
  rep(x, nrow(rbind(us_with_qqq, bra_with_qqq)))
}))

complete_us_from_combined_qqq <- complete_combined_qqq |>
  filter(Country == "USA")
complete_us_from_combined_qqq <- split(
  complete_us_from_combined_qqq,
  complete_us_from_combined_qqq$`.imp`
)
complete_bra_from_combined_qqq <- complete_combined_qqq |>
  filter(Country == "BRA")
complete_bra_from_combined_qqq <- split(
  complete_bra_from_combined_qqq,
  complete_bra_from_combined_qqq$`.imp`
)

fit_us_by_pv_qqq <- lapply(complete_us_from_combined_qqq, function(x) {
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
    design = svydesign(ids = ~1, weights = ~W_FSTUWT, data = x)
  )
})
fit_bra_by_pv_qqq <- lapply(complete_bra_from_combined_qqq, function(x) {
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
    design = svydesign(ids = ~1, weights = ~W_FSTUWT, data = x)
  )
})
fit_combined_by_pv_qqq <- lapply(
  complete_combined_qqq |> split(complete_combined_qqq$.imp),
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
        strata = ~Country,
        weights = ~W_FSTUWT,
        data = x
      )
    )
  }
)
model_us_from_combined_qqq <- pool(fit_us_by_pv_qqq)
model_bra_from_combined_qqq <- pool(fit_bra_by_pv_qqq)
model_combined_qqq <- pool(fit_combined_by_pv_qqq)

combined_us_qqq <- list(
  imputation_sample = weighted_pmm_combined_qqq[[1]],
  fitted_sample = fit_us_by_pv_qqq[1:5],
  pooled = model_us_from_combined
)
combined_bra_qqq <- list(
  imputation_sample = weighted_pmm_combined_qqq[[1]],
  fitted = fit_bra_by_pv_qqq[1:5],
  pooled = model_bra_from_combined
)
combined_us_bra_qqq <- list(
  imputation_sample = weighted_pmm_combined_qqq[[1]],
  fitted = fit_combined_by_pv_qqq[1:5],
  pooled = model_combined_qqq
)
save(
  combined_us_qqq,
  combined_bra_qqq,
  combined_us_bra_qqq,
  file = "models/model_us_bra_qqq.rda"
)
#####
# I believe using intsvy is the most correct way to analyze the data (using the replicate weights)
# but I cannot for the life of me figure out how to pool the results using mice
# I guess I might have to just Rubin's Rules it myself but I don't want to... T_T
# library(intsvy)
# us_complete <- complete(weighted_pmm, action="all")
# fit_intsvy <- lapply(us_complete, function(d) {
#   print("Fitting with intsvy....")
#   pisa.reg.pv(paste0("PV", 1:10, "FLIT"), x=vars_bg, data=d)
# })
# model <- pool(fit_intsvy)
# summary(model)

#### Code from Horacio for data exploration

mod1 <- lm(
  plausible_FLIT ~
    Gender +
      Books.Home +
      Home.Cars +
      Home.Computer +
      Siblings +
      Immigrant +
      Father.Ed +
      Familiar.Fin.Concept +
      Home.Devices,
  data = tall_us
)
summary(mod1)
svy <- svydesign(
  ids = ~1,
  weights = ~W_FSTUWT,
  data = data_us_small_numeric |>
    mutate(
      FLIT_Ave = rowMeans(
        across(starts_with("PV") & ends_with("FLIT")),
        na.rm = TRUE
      )
    )
)
svymod1 <- svyglm(
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
  data = data_us_small_numeric |>
    mutate(
      FLIT_Ave = rowMeans(
        across(starts_with("PV") & ends_with("FLIT")),
        na.rm = TRUE
      )
    ),
  design = svy
)
summary(svymod1)
mod2 <- lm(
  PV10FLIT ~
    Gender +
      Books.Home +
      Home.Cars +
      Home.Computer +
      Siblings +
      Immigrant +
      Father.Ed +
      Familiar.Fin.Concept +
      Grade.Repeat +
      Home.Devices,
  data = dataQQQ
)
summary(mod2)

library(lm.beta)
summary(lm.beta::lm.beta(mod1))


###############################################################################
# 6. MISSINGNESS BY COUNTRY
###############################################################################

missing_by_country <- dataQQQ %>%
  select(Country, all_of(vars_bg)) %>%
  group_by(Country) %>%
  summarise(across(
    all_of(vars_bg),
    ~ mean(is.na(.)) * 100,
    .names = "pct_missing_{col}"
  )) %>%
  arrange(Country)

###############################################################################
# 7. TOTAL (VARIABLE-LEVEL MISSINGNESS ACROSS ALL COUNTRIES)
###############################################################################

total_missing <- dataQQQ %>%
  summarise(across(
    all_of(vars_bg),
    ~ mean(is.na(.)) * 100,
    .names = "pct_missing_{col}"
  )) %>%
  mutate(Country = "TOTAL") %>%
  select(Country, everything())

###############################################################################
# 8. GRAND TOTAL (ONE VALUE ACROSS ALL VARIABLES)
###############################################################################

grand_total_value <- dataQQQ %>%
  select(all_of(vars_bg)) %>%
  summarise(grand = mean(is.na(.))) %>%
  pull(grand) *
  100

# Listwise deletion missingness across all variables
grand_total_listwise <- dataQQQ %>%
  select(all_of(vars_bg)) %>%
  mutate(row_missing = if_any(everything(), is.na)) %>%
  summarise(grand_total_missingness = mean(row_missing) * 100) %>%
  pull(grand_total_missingness)

grand_total_row <- tibble(
  Country = "GRAND_TOTAL",
  pct_missing_Books.Home = grand_total_listwise,
  pct_missing_Home.Cars = grand_total_listwise,
  pct_missing_Home.Computer = grand_total_listwise,
  pct_missing_Siblings = grand_total_listwise,
  pct_missing_Immigrant = grand_total_listwise,
  pct_missing_Father.Ed = grand_total_listwise,
  pct_missing_Grade.Repeat = grand_total_listwise,
  pct_missing_Home.Devices = grand_total_listwise,
  pct_missing_Familiar.Fin.Concept = grand_total_listwise
)


###############################################################################
# 9. FINAL TABLE
###############################################################################

missing_by_country_final <- bind_rows(
  missing_by_country,
  total_missing,
  grand_total_row
)


# Fix column names: add a line break after "pct_missing"
colnames_nice <- gsub(
  "pct_missing_",
  "pct\nmissing\n",
  names(missing_by_country_final)
)
names(missing_by_country_final) <- colnames_nice

# PDF output with smaller font and layout adjustments
pdf("Missingness_By_Country.pdf", width = 11, height = 8.5)

grid.table(
  missing_by_country_final,
  theme = ttheme_minimal(
    core = list(
      fg_params = list(cex = 0.55), # shrink table text
      padding = unit(c(2, 2), "mm")
    ),
    colhead = list(
      fg_params = list(fontface = "bold", cex = 0.6), # shrink header text
      padding = unit(c(3, 3), "mm")
    )
  )
)

dev.off()


print(missing_by_country_final)

###############################################################################
# 10. EXPORT TO PDF
###############################################################################

pdf("Missingness_By_Country.pdf", width = 11, height = 8.5)
grid.table(missing_by_country_final)
dev.off()
dev.cur()
if (names(dev.cur()) != "RStudioGD") {
  dev.off()
}

###############################################################################
# END
###############################################################################

summary(dataQQQ)

library(tidyverse)
library(pheatmap)
library(GGally)
library(fmsb)

names(missing_by_country)
missing_by_country <- missing_by_country %>% select(-pct_missing_Grade.Repeat)

pdf("Missingness_Plots.pdf", width = 8.5, height = 11)

###############################################################################
# 1 — Heatmap
###############################################################################
mat <- as.matrix(missing_by_country[, -1])
rownames(mat) <- missing_by_country$Country

pheatmap(
  mat,
  cluster_rows = TRUE,
  cluster_cols = TRUE,
  color = colorRampPalette(c("white", "red"))(50),
  main = "Missingness Heatmap"
)

###############################################################################
# 3 — Boxplots
###############################################################################
missing_by_country_long <- missing_by_country %>%
  pivot_longer(-Country, names_to = "Variable", values_to = "Missing")

print(
  ggplot(missing_by_country_long, aes(x = Variable, y = Missing)) +
    geom_boxplot(fill = "skyblue") +
    theme_bw() +
    theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
    labs(
      title = "Boxplots of Missingness by Variable",
      x = "Variable",
      y = "% Missing"
    )
)

###############################################################################
# 4 — Faceted Bar Charts
###############################################################################
print(
  ggplot(missing_by_country_long, aes(x = Country, y = Missing)) +
    geom_col(fill = "steelblue") +
    facet_wrap(~Variable, scales = "free_y") +
    theme_bw() +
    theme(axis.text.x = element_text(angle = 90, hjust = 1, size = 7)) +
    labs(
      title = "Missingness by Country for Each Variable",
      y = "% Missing",
      x = "Country"
    )
)

###############################################################################
# 5 — Hierarchical Clustering
###############################################################################
mat <- as.matrix(missing_by_country[, -1])
rownames(mat) <- missing_by_country$Country

dist_mat <- dist(mat)
hc <- hclust(dist_mat)

plot(hc, main = "Country Clustering by Missingness", xlab = "", sub = "")

###############################################################################
# 6 — PCA
###############################################################################

dev.off()
dev.cur()
if (names(dev.cur()) != "RStudioGD") {
  dev.off()
}
