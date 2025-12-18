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
  us_noimpute$pooled,
  model_us_ignorePV,
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

for (c in unlist(data_sample_small_c |> distinct(Country))) {
  print(c)
  country_alone <- readRDS(paste0("models/fromsamp/", c, "_small_alone.rds"))
  country_fromsample <- readRDS(paste0(
    "models/fromsamp/",
    c,
    "_small_allsample.rds"
  ))
  country_noimpute <- readRDS(paste0(
    "models/fromsamp/",
    c,
    "_noimpute.rds"
  ))
  png(
    paste0("figures/from_sample/", c, "_vs_allcountry.png"),
    width = 5.5,
    height = 8,
    units = "in",
    res = 300
  )
  p <- plot_coefs(
    country_fromsample$pooled,
    country_alone$pooled,
    country_noimpute$pooled,
    model.names = c(
      "Sample of all countries used for imputation model",
      paste(c, "imputed alone"),
      "Listwise deletion"
    )
  ) +
    guides(color = guide_legend(nrow = 2, byrow = T)) +
    theme(
      legend.position = "bottom",
      legend.location = "plot",
      legend.key.spacing.y = unit(0, "pt"),
      legend.text = element_text(lineheight = 0.5, margin = margin(0))
    )
  print(p)
  dev.off()
}

missing_pointestimates <- data.frame()

for (c in unlist(data_sample_small_c |> distinct(Country))) {
  print(c)
  country_alone <- readRDS(paste0("models/fromsamp/", c, "_small_alone.rds"))
  country_fromsample <- readRDS(paste0(
    "models/fromsamp/",
    c,
    "_small_allsample.rds"
  ))
  country_noimpute <- readRDS(paste0(
    "models/fromsamp/",
    c,
    "_noimpute.rds"
  ))
  est_alone <- country_alone$pooled |>
    broom::tidy() |>
    select(all_of(c("term", "estimate", "std.error"))) |>
    rename(est_alone = estimate, se_imputed = std.error)
  est_fromsample <- country_fromsample$pooled |>
    broom::tidy() |>
    select(all_of(c("term", "estimate", "std.error"))) |>
    rename(est_fromsample = estimate, se_fromsample = std.error)
  est_noimpute <- country_noimpute$pooled |>
    broom::tidy() |>
    select(all_of(c("term", "estimate", "std.error"))) |>
    rename(est_noimpute = estimate)
  country_pctmissing <- colSums(is.na(
    data_sample_small_c |>
      filter(Country == c) |>
      select(vars_bg)
  )) /
    nrow(
      data_sample_small_c |>
        filter(Country == c)
    )
  sample_pctmissing <- colSums(is.na(data_sample_small_c |> select(vars_bg))) /
    nrow(data_sample_small_c)

  summ <- est_alone |>
    left_join(est_fromsample, by = "term") |>
    left_join(est_noimpute, by = "term") |>
    mutate(
      var = case_when(
        str_starts(term, "Gender") ~ "Gender",
        str_starts(term, "Books") ~ "Books.Home",
        str_starts(term, "Home.Cars") ~ "Home.Cars",
        str_starts(term, "Home.Computer") ~ "Home.Computer",
        str_starts(term, "Siblings") ~ "Siblings",
        str_starts(term, "Immigrant") ~ "Immigrant",
        str_starts(term, "Home.Devices") ~ "Home.Devices",
        str_starts(term, "Grade") ~ "Grade.Repeat",
        term == "(Intercept)" ~ NA,
        .default = term
      ),
      Country = c
    )
  missingness <- data.frame(
    n_listwise = nrow(
      data_sample_small_c |>
        filter(Country == c) |>
        select(all_of(vars_bg)) |>
        na.omit()
    ),
    n_total = nrow(
      data_sample_small_c |>
        filter(Country == c)
    ),
    country_pctmissing = country_pctmissing,
    sample_pctmissing = sample_pctmissing
  ) |>
    rownames_to_column(var = "var") |>
    left_join(summ, by = "var") |>
    mutate(
      sd = sqrt(n_listwise) *
        std.error,
      sd_imputed = sqrt(n_total) * se_imputed
    )
  if (nrow(missing_pointestimates) == 0) {
    missing_pointestimates <- missingness
  } else {
    missing_pointestimates <- bind_rows(missing_pointestimates, missingness)
  }
}

ggplot(
  missing_pointestimates,
  aes(
    y = country_pctmissing,
    x = abs(est_alone - est_noimpute) / sd_imputed,
    color = var,
    shape = var
  )
) +
  geom_point() +
  scale_x_continuous(
    "Difference between imputed and\nnon-imputed coefficients\n(SD of imputed value)"
  ) +
  scale_y_continuous("% missingness on variable", labels = scales::percent) +
  #scale_color_brewer(palette = "Dark2") +
  scale_shape_manual(values = c(0:2, 5:6, 0:2, 5:6)) +
  theme_bw() +
  geom_smooth(
    method = lm,
    aes(shape = NULL, color = NULL),
    linewidth = 0.5,
    se = F
  ) +
  theme(legend.position = "bottom", legend.title = element_blank())

ggplot(
  missing_pointestimates,
  aes(
    y = country_pctmissing,
    x = abs(est_alone - est_fromsample) / sd_imputed,
    color = var,
    shape = var
  )
) +
  geom_point() +
  scale_x_continuous(
    "Difference between imputation strategies\n(SD of value under country-specific model)"
  ) +
  scale_y_continuous("% missingness on variable", labels = scales::percent) +
  #scale_color_brewer(palette = "Dark2") +
  scale_shape_manual(values = c(0:2, 5:6, 0:2, 5:6)) +
  geom_smooth(
    method = lm,
    aes(shape = NULL, color = NULL),
    linewidth = 0.5,
    se = F
  ) +
  theme_bw() +
  theme(legend.position = "bottom", legend.title = element_blank())
