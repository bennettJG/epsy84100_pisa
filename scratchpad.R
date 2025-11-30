# Just a dump of code that I can't get to work
# Keeping it in a file just so I don't forget what I tried

tall_us <- split(tall_us, tall_us$imp)
library(intsvy)
fit_tall_us <- lapply(tall_us, function(x){
  print("Fitting...")
  pisa.reg(paste0("plausible_FLIT"), x=vars_bg, data=x)
  # svyglm(plausible_FLIT~Gender+Books.Home+Home.Cars+Home.Computer+Siblings+
  #          Immigrant+Father.Ed+Familiar.Fin.Concept+Home.Devices+Grade.Repeat, 
  #        design=svydesign(ids = ~ 1, weights = ~W_FSTUWT, data = x))
})

fit_tall_us_models <- lapply(fit_tall_us, function(x){
  x$reg |> 
    rownames_to_column() |> 
    rename(term = rowname, estimate = Estimate, `std.error` = "Std. Error") |>
    filter(term != "R-squared")
})

barnard.rubin <- function(m, b, t, dfcom = Inf) {
  lambda <- (1 + 1 / m) * b / t
  lambda[lambda < 1e-04] <- 1e-04
  dfold <- (m - 1) / lambda^2
  dfobs <- (dfcom + 1) / (dfcom + 3) * dfcom * (1 - lambda)
  ifelse(is.infinite(dfcom), dfold, dfold * dfobs / (dfold + dfobs))
}

pool_custom <- function(w, dfcom=NULL, custom.t=NULL) {
  pooled <- w %>%
    group_by(term) %>%
    summarize(
      m = n(),
      qbar = mean(.data$estimate),
      ubar = mean(.data$std.error^2),
      b = var(.data$estimate),
      t = ifelse(is.null(custom.t),
                 .data$ubar + (1 + 1 / .data$m) * .data$b,
                 eval(parse(text = custom.t))),
      dfcom = dfcom,
      df = barnard.rubin(.data$m, .data$b, .data$t, .data$dfcom),
      riv = (1 + 1 / .data$m) * .data$b / .data$ubar,
      lambda = (1 + 1 / .data$m) * .data$b / .data$t,
      fmi = (.data$riv + 2 / (.data$df + 3)) / (.data$riv + 1)
    )
  pooled
}
model <- data.frame(lapply(getfit(fit_tall_us_models), function(x)pool_custom(x, 3206-25)))
summary(model)