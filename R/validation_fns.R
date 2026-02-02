# Functions to help with model validation


compute_crps <- function(data) {
  
  # input data has two variabls of interest: mean and flux
  
  .x <- data$mean
  .y <- data$flux
  
  s <- 0
  for (i in seq_along(.x)) {
    for (j in seq_along(.x)) {
      s <- s + abs(.x[i] - .x[j])
    }
  }
  mean(abs(.x - .y)) - s / (2 * length(.x)^2)
}

compute_taylor <- function(data) {
  
  # input variable data needs variables of interest: q0.5 and flux
  
  data |>
    drop_na() |>
    summarize(sd_meas=1,
              sd=sd(q0.5)/sd(flux),
              r=cor(flux,q0.5),
              centered_rms=sd((flux-mean(flux))-((q0.5-mean(q0.5))))/sd(flux)
    ) |>
    mutate(x_coord = sd*r, y_coord = sd*sin(acos(r)))
  
  
}

compute_reliability <- function(data) {
  
  # input variable data needs variables of interest: q0.025, q0.075, and flux
  
  data |> 
    dplyr::mutate(within_PI = dplyr::between(flux, q0.025, q0.975)) |> 
    dplyr::summarise(within_TRUE = mean(within_PI,na.rm=TRUE),
                     within_FALSE = mean(!within_PI,na.rm=TRUE),
                     
                     .groups = 'drop') 
  
}

compute_model_stats <- function(data) {
  
  # input variable data needs variables of interest: q0.5 and flux
  
  data <- data |> drop_na()
  if(nrow(data)>2) {
    
    lm_model<- lm(q0.5~flux,data = data)
    
    out <- broom::glance(lm_model) |>
      mutate(slope = lm_model$coefficients[[2]])
    
  } else {
    
    out <- tibble(r.squared = NA,
                  adj.r.squared = NA,
                  sigma=NA,
                  statistic=NA,
                  p.value = NA,
                  df = NA,
                  logLik = NA,
                  AIC = NA,
                  BIC = NA,
                  deviance = NA,
                  df.residual =NA,
                  nobs = NA,
                  slope = NA)
  }
  
  return(out)
  
}
