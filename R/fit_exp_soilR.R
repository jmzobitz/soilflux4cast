#' fit_exp_soilR fits a Q10 relationship to a set of soil respiration data
#'
#' @param input_data data table of SOILT and flux data (joined)
#'
#' @returns data table of coefficients of kR and Q10


fit_exp_soilR <- function(input_data) {
  
  # R = kR * Q10^(T-10/10)
  #ln(R) = ln(kR) + [(T-10)/10]*Q10
  
  
  # Set up the Q10 relationship
  
  mod_data <- input_data |>
    mutate(ln_flux = log(flux),
           T10 = (SOILT-10)/10) |>
    filter(if_all(everything(), ~ !is.na(.x) & is.finite(.x)))
  
  #print(mod_data)
  if (nrow(mod_data) > 3) {
    
    coeffs <- lm(ln_flux~T10,data=mod_data) |>
      broom::tidy() |>
      select(term,estimate) |>
      rename(value = estimate) |>
      mutate(term = if_else(term =="T10","Q10","kR"),
             value = if_else(term == "kR",exp(value),value))
    
    
    
    
    
  } else{   # no fit, so no soil R produced
    
    coeffs <- tibble(term = c("kR","Q10"),
                     value = NA)
    
  }
  
  return(coeffs)
}
