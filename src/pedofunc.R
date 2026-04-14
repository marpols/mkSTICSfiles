calc_som <- function(soc, van_bemmelen = vbf){
  if(anyNA(soc)) stop(missing_value("SOC", "SOM"))
  
  return(soc * van_bemmelen)
}

calc_CNratio <- function(soc, SON, inorganic_N){
  if(anyNA(soc)) stop(missing_value("SOC", "CN ratio"))
  if(anyNA(SON)) stop(missing_value("SON", "CN ratio"), call. = FALSE)
  if(anyNA(inorganic_N)) stop(missing_value("Inorganic N", "CN ratio"),
                              call. = FALSE)

  return(soc/(SON+inorganic_N))
}

calc_orgN <- function(soc, cn_ratio, inorganic_N){
  if(anyNA(soc)) stop(missing_value("SOC", "SON"))
  if(anyNA(cn_ratio)) stop(missing_value("CN ratio", "SON"))
  if(anyNA(inorganic_N)) stop(missing_value("Inorganic N", "SON"))
  
  return((soc/cn_ratio) - inorganic_N)
}

calc_fc <- function(sand, clay, som){
  if(anyNA(sand)) stop(missing_value("Sand", "FC"))
  if(anyNA(clay)) stop(missing_value("Clay", "FC"))
  if(anyNA(som)) stop(sprintf("%s SOM can be calculated using SOC",
                              missing_value("SOM", "FC")))
  
  theta_33t <- calc_theta33(sand, clay, som)
  
  return((theta_33t + (1.283 * theta_33t^2 - 0.374 * theta_33t - 0.15))*100)
}

calc_wp <- function(sand, clay, som){
  if(anyNA(sand)) stop(missing_value("Sand", "WP"))
  if(anyNA(clay)) stop(missing_value("Clay", "WP"))
  if(anyNA(som)) stop(sprintf("%s SOM can be calculated using SOC",
                              missing_value("SOM", "WP")))
  
  theta_1500t <- calc_theta1500(sand, clay, som)
  
  return((theta_1500t + (0.14 * theta_1500t - 0.02))*100) 
}

calc_bd <- function(sand, clay, som){
  if(anyNA(sand)) stop(missing_value("Sand", "BD"))
  if(anyNA(clay)) stop(missing_value("Clay", "BD"))
  if(anyNA(som)) stop(sprintf("%s SOM can be calculated using SOC",
                              missing_value("SOM", "BD")))
  #porosity
  A <- 0.278 * (sand/100) + 
    0.034 * (clay/100) +
    0.022 * som -
    0.018 * (sand/100) * som -
    0.027 * (clay/100) * som - 
    0.584 * (sand/100) * (clay/100) + 0.078
  
  #adjusted porosity
  B <- A + (0.636 * A - 0.107)
  
  #saturation porosity + moisture
  spm <- B + calc_fc(sand, clay, som)/100
  
  #sand adjustment factor
  saf <- -0.097 * (sand/100) + 0.043
  
  #sand adjusted saturation
  sas <- spm + saf
  
  return((1 - sas) * 2.65)
  
}

calc_theta33 <- function(sand, clay, som){
    
  return(
    -0.251 * (sand / 100) +
      0.195 * (clay / 100) +
      0.011 * som -
      0.006 * (sand / 100) * som -
      0.027 * (clay / 100) * som +
      0.452 * (sand / 100) * (clay / 100) + 0.299
  )
}

calc_theta1500 <- function(sand, clay, som){
  return(
    -0.024 * (sand / 100) +
      0.478 * (clay / 100) +
      0.006 * som -
      0.005 * (sand / 100) * som -
      0.013 * (clay / 100) * som +
      0.068 * (sand / 100) * (clay / 100) + 0.031
  )
}

calc_q0 <- function(sand, clay){
  if(anyNA(sand)) stop(missing_value("Sand", "q0"))
  if(anyNA(clay)) stop(missing_value("Clay", "q0"))
  
  if (clay > 50){
    return(5 + 0.06 * (100 - clay))
  } else if (sand > 80){
    return(5 + 0.15 * (100 - sand))
  } else{
    return(8 + 0.08 * clay)
  }
}

set_vbf <- function(value = 1.724){
  
  vbf <<- value
  sprintf("Van Bemmelen factor set to %.3f", value)
  
}

calculate_vals <- function(calc, args = list()){
  do.call(calc,agrs)
}
