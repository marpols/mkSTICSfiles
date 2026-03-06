convert_sm <- function(value, bd){
  return(value * bd)
}

convert_mgKg <- function(value, depth, bd){
  return((value * depth * bd)/10)
}

calc_som <- function(soc, van_bemmelen = 1.724){
  return(soc * van_bemmelen)
}

calc_orgN <- function(soc, cn_ratio, inorganic_N){
  return((soc/cn_ratio) - inorganic_N)
}

calc_fc <- function(sand, clay, som){
  
  theta_33t <- calc_theta33(sand, clay, som)
  
  return((theta_33t + (1.283 * theta_33t^2 - 0.374 * theta_33t - 0.15))*100)
}

calc_wp <- function(sand, clay, som){
  
  theta_1500t <- calc_theta1500(sand, clay, som)
  
  return((theta_1500t + (0.14 * theta_1500t - 0.02))*100) 
}

calc_bd <- function(sand, clay, som){
  
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
  
  if (clay > 50){
    return(5 + 0.06 * (100 - clay))
  } else if (sand > 80){
    return(5 + 0.15 * (100 - sand))
  } else{
    return(8 + 0.08 * clay)
  }
}
