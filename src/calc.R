calc_soils <- function(df){

  #layer 1 only
  
  #check CN ratio
  cells <- value_is_empty(df, "CN_ratio")
  if(!(length(cells) == 0)){
    SOC <- df[cells,"SOC_1"]
    SON <- df[cells,"SON_1"]
    inorganic_N <- df[cells,"inorganic_N"]
    df[cells,"CN_ratio"] <- calc_CNratio(SOC, SON, inorganic_N)
  }
  
  #check orgN
  cells <- value_is_empty(df, "SON_1")
  if(!(length(cells) == 0)){
    SOC <- df[cells,"SOC_1"]
    cn_ratio <- df[cells,"CN_ratio"]
    inorganic_N <- df[cells,"inorganic_N"]
    df[cells,"SON_1"] <- calc_orgN(SOC, cn_ratio, inorganic_N)
  }
  
  #all layers
  values <- c("SOM","BD","FC","WP")
  
  for(n in seq(1,5,1)){
    
    SOC <- df[cells,sprintf("SOC_%d",n)]
    sand <- df[cells,sprintf("%%_sand_%d",n)]
    clay <- df[cells,sprintf("%%_clay_%d",n)]
    som <- df[cells,sprintf("SOM_%d",n)]
    
    for(v in values){
    
      column <- sprintf("%s_%d",v,n)
      cells <- value_is_empty(df, column)
  
      if(!(length(cells) == 0)){
        if(v == "SOM"){
          func <- calc_som
          args <- list(SOC)
        } else if(v == "BD"){
          func <- calc_bd
          args <- list(sand, clay, som)
        } else if (v == "FC"){
          func <- calc_fc
          args <- list(sand, clay, som)
        } else if (v == "WP"){
          func <- calc_wp
          args <- list(sand, clay, som)
        }
        df[cells,column] <- do.call(func, args)
        
        }
    }
  }
  l1 <- seq(7, 26, 1)
  l2 <- seq(27, 41, 1)
  l3 <- seq(42, 56, 1)
  l4 <- seq(57, 71, 1)
  l5 <- seq(72, 86, 1)
  
  #check fc, wp units
  cells_sm <- unit_check(df, "% V/V")
  
  #check initial NO3, NH4 units
  cells_N <- unit_check(df, "mg N/ha.")
  
  bds <- c(11, 30, 45, 60, 75)
  ts <- c(7, 27, 42, 57, 72)
  ls  <- list(l1, l2, l3, l4, l5)
  
  for (i in seq_along(ls)) {
    l  <- ls[[i]]
    bd <- bds[[i]]
    t <- ts[[i]]
    
    for (u in c("% V/V", "mg N/ha.")){
      cells <- unit_check(df, u)
      idx <- which(cells[, 2] %in% l)
      cells_idx <- as.matrix(cells[idx[1]:idx[2], , drop = FALSE])
      
      if(u == "% V/V"){
        func <- convert_sm
        args <- list(as.numeric(df[cells_idx]),
                     df[cells_idx[, 1], bd])
        new_unit <- "% g/g"
      } else if (u == "mg N/ha."){
        func <- convert_mgKg
        args <- list(as.numeric(df[cells_idx]),
                     df[cells_idx[, 1], t],
                     df[cells_idx[, 1], bd])
        new_unit <- "kg N/ha."
      }
      
      df[cells_idx] <- do.call(func, args)
      
      cells[, 2] <- cells[, 2] + 1
      df[cells] <- new_unit
    }
  }
  
  return(df)

}
  
