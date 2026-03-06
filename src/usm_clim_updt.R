usm_clim_updt <- function(stn_col,
                          clim1,
                          clim2){
  
  y.only1 <- grepl("^\\d{4}$",clim1)
  y.only2 <- grepl("^\\d{4}$",clim2)
  stn_names <- sub("_sta\\.xml$", "", stn_col)
  
  clim1[y.only1] <- paste(stn_names[which(y.only1)],
                          clim1[y.only1],
                          sep = ".")
  clim2[y.only2] <- paste(stn_names[which(y.only2)],
                          clim2[y.only2],
                          sep = ".")
  
  stn_col <- add_ext(stn_col,"sta")
  
  list(stn_col,clim1,clim2)
}