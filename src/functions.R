
make_files <- function(sheet,
                       excel.path,
                       outdir,
                       save2csv
                       ){

  df <- read_params_table(excel.path, num_na = -999.99, sheet_name = sheet)
  
  m <- if (save2csv) save_csv(sheet, df) else ""
  
  if (tolower(sheet) %in% c("usms")){ #USMS
    df[which(df$finit == "NA"),4] <- paste(df$usm_name[which(df$finit == "NA")],
                                              "_ini.xml", sep="")
    
    df$finit <- add_ext(df$finit,"ini")
    usm.clim <- usm_clim_updt(df$fstation,
                              df$fclim1,
                              df$fclim2)
    
    df$fstation <- usm.clim[[1]]
    df$fclim1 <- usm.clim[[2]]
    df$fclim2 <- usm.clim[[3]]

    gen_usms_xml(file.path(outdir,"usms.xml"), df)
    
    return(c(sprintf("usms.xml file with %d usm(s) -  %s", nrow(df), date()),
           m))
    
  } else if (tolower(sheet) %in% c("ini","init")){ #INI
    
    df$Ini_name <- add_ext(df$Ini_name,"ini")
    
    gen_ini_xml(param_df = df,out_dir = outdir)
    
    return(c(sprintf("%d ini.xml files -  %s", nrow(df), date()),
           m))
    
  } else if (tolower(sheet) %in% c("sol","sols","soils","soil")){ #SOL
    
    gen_sols_xml(file = file.path(outdir,"sols.xml"), param_df = df)
    
    return(c(sprintf("sol.xml file containing %d profile(s) -  %s",
                   nrow(df), date()),
           m))
    
  } else if (tolower(sheet) %in% c("tec")){ #TEC
    
    df$Tec_name <- add_ext(df$Tec_name,"tec")
    
    gen_tec_xml(param_df = df,out_dir = outdir)
    
    return(c(sprintf("%d tec.xml files -  %s", nrow(df), date()),
           m))
    
  } else if (tolower(sheet) %in% c("sta","station")){ #STA
    
    df$Sta_name <- add_ext(df$Sta_name,"sta")
    
    gen_sta_xml(param_df = df,out_dir = outdir)
    
    return(c(sprintf("%d sta.xml files -  %s", nrow(df), date()),
           m))
  }

}

add_ext <- function(col,ext){
  
  w.ext <- grepl(sprintf("\\_%s.xml$",ext), col)
  col[!w.ext] <- paste(col[!w.ext],sprintf("_%s.xml",ext), sep="")
  col
}

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

save_csv <- function(sheet,
                     df,
                     obs = ""){
  if(save2csv){
    #save excel sheets to individual .csv files
    csv_dir <- sprintf("../files/%s/csv_files/%s", 
                       format(Sys.Date(), "%Y-%m-%d"), 
                       obs)
    dir_create(csv_dir)
    write.csv(df, sprintf("%s/%s.csv",csv_dir,sheet), row.names = F, quote = F)
    message("saved as csv in: ", csv_dir)
    return(paste("Saved as csv in: ", csv_dir))
  }
}

make_obs <- function(sheet,
                     excel.path,
                     outdir,
                     save2csv
                     ){
  #generates .obs files for STICS where each sheet of an excel file is an individual usm/set of observations
  #xl_file (chr) - path of excel file containing observations
  #outdir (chr) - path of output directory
  df <- readxl::read_excel(excel.path, sheet = sheet)
  
  if(outdir ==  sprintf("../files/%s",format(Sys.Date(), "%Y-%m-%d"))){
    fn <- sprintf("../files/%s/obs",format(Sys.Date(), "%Y-%m-%d"))
    create_dir(fn)
    fn <- file.path(fn,sprintf("%s.obs"))
  } else {
    fn <- file.path(outdir,sprintf("%s.obs"))
  }
  write.table(df, fn, sep=";", row.names = FALSE, quote = FALSE)
  
  m <- if (save2csv) save_csv(sheet, df, obs = "obs") else ""
  
  return(c(sprintf("%d .obs files -  %s", nrow(df), date()),
           m))
  
}

invalid_sheets <- function(sheets){
  list <- tolower(sheets) %in% valid_sheets
 !list
}

valid_sheets <<- c("usms","usm",
                  "init","ini",
                  "sol","sols","soils","soil",
                  "tec",
                  "sta", "station")





