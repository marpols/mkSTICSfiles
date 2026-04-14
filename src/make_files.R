make_files <- function(sheet,
                       excel_path,
                       outdir,
                       savecsv
){
  
  df <- read_params_table(excel_path, num_na = -999.99, sheet_name = sheet)
  
  m <- if (savecsv) save_csv(sheet, df) else ""
  
  invalid_chars <- df |> mutate(across(everything(), ~ grepl(" |-", .))) |>
    as.matrix() |>
    any()
  
  if (invalid_chars){
    df <- df |>
      mutate(across(everything(), ~ gsub(" ", "_", as.character(.))))
    
    df <- df |>
      mutate(across(everything(), ~ gsub("-", "", as.character(.))))
    warning("spaces and hyphens are not valid in STICS file names and have been removed or replaced")
  }
  
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
    
    stop("This is an error test")
    
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

pop_values <- function(sheet,
                            excel_path,
                            outdir
){
  
  xl_sheet_org <- read_sheet(sheet, excel_path)
  xl_sheet <- xl_sheet_org |>
    convert_dates(sheet) |>
    get_code_choice()
  
  if(sheet == "soils"){
    
  } else if (sheet == ""){
    
  }
  
  temp <- read.csv("data/var-values.csv")
}
  
  

  
