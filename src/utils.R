open_template <- function(sheet){
  
 return(read_params_table("files/templates/direct-input-template.xlsx",
                             num_na = -999.99, sheet_name = sheet))
}


add_ext <- function(col,ext){
  
  wo_ext <- grepl(sprintf("\\_%s.xml$",ext), col)
  col[!wo_ext] <- paste(col[!wo_ext],sprintf("_%s.xml",ext), sep="")
  return(col)
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
    message("Saved as csv in: ", csv_dir)
    return(paste("Saved as csv in: ", csv_dir))
  }
}

invalid_sheets <- function(sheets){
  list <- tolower(sheets) %in% valid_sheets
 !list
}

date_to_jul <- function(value){
  
}



valid_sheets <<- c("usms","usm",
                   "init","ini",
                   "sol","sols","soils","soil",
                   "tec", "management",
                   "sta", "stations",
                   "obs", "observations")

variables <<- readRDS("data/variables.RDS")







