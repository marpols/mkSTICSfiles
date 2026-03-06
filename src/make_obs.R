make_obs <- function(sheet,
                     excel.path,
                     outdir,
                     save2csv
){
  #generates .obs files for STICS where each sheet of an excel file is an individual usm/set of observations
  #xl_file (chr) - path of excel file containing observations
  #outdir (chr) - path of output directory
  df <- readxl::read_excel(excel.path, sheet = sheet)
  
  if(grepl(sprintf("/files/%s",format(Sys.Date(), "%Y-%m-%d")), outdir)){
    fn <- sprintf("../files/%s/obs",format(Sys.Date(), "%Y-%m-%d"))
    dir_create(fn)
    fn <- file.path(fn,sprintf("%s.obs",gsub("obs - ", "", sheet)))
  } else {
    fn <- file.path(outdir,sprintf("%s.obs"), gsub("obs - ", "", sheet))
  }
  write.table(df, fn, sep=";", row.names = FALSE, quote = FALSE)
  
  m <- if (save2csv) save_csv(sheet, df, obs = "obs") else ""
  
  return(c(sprintf(".obs file for %s containing %d observations for %d parameters -  %s",
                   gsub("obs - ", "", sheet), nrow(df), ncol(df) - 4, date()),
           m))
  
}