open_template <- function(sheet){
  
 return(read_params_table(template, num_na = -999.99, sheet_name = sheet))
}

read_sheet <- function(sheet,
                       excel_path){

  df <- read.xlsx(excel_path, sheet = sheet, startRow = 2, detectDates = TRUE)                             
  
  return(df)
}

invalid_sheets <- function(sheets){
  list <- tolower(sheets) %in% valid_sheets
  return(!list)
}


add_ext <- function(col,ext){
  
  wo_ext <- grepl(sprintf("\\_%s.xml$",ext), col)
  col[!wo_ext] <- paste(col[!wo_ext],sprintf("_%s.xml",ext), sep="")
  return(col)
}


is_excel_date <- function(df) {
  dates <- data.frame(lapply(df, function(x) {
    x_val <- suppressWarnings(as.numeric(x))
    !is.na(x_val) & x_val > 20000
  }))
  return(which(dates == TRUE, arr.ind = TRUE))
}

get_code_choice <- function(df){
  choices <- grep("=",df)
  df[,choices] <- lapply(df[,choices], function(x){
    sub("\\s=.*", "", x)
  }) 
  return(df)
}

unit_check <- function(df, unit){
  is_unit <- data.frame(lapply(df, function(x) {
    x_val <- x == unit
  }))
  cells <- which(is_unit == TRUE, arr.ind = TRUE)
  cells[,2] <- cells[,2]-1
  return(cells)
}

get_empty_vals <- function(df, value){
  return(which(is.na(df[value])))
}

missing_value <- function(value, calc){
  return(sprintf("%s is needed to calculate %s but is empty", value, calc)) 
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











