date_to_jul <- function(value){
  date <- as.Date(value, origin = "1899-12-30")
  return(format(date, "%j"))
}

convert_dates <- function(df, sheet){
  cells <- is_excel_date(df)
  result <- tryCatch({
    df[cells] <- date_to_jul(as.numeric(df[cells]))
    },
    error = function(e){
      message("no dates for conversion in sheet ", sheet)
      return(df)
    })
  return(df)
}

convert_sm <- function(value, bd){
  return(value * bd)
}

convert_mgKg <- function(value, depth, bd){
  return((value * depth * bd)/10)
}

convert_kmh <- function(speed){
  return(speed/3.6)
}