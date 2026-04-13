date_to_jul <- function(value){
  date <- as.Date(value, origin = "1899-12-30")
  return(format(date, "%j"))
}

convert_dates <- function(df){
  cells <- is_excel_date(df)
  df[cells] <- date_to_jul(as.numeric(df[cells]))
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