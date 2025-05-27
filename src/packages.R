#Install and Apply Packages

if (!require("SticsRPacks")){
  devtools::install_github("SticsRPacks/SticsRPacks@*release")
  devtools::install_github("SticsRPacks/CroPlotR@*release")
}
if (!require("dplyr")){
  install.packages("dplyr")
}
if (!require("readxl")){
  install.packages("readxl")
}

