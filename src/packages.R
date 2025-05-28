#Install and Apply Packages

if (!require("SticsRFiles")){
  devtools::install_github("SticsRPacks/SticsRFiles@*release")
}
if (!require("dplyr")){
  install.packages("dplyr")
}
if (!require("readxl")){
  install.packages("readxl")
}
if (!require("shiny")){
  install.packages("shiny")
}
if (!require("fs")){
  install.packages("fs")
}
if (!require("shinyFiles")){
  install.packages("shinyFiles")
}
