#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#Generate STICS xml files from tabulated data
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

#source files---------------------
source("src/packages.R")
source("src/functions.R")


#Select file and output directory--------
#if no output directory selected xml files will be saved in project folder "files"


fn <- "dataforSTICS_tabulatedTEMPLATE.xlsx"
OR
fn <- rstudioapi::selectFile()


outdir <- rstudioapi::selectDirectory()
