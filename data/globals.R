valid_sheets <- c("usms","usm",
                   "init","ini",
                   "sol","sols","soils","soil",
                   "tec", "management",
                   "sta", "stations",
                   "obs", "observations")

variables <- readRDS("data/variables.RDS")

template <- "files/templates/direct-input-template.xlsx"

vbf <- 1.724


