valid_sheets <- c("usms","usm",
                   "init","ini",
                   "sol","sols","soils","soil",
                   "tec", "management",
                   "sta", "stations",
                   "obs", "observations")

sheet_pairs <- data.frame(sheets = c("management", "soils", "soils", "stations", "usms", "observations"),
                          STICSfiles = c("tec", "sol", "ini", "sta", "usms", "obs"))

variables <- readRDS("../data/variables.RDS")

template <- "files/templates/direct-input-template.xlsx"

vbf <- 1.724


