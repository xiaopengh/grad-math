library(readxl)
library(dplyr)

colnames <- read_excel("wagesmicrodata.xls", 
                       sheet = "Data", range = "A1:L1", 
                       col_names = FALSE) %>% as.character()

df <- read_excel("wagesmicrodata.xls", 
                 sheet = "Data", skip = 2,
                 col_names = colnames)

