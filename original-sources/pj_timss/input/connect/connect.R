source("./input/connect/saveTIMSS.R")

saveTIMSS(
  read_path = "../database_timss", 
  years = c(2007, 2011, 2015, 2019), 
  countries = c("jpn"), 
  gradeLvl = 8, 
  save_path = "./input/connect/connect_data"
)

saveTIMSS(
  read_path = "../database_timss", 
  years = c(2007, 2011, 2015, 2019), 
  countries = c("jpn"), 
  gradeLvl = 4, 
  save_path = "./input/connect/connect_data"
)
