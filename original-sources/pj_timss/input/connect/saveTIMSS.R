saveTIMSS <- function(read_path, years, countries, gradeLvl = 0, save_path){
  
  library(magrittr)
  library(EdSurvey)
  
  countries_str <- paste0(countries, collapse = "W")
  if (is.vector(gradeLvl)) {
    gradeLvl_str <- paste0(gradeLvl, collapse = "W")
  } else {
    gradeLvl_str <- gradeLvl
  }
  
  for (year in years) {
    
    read_path_y <- paste0(read_path, "/TIMSS/", year)
    base_name <- paste("sdf_TIMSS", year, countries_str, gradeLvl_str, sep = "_")
    save_path_y <- paste0(save_path, "/", base_name, ".obj")
    
    EdSurvey::readTIMSS(path = read_path_y, countries = countries, gradeLvl = gradeLvl) %>% 
      saveRDS(file = save_path_y)
    
  }
  
}