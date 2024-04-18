getLevelVarsList <- function(sdf){
  
  library(dplyr)
  
  lapply(sdf$dataList, function(levelData){
    dplyr::setdiff(levelData[["fileFormat"]][["variableName"]], levelData[["ignoreVars"]])
  }) %>% 
    return()
  
}
