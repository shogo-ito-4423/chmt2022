getLevelVarsDF <- function(sdf){
  
  library(dplyr)
  
  source(funcPath$getLevelVarsList)
  
  levelVarsList <- getLevelVarsList(sdf = sdf)
  df <- data.frame(variableName = NULL, fileFormat = NULL)
  
  for (levelName in names(levelVarsList)) {
    variableName <- levelVarsList[[levelName]]
    fileFormat <- rep(levelName, length(variableName))
    df <- dplyr::bind_rows(df, data.frame(variableName, fileFormat))
  }
  
  return(df)
  
}
