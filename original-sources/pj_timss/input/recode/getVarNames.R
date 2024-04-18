getVarNames <- function(sdf, variables = attr(sdf$pvvars, "default"), keywords = NULL, getid = TRUE, getjkseeds = TRUE){
  
  library(dplyr)
  library(EdSurvey)
  
  source(funcPath$getLevelVarsDF)
  
  
  codebook <- getLevelVarsDF(sdf = sdf)
  
  wgtbook <- codebook %>% 
    dplyr::filter(variableName %in% names(sdf$weights))
  
  
  defwgt <- attr(sdf$weights, "default")
  
  deffmt <- codebook %>% 
    dplyr::filter(variableName == defwgt) %>% 
    .$fileFormat %>% 
    unique()
  
  ids <- NULL
  
  jkseeds <- NULL
  
  
  if (!is.null(keywords)) {
    keywords <- c(keywords)
    
    for (keyword in keywords) {
      subs <- grep(
        keyword, 
        codebook %>% 
          .$variableName, 
        value = TRUE
      )
      
      variables <- c(variables, subs)
    }
  }
  
  variables <- unique(variables)
  
  
  fmts <- codebook %>% 
    dplyr::filter(variableName %in% variables) %>% 
    .$fileFormat %>% 
    c(deffmt, .) %>% 
    unique()
  
  wgts <- wgtbook %>% 
    dplyr::filter(fileFormat %in% fmts) %>% 
    .$variableName %>% 
    c(defwgt, .) %>% 
    unique()
  
  if (getid) {
    ids <- grep("^id", 
                (codebook %>% 
                   dplyr::filter(fileFormat %in% fmts) %>% 
                   .$variableName), 
                value = TRUE)
  }
  
  if (getjkseeds) {
      jkseeds <- c("jkzone", "jkrep")
  }
  
  
  unique(c(variables, wgts, ids, jkseeds)) %>%
    return()
  
}
