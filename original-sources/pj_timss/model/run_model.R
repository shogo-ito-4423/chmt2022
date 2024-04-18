run_model <- function(){
    
    library(tidyverse)
    library(EdSurvey)
    library(BIFIEsurvey)
    
    
    model0 <- '
    asmmat ~ b1*isUniv + b2*isElmExcel + b3*isElnOften + b4*isGirl
    isElmExcel ~ c1*isUniv + c2*isGirl
    isElnOften ~ d1*isUniv + d2*isGirl
    ide1 := b2*c1
    ide2 := b3*d1
    te := b1+ide1+ide2
    er1 := ide1/te
    er2 := ide2/te
    '
    
    model1 <- '
    asmmat ~ b1*isUniv + b2*isElmExcel + b3*isElnOften + b4*isGirl
    isElmExcel ~ c1*isUniv + c2*isGirl
    isElnOften ~ d1*isUniv + d2*isGirl
    ide1 := b2*c1
    ide2 := b3*d1
    te := b1+ide1+ide2
    er1 := ide1/te
    er2 := ide2/te
    '
    
    model2 <-  '
    asmmat ~ b1*isUniv + b2*isC2_4 + b3*isC3_4 + b4*isGirl
    isC2_4 ~ c1*isUniv + c2*isGirl
    isC3_4 ~ d1*isUniv + d2*isGirl
    ide1 := b2*c1
    ide2 := b3*d1
    te := b1+ide1+ide2
    er1 := ide1/te
    er2 := ide2/te
    '
    
    mca_list <- readRDS("./mca/mca_obj/mca_list.obj")
    
    ldf_model <- readRDS("./input/recode/recode_data/ldf_TIMSS_2015_jpn_4.obj") %>% 
        filter(!is.na(asbheln)) %>% 
        dplyr::bind_cols(bind_cols(data.frame(clust = mca_list$hcpc0$data.clust$clust))) %>% 
        dplyr::mutate(
            isC2_4 = dplyr::case_when(
                clust == 2 | clust == 4 ~ 1, 
                TRUE ~ 0
            ), 
            isC3_4 = dplyr::case_when(
                clust == 3 | clust == 4 ~ 1, 
                TRUE ~ 0
            )
        )
    
    bfit1 <- ldf_model %>% 
        BIFIEsurvey::BIFIE.data.jack(
            jktype="JK_TIMSS2", 
            wgt = "totwgt", 
            jkzone = "jkzone", 
            jkrep = "jkrep", 
            pv_vars = c("asmmat")
        ) %>% 
        BIFIEsurvey::BIFIE.lavaan.survey(
            lavmodel = model1, 
            svyrepdes = .
        )
    
    bfit2 <- ldf_model %>% 
        BIFIEsurvey::BIFIE.data.jack(
            jktype="JK_TIMSS2", 
            wgt = "totwgt", 
            jkzone = "jkzone", 
            jkrep = "jkrep", 
            pv_vars = c("asmmat")
        ) %>% 
        BIFIEsurvey::BIFIE.lavaan.survey(
            lavmodel = model2, 
            svyrepdes = .
        )
    
    list(
        ldf_model = ldf_model, 
        bfit1 = bfit1, 
        bfit2 = bfit2
    ) %>% 
        saveRDS("./model/model_obj/model_list.obj")
    
    ldf_model %>% 
        BIFIEsurvey::BIFIE.data.jack(
            jktype="JK_TIMSS2", 
            wgt = "totwgt", 
            jkzone = "jkzone", 
            jkrep = "jkrep", 
            pv_vars = c("asmmat")
            ) %>% 
        BIFIEsurvey::BIFIE.lavaan.survey(
            lavmodel = model0, 
            svyrepdes = .
        ) %>% 
        saveRDS("./model/model_obj/bfit0.obj")
    
}

run_model()
