run_mca <- function(){
    
    library(tidyverse)
    library(EdSurvey)
    library(FactoMineR)
    
    mca0_vars <- data.frame(
        vars = c("isElnOften", "isElmExcel", "isAmsPositive", "isRead6hWeek", "isBookOver25", "isExpectUniv"), 
        ja_vars = c("知育高頻度", "通塾高成績", "理数好意的", "読書親週6時間以上", "本親26冊以上", "大卒以上期待")
    )
    
    named_mca0_ja_vars <- setNames(mca0_vars$vars, mca0_vars$ja_vars)
    
    ldf_mca <- readRDS("./input/recode/recode_data/ldf_TIMSS_2015_jpn_4.obj") %>% 
        filter(!is.na(asbheln)) %>% 
        dplyr::select(mca0_vars$vars) %>% 
        dplyr::mutate(
            dplyr::across(
                .fns = ~as.factor(.x)
            )
        )
    
    mca0 <- ldf_mca %>% 
        FactoMineR::MCA(graph = FALSE)
    
    mca0_ja <- ldf_mca %>% 
        dplyr::rename(!!!named_mca0_ja_vars) %>% 
        FactoMineR::MCA(graph = FALSE)
    
    hcpc0 <- mca0 %>% 
        FactoMineR::HCPC(min = 4, nb.clust = -1, graph = FALSE)
    
    hcpc0_ja <- mca0_ja %>% 
        FactoMineR::HCPC(min = 4, nb.clust = -1, graph = FALSE)
    
    list(
        mca0_vars = mca0_vars, 
        named_mca0_ja_vars = named_mca0_ja_vars, 
        ldf_mca = ldf_mca, 
        mca0 = mca0, 
        mca0_ja = mca0_ja, 
        hcpc0 = hcpc0, 
        hcpc0_ja = hcpc0_ja
    ) %>% 
        saveRDS("./mca/mca_obj/mca_list.obj")
    
}

run_mca()
