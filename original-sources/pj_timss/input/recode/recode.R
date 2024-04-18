run_recode1 <- function(){
    
    library(dplyr)
    library(EdSurvey)
    
    source(funcPath$getVarNames)
    
    readRDS("./input/connect/connect_data/sdf_TIMSS_2015_jpn_4.obj") %>% 
        EdSurvey::getData(getVarNames(., c("mmat", "asbg01"), keywords = c("asbh", "asdh")),
                          omittedLevels = FALSE,
                          includeNaLabel = TRUE,
                          addAttributes = TRUE) %>%
        dplyr::mutate(
            isGirl = dplyr::recode(
                asbg01, 
                "GIRL" = 1, 
                .default = 0
            ), 
            isUniv = dplyr::recode(
                asdhedup,
                "UNIVERSITY OR HIGHER" = 1,
                .default = 0
            ),
            empStyleA = dplyr::recode(
                asbh22a,
                "WORKING FULL-TIME FOR PAY" = 2,
                "WORKING PART-TIME FOR PAY" = 1,
                "NOT WORKING FOR PAY" = 1,
                .default = 0
            ),
            empStyleB = dplyr::recode(
                asbh22b,
                "WORKING FULL-TIME FOR PAY" = 2,
                "WORKING PART-TIME FOR PAY" = 1,
                "NOT WORKING FOR PAY" = 1,
                .default = 0
            ),
            isElnOften = dplyr::recode(
                asdheln,
                "OFTEN" = 1,
                .default = 0
            ), 
            isLnt = dplyr::recode(
                asdhlnt, 
                "VERY WELL" = 1, 
                .default = 0
            ), 
            across(
                .cols = c(starts_with("asbh02")), 
                .fns = list(
                    isOften = ~dplyr::recode(
                        .x, 
                        "OFTEN" = 1, 
                        .default = 0
                    )
                ), 
                .names = "{.fn}_{.col}"
            ), 
            isAmsPositive = dplyr::recode(
                asdhams, 
                "VERY POSITIVE ATTITUDE" = 1, 
                "POSITIVE ATTITUDE" = 1, 
                .default = 0
            ), 
            isElm = dplyr::recode(
                asbh10aa, 
                "YES, TO EXCEL IN CLASS" = 1, 
                "YES, TO KEEP UP IN CLASS" = 1, 
                .default = 0
            ), 
            isElmExcel = dplyr::recode(
                asbh10aa, 
                "YES, TO EXCEL IN CLASS" = 1, 
                .default = 0
            ), 
            isRead6hWeek = dplyr::recode(
                asbh12, 
                "MORE THAN 10 HOURS A WEEK" = 1, 
                "6-10 HOURS A WEEK" = 1, 
                .default = 0
            ), 
            isBookOver25 = dplyr::recode(
                asbh13, 
                "MORE THAN 200" = 1, 
                "101-200" = 1, 
                "26-100" = 1, 
                .default = 0
            ), 
            isChildBookOver25 = dplyr::recode(
                asbh14, 
                "MORE THAN 100" = 1, 
                "51-100" = 1, 
                "26-50" = 1, 
                .default = 0
            ), 
            across(
                .cols = c(starts_with("asbh16")), 
                .fns = list(
                    isOften = ~dplyr::recode(
                        .x, 
                        "AGREE A LOT" = 1, 
                        "AGREE A LITTLE" = 1, 
                        .default = 0
                    )
                ), 
                .names = "{.fn}_{.col}"
            ), 
            isExpectUniv = dplyr::recode(
                asbh21, 
                "FINISH POSTGRADUATE DEGREE" = 1, 
                "FINISH BACHELOR’S OR EQUIVALENT" = 1, 
                .default = 0
            )
        ) %>%
        dplyr::mutate(
            familyModel = dplyr::case_when(
                empStyleA == 2 & empStyleB == 2 ~ 2,
                empStyleA == 2 & empStyleB == 1 ~ 1,
                empStyleA == 1 & empStyleB == 2 ~ 1,
                TRUE ~ 0
            )
        ) %>% 
        saveRDS("./input/recode/recode_data/ldf_TIMSS_2015_jpn_4.obj")
    
}

run_recode1()

