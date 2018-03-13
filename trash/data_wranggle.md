ESS
================
Rebecca & Simon

Packages
--------

``` r
pacman::p_load(dplyr, ggplot2, readr, haven, broom, purrr, tidyr, magrittr, labelled, sjPlot, viridis, forcats, ggthemes)
```

Data
----

``` r
get_party <- function(x){
  dat1 <- x %>% 
    select(contains("prtv"), prtvede1, prtvede2) %>%
    mutate_all(sjmisc::to_label) %>% 
    mutate_all(as.character) %>%
    mutate(vote = apply(., 1, function(x) paste(ifelse(is.na(x), "", x), collapse = ""))) %>%
    mutate(vote = vote %>%
           stringr::str_replace_all("SPD", " SPD ") %>%
           stringr::str_replace_all("CDU/CSU", " CDU/CSU ") %>%
           stringr::str_replace_all("FDP", " FDP ") %>%
           stringr::str_replace_all("NPD", " NPD ") %>%
           stringr::str_replace_all("Die Linke", " DieLinke ") %>%
           stringr::str_replace_all("AfD", " AfD ") %>%
           stringr::str_replace_all("Andere Partei", " AnderePartei ") %>%
           stringr::str_replace_all("Piratenpartei", " Piratenpartei ") %>%
           stringr::str_replace_all("Bündnis.*?Grünen", " Bündnis90/DieGrünen ") %>%
           stringr::str_replace_all("^Grünen", " Bündnis90/DieGrünen ") %>%
           stringr::str_replace_all("\\s+", " ") %>%
           stringr::str_trim() %>%
           stringr::str_replace_all("^.*? ", "")
         )
  
  vote_id <- x %>%
    select(contains("prtv"), prtvede1, prtvede2) %>%
    #mutate_all(sjmisc::to_label) %>% 
    mutate_all(as.character) %>%
    split(1:nrow(x)) %>%
    map_chr(~ paste(ifelse(is.na(.x), "", .x), collapse = ""))
    #mutate(vote_id = map_int(. )) #apply(., 1, function(x) paste(ifelse(is.na(x), "", x), collapse = "")))
  
  x %>%
    mutate(vote = dat1$vote, vote_id = vote_id)
}

ess <- read_sav("data/ESS8e01.sav") %>%
  mutate(cname = sjmisc::to_label(cntry)) %>%
  get_party()

ess_add <- read_sav("data/ESS1-7e01.sav") %>%
  filter(! cntry == "RU" & !cntry == 6) %>%
  mutate(cname = sjmisc::to_label(cntry)) %>%
  get_party()
# unique(ess$cntry) %in% unique(ess_add$cntry)

ess_all <- bind_rows(ess, ess_add)
```

    ## Warning in bind_rows_(x, .id): Vectorizing 'labelled' elements may not
    ## preserve their attributes

    ## Warning in bind_rows_(x, .id): Vectorizing 'labelled' elements may not
    ## preserve their attributes

    ## Warning in bind_rows_(x, .id): Vectorizing 'labelled' elements may not
    ## preserve their attributes

    ## Warning in bind_rows_(x, .id): Vectorizing 'labelled' elements may not
    ## preserve their attributes

    ## Warning in bind_rows_(x, .id): Vectorizing 'labelled' elements may not
    ## preserve their attributes

    ## Warning in bind_rows_(x, .id): Vectorizing 'labelled' elements may not
    ## preserve their attributes

    ## Warning in bind_rows_(x, .id): Vectorizing 'labelled' elements may not
    ## preserve their attributes

    ## Warning in bind_rows_(x, .id): Vectorizing 'labelled' elements may not
    ## preserve their attributes

    ## Warning in bind_rows_(x, .id): Vectorizing 'labelled' elements may not
    ## preserve their attributes

    ## Warning in bind_rows_(x, .id): Vectorizing 'labelled' elements may not
    ## preserve their attributes

    ## Warning in bind_rows_(x, .id): Vectorizing 'labelled' elements may not
    ## preserve their attributes

    ## Warning in bind_rows_(x, .id): Vectorizing 'labelled' elements may not
    ## preserve their attributes

    ## Warning in bind_rows_(x, .id): Vectorizing 'labelled' elements may not
    ## preserve their attributes

    ## Warning in bind_rows_(x, .id): Vectorizing 'labelled' elements may not
    ## preserve their attributes

    ## Warning in bind_rows_(x, .id): Vectorizing 'labelled' elements may not
    ## preserve their attributes

    ## Warning in bind_rows_(x, .id): Vectorizing 'labelled' elements may not
    ## preserve their attributes

    ## Warning in bind_rows_(x, .id): Vectorizing 'labelled' elements may not
    ## preserve their attributes

    ## Warning in bind_rows_(x, .id): Vectorizing 'labelled' elements may not
    ## preserve their attributes

    ## Warning in bind_rows_(x, .id): Vectorizing 'labelled' elements may not
    ## preserve their attributes

    ## Warning in bind_rows_(x, .id): Vectorizing 'labelled' elements may not
    ## preserve their attributes

    ## Warning in bind_rows_(x, .id): Vectorizing 'labelled' elements may not
    ## preserve their attributes

    ## Warning in bind_rows_(x, .id): Vectorizing 'labelled' elements may not
    ## preserve their attributes

    ## Warning in bind_rows_(x, .id): Vectorizing 'labelled' elements may not
    ## preserve their attributes

    ## Warning in bind_rows_(x, .id): Vectorizing 'labelled' elements may not
    ## preserve their attributes

    ## Warning in bind_rows_(x, .id): Vectorizing 'labelled' elements may not
    ## preserve their attributes

    ## Warning in bind_rows_(x, .id): Vectorizing 'labelled' elements may not
    ## preserve their attributes

    ## Warning in bind_rows_(x, .id): Vectorizing 'labelled' elements may not
    ## preserve their attributes

    ## Warning in bind_rows_(x, .id): Vectorizing 'labelled' elements may not
    ## preserve their attributes

    ## Warning in bind_rows_(x, .id): Vectorizing 'labelled' elements may not
    ## preserve their attributes

    ## Warning in bind_rows_(x, .id): Vectorizing 'labelled' elements may not
    ## preserve their attributes

    ## Warning in bind_rows_(x, .id): Vectorizing 'labelled' elements may not
    ## preserve their attributes

    ## Warning in bind_rows_(x, .id): Vectorizing 'labelled' elements may not
    ## preserve their attributes

    ## Warning in bind_rows_(x, .id): Vectorizing 'labelled' elements may not
    ## preserve their attributes

    ## Warning in bind_rows_(x, .id): Vectorizing 'labelled' elements may not
    ## preserve their attributes

    ## Warning in bind_rows_(x, .id): Vectorizing 'labelled' elements may not
    ## preserve their attributes

    ## Warning in bind_rows_(x, .id): Vectorizing 'labelled' elements may not
    ## preserve their attributes

    ## Warning in bind_rows_(x, .id): Vectorizing 'labelled' elements may not
    ## preserve their attributes

    ## Warning in bind_rows_(x, .id): Vectorizing 'labelled' elements may not
    ## preserve their attributes

    ## Warning in bind_rows_(x, .id): Vectorizing 'labelled' elements may not
    ## preserve their attributes

    ## Warning in bind_rows_(x, .id): Vectorizing 'labelled' elements may not
    ## preserve their attributes

    ## Warning in bind_rows_(x, .id): Vectorizing 'labelled' elements may not
    ## preserve their attributes

    ## Warning in bind_rows_(x, .id): Vectorizing 'labelled' elements may not
    ## preserve their attributes

    ## Warning in bind_rows_(x, .id): Vectorizing 'labelled' elements may not
    ## preserve their attributes

    ## Warning in bind_rows_(x, .id): Vectorizing 'labelled' elements may not
    ## preserve their attributes

    ## Warning in bind_rows_(x, .id): Vectorizing 'labelled' elements may not
    ## preserve their attributes

    ## Warning in bind_rows_(x, .id): Vectorizing 'labelled' elements may not
    ## preserve their attributes

    ## Warning in bind_rows_(x, .id): Vectorizing 'labelled' elements may not
    ## preserve their attributes

    ## Warning in bind_rows_(x, .id): Vectorizing 'labelled' elements may not
    ## preserve their attributes

    ## Warning in bind_rows_(x, .id): Vectorizing 'labelled' elements may not
    ## preserve their attributes

    ## Warning in bind_rows_(x, .id): Vectorizing 'labelled' elements may not
    ## preserve their attributes

    ## Warning in bind_rows_(x, .id): Vectorizing 'labelled' elements may not
    ## preserve their attributes

    ## Warning in bind_rows_(x, .id): Vectorizing 'labelled' elements may not
    ## preserve their attributes

    ## Warning in bind_rows_(x, .id): Vectorizing 'labelled' elements may not
    ## preserve their attributes

    ## Warning in bind_rows_(x, .id): Vectorizing 'labelled' elements may not
    ## preserve their attributes

    ## Warning in bind_rows_(x, .id): Vectorizing 'labelled' elements may not
    ## preserve their attributes

    ## Warning in bind_rows_(x, .id): Vectorizing 'labelled' elements may not
    ## preserve their attributes

    ## Warning in bind_rows_(x, .id): Vectorizing 'labelled' elements may not
    ## preserve their attributes

    ## Warning in bind_rows_(x, .id): Vectorizing 'labelled' elements may not
    ## preserve their attributes

    ## Warning in bind_rows_(x, .id): Vectorizing 'labelled' elements may not
    ## preserve their attributes

    ## Warning in bind_rows_(x, .id): Vectorizing 'labelled' elements may not
    ## preserve their attributes

    ## Warning in bind_rows_(x, .id): Vectorizing 'labelled' elements may not
    ## preserve their attributes

    ## Warning in bind_rows_(x, .id): Vectorizing 'labelled' elements may not
    ## preserve their attributes

    ## Warning in bind_rows_(x, .id): Vectorizing 'labelled' elements may not
    ## preserve their attributes

    ## Warning in bind_rows_(x, .id): Vectorizing 'labelled' elements may not
    ## preserve their attributes

    ## Warning in bind_rows_(x, .id): Vectorizing 'labelled' elements may not
    ## preserve their attributes

    ## Warning in bind_rows_(x, .id): Vectorizing 'labelled' elements may not
    ## preserve their attributes

    ## Warning in bind_rows_(x, .id): Vectorizing 'labelled' elements may not
    ## preserve their attributes

    ## Warning in bind_rows_(x, .id): Vectorizing 'labelled' elements may not
    ## preserve their attributes

    ## Warning in bind_rows_(x, .id): Vectorizing 'labelled' elements may not
    ## preserve their attributes

    ## Warning in bind_rows_(x, .id): Vectorizing 'labelled' elements may not
    ## preserve their attributes

    ## Warning in bind_rows_(x, .id): Vectorizing 'labelled' elements may not
    ## preserve their attributes

    ## Warning in bind_rows_(x, .id): Vectorizing 'labelled' elements may not
    ## preserve their attributes

    ## Warning in bind_rows_(x, .id): Vectorizing 'labelled' elements may not
    ## preserve their attributes

    ## Warning in bind_rows_(x, .id): Vectorizing 'labelled' elements may not
    ## preserve their attributes

    ## Warning in bind_rows_(x, .id): Vectorizing 'labelled' elements may not
    ## preserve their attributes

    ## Warning in bind_rows_(x, .id): Vectorizing 'labelled' elements may not
    ## preserve their attributes

    ## Warning in bind_rows_(x, .id): Vectorizing 'labelled' elements may not
    ## preserve their attributes

    ## Warning in bind_rows_(x, .id): Vectorizing 'labelled' elements may not
    ## preserve their attributes

    ## Warning in bind_rows_(x, .id): Vectorizing 'labelled' elements may not
    ## preserve their attributes

    ## Warning in bind_rows_(x, .id): Vectorizing 'labelled' elements may not
    ## preserve their attributes

    ## Warning in bind_rows_(x, .id): Vectorizing 'labelled' elements may not
    ## preserve their attributes

    ## Warning in bind_rows_(x, .id): Vectorizing 'labelled' elements may not
    ## preserve their attributes

    ## Warning in bind_rows_(x, .id): Vectorizing 'labelled' elements may not
    ## preserve their attributes

    ## Warning in bind_rows_(x, .id): Vectorizing 'labelled' elements may not
    ## preserve their attributes

    ## Warning in bind_rows_(x, .id): Vectorizing 'labelled' elements may not
    ## preserve their attributes

    ## Warning in bind_rows_(x, .id): Vectorizing 'labelled' elements may not
    ## preserve their attributes

    ## Warning in bind_rows_(x, .id): Vectorizing 'labelled' elements may not
    ## preserve their attributes

    ## Warning in bind_rows_(x, .id): Vectorizing 'labelled' elements may not
    ## preserve their attributes

    ## Warning in bind_rows_(x, .id): Vectorizing 'labelled' elements may not
    ## preserve their attributes

    ## Warning in bind_rows_(x, .id): Vectorizing 'labelled' elements may not
    ## preserve their attributes

    ## Warning in bind_rows_(x, .id): Vectorizing 'labelled' elements may not
    ## preserve their attributes

    ## Warning in bind_rows_(x, .id): Vectorizing 'labelled' elements may not
    ## preserve their attributes

    ## Warning in bind_rows_(x, .id): Vectorizing 'labelled' elements may not
    ## preserve their attributes

    ## Warning in bind_rows_(x, .id): Vectorizing 'labelled' elements may not
    ## preserve their attributes

    ## Warning in bind_rows_(x, .id): Vectorizing 'labelled' elements may not
    ## preserve their attributes

    ## Warning in bind_rows_(x, .id): Vectorizing 'labelled' elements may not
    ## preserve their attributes

    ## Warning in bind_rows_(x, .id): Vectorizing 'labelled' elements may not
    ## preserve their attributes

    ## Warning in bind_rows_(x, .id): Vectorizing 'labelled' elements may not
    ## preserve their attributes

    ## Warning in bind_rows_(x, .id): Vectorizing 'labelled' elements may not
    ## preserve their attributes

    ## Warning in bind_rows_(x, .id): Vectorizing 'labelled' elements may not
    ## preserve their attributes

    ## Warning in bind_rows_(x, .id): Vectorizing 'labelled' elements may not
    ## preserve their attributes

    ## Warning in bind_rows_(x, .id): Vectorizing 'labelled' elements may not
    ## preserve their attributes

    ## Warning in bind_rows_(x, .id): Vectorizing 'labelled' elements may not
    ## preserve their attributes

    ## Warning in bind_rows_(x, .id): Vectorizing 'labelled' elements may not
    ## preserve their attributes

    ## Warning in bind_rows_(x, .id): Vectorizing 'labelled' elements may not
    ## preserve their attributes

    ## Warning in bind_rows_(x, .id): Vectorizing 'labelled' elements may not
    ## preserve their attributes

    ## Warning in bind_rows_(x, .id): Vectorizing 'labelled' elements may not
    ## preserve their attributes

    ## Warning in bind_rows_(x, .id): Vectorizing 'labelled' elements may not
    ## preserve their attributes

    ## Warning in bind_rows_(x, .id): Vectorizing 'labelled' elements may not
    ## preserve their attributes

    ## Warning in bind_rows_(x, .id): Vectorizing 'labelled' elements may not
    ## preserve their attributes

    ## Warning in bind_rows_(x, .id): Vectorizing 'labelled' elements may not
    ## preserve their attributes

    ## Warning in bind_rows_(x, .id): Vectorizing 'labelled' elements may not
    ## preserve their attributes

    ## Warning in bind_rows_(x, .id): Vectorizing 'labelled' elements may not
    ## preserve their attributes

    ## Warning in bind_rows_(x, .id): Vectorizing 'labelled' elements may not
    ## preserve their attributes

    ## Warning in bind_rows_(x, .id): Vectorizing 'labelled' elements may not
    ## preserve their attributes

    ## Warning in bind_rows_(x, .id): Vectorizing 'labelled' elements may not
    ## preserve their attributes

    ## Warning in bind_rows_(x, .id): Vectorizing 'labelled' elements may not
    ## preserve their attributes

    ## Warning in bind_rows_(x, .id): Vectorizing 'labelled' elements may not
    ## preserve their attributes

    ## Warning in bind_rows_(x, .id): Vectorizing 'labelled' elements may not
    ## preserve their attributes

    ## Warning in bind_rows_(x, .id): Vectorizing 'labelled' elements may not
    ## preserve their attributes

    ## Warning in bind_rows_(x, .id): Vectorizing 'labelled' elements may not
    ## preserve their attributes

    ## Warning in bind_rows_(x, .id): Vectorizing 'labelled' elements may not
    ## preserve their attributes

    ## Warning in bind_rows_(x, .id): Vectorizing 'labelled' elements may not
    ## preserve their attributes

    ## Warning in bind_rows_(x, .id): Vectorizing 'labelled' elements may not
    ## preserve their attributes

    ## Warning in bind_rows_(x, .id): Vectorizing 'labelled' elements may not
    ## preserve their attributes

    ## Warning in bind_rows_(x, .id): Vectorizing 'labelled' elements may not
    ## preserve their attributes

    ## Warning in bind_rows_(x, .id): Vectorizing 'labelled' elements may not
    ## preserve their attributes

    ## Warning in bind_rows_(x, .id): Vectorizing 'labelled' elements may not
    ## preserve their attributes

    ## Warning in bind_rows_(x, .id): Vectorizing 'labelled' elements may not
    ## preserve their attributes

    ## Warning in bind_rows_(x, .id): Vectorizing 'labelled' elements may not
    ## preserve their attributes

    ## Warning in bind_rows_(x, .id): Vectorizing 'labelled' elements may not
    ## preserve their attributes

    ## Warning in bind_rows_(x, .id): Vectorizing 'labelled' elements may not
    ## preserve their attributes

    ## Warning in bind_rows_(x, .id): Vectorizing 'labelled' elements may not
    ## preserve their attributes

    ## Warning in bind_rows_(x, .id): Vectorizing 'labelled' elements may not
    ## preserve their attributes

    ## Warning in bind_rows_(x, .id): Vectorizing 'labelled' elements may not
    ## preserve their attributes

    ## Warning in bind_rows_(x, .id): Vectorizing 'labelled' elements may not
    ## preserve their attributes

    ## Warning in bind_rows_(x, .id): Vectorizing 'labelled' elements may not
    ## preserve their attributes

    ## Warning in bind_rows_(x, .id): Vectorizing 'labelled' elements may not
    ## preserve their attributes

    ## Warning in bind_rows_(x, .id): Vectorizing 'labelled' elements may not
    ## preserve their attributes

    ## Warning in bind_rows_(x, .id): Vectorizing 'labelled' elements may not
    ## preserve their attributes

    ## Warning in bind_rows_(x, .id): Vectorizing 'labelled' elements may not
    ## preserve their attributes

    ## Warning in bind_rows_(x, .id): Vectorizing 'labelled' elements may not
    ## preserve their attributes

    ## Warning in bind_rows_(x, .id): Vectorizing 'labelled' elements may not
    ## preserve their attributes

    ## Warning in bind_rows_(x, .id): Vectorizing 'labelled' elements may not
    ## preserve their attributes

    ## Warning in bind_rows_(x, .id): Vectorizing 'labelled' elements may not
    ## preserve their attributes

    ## Warning in bind_rows_(x, .id): Vectorizing 'labelled' elements may not
    ## preserve their attributes

    ## Warning in bind_rows_(x, .id): Vectorizing 'labelled' elements may not
    ## preserve their attributes

    ## Warning in bind_rows_(x, .id): Vectorizing 'labelled' elements may not
    ## preserve their attributes

    ## Warning in bind_rows_(x, .id): Vectorizing 'labelled' elements may not
    ## preserve their attributes

    ## Warning in bind_rows_(x, .id): Vectorizing 'labelled' elements may not
    ## preserve their attributes

    ## Warning in bind_rows_(x, .id): Vectorizing 'labelled' elements may not
    ## preserve their attributes

    ## Warning in bind_rows_(x, .id): Vectorizing 'labelled' elements may not
    ## preserve their attributes

    ## Warning in bind_rows_(x, .id): Vectorizing 'labelled' elements may not
    ## preserve their attributes

    ## Warning in bind_rows_(x, .id): Vectorizing 'labelled' elements may not
    ## preserve their attributes

    ## Warning in bind_rows_(x, .id): Vectorizing 'labelled' elements may not
    ## preserve their attributes

    ## Warning in bind_rows_(x, .id): Vectorizing 'labelled' elements may not
    ## preserve their attributes

    ## Warning in bind_rows_(x, .id): Vectorizing 'labelled' elements may not
    ## preserve their attributes

    ## Warning in bind_rows_(x, .id): Vectorizing 'labelled' elements may not
    ## preserve their attributes

    ## Warning in bind_rows_(x, .id): Vectorizing 'labelled' elements may not
    ## preserve their attributes

    ## Warning in bind_rows_(x, .id): Vectorizing 'labelled' elements may not
    ## preserve their attributes

    ## Warning in bind_rows_(x, .id): Vectorizing 'labelled' elements may not
    ## preserve their attributes

    ## Warning in bind_rows_(x, .id): Vectorizing 'labelled' elements may not
    ## preserve their attributes

    ## Warning in bind_rows_(x, .id): Vectorizing 'labelled' elements may not
    ## preserve their attributes

    ## Warning in bind_rows_(x, .id): Vectorizing 'labelled' elements may not
    ## preserve their attributes

    ## Warning in bind_rows_(x, .id): Vectorizing 'labelled' elements may not
    ## preserve their attributes

    ## Warning in bind_rows_(x, .id): Vectorizing 'labelled' elements may not
    ## preserve their attributes

    ## Warning in bind_rows_(x, .id): Vectorizing 'labelled' elements may not
    ## preserve their attributes

    ## Warning in bind_rows_(x, .id): Vectorizing 'labelled' elements may not
    ## preserve their attributes

    ## Warning in bind_rows_(x, .id): Vectorizing 'labelled' elements may not
    ## preserve their attributes

    ## Warning in bind_rows_(x, .id): Vectorizing 'labelled' elements may not
    ## preserve their attributes

    ## Warning in bind_rows_(x, .id): Vectorizing 'labelled' elements may not
    ## preserve their attributes

    ## Warning in bind_rows_(x, .id): Vectorizing 'labelled' elements may not
    ## preserve their attributes

    ## Warning in bind_rows_(x, .id): Vectorizing 'labelled' elements may not
    ## preserve their attributes

    ## Warning in bind_rows_(x, .id): Vectorizing 'labelled' elements may not
    ## preserve their attributes

    ## Warning in bind_rows_(x, .id): Vectorizing 'labelled' elements may not
    ## preserve their attributes

    ## Warning in bind_rows_(x, .id): Vectorizing 'labelled' elements may not
    ## preserve their attributes

    ## Warning in bind_rows_(x, .id): Vectorizing 'labelled' elements may not
    ## preserve their attributes

    ## Warning in bind_rows_(x, .id): Vectorizing 'labelled' elements may not
    ## preserve their attributes

    ## Warning in bind_rows_(x, .id): Vectorizing 'labelled' elements may not
    ## preserve their attributes

    ## Warning in bind_rows_(x, .id): Vectorizing 'labelled' elements may not
    ## preserve their attributes

    ## Warning in bind_rows_(x, .id): Vectorizing 'labelled' elements may not
    ## preserve their attributes

    ## Warning in bind_rows_(x, .id): Vectorizing 'labelled' elements may not
    ## preserve their attributes

    ## Warning in bind_rows_(x, .id): Vectorizing 'labelled' elements may not
    ## preserve their attributes

    ## Warning in bind_rows_(x, .id): Vectorizing 'labelled' elements may not
    ## preserve their attributes

    ## Warning in bind_rows_(x, .id): Vectorizing 'labelled' elements may not
    ## preserve their attributes

    ## Warning in bind_rows_(x, .id): Vectorizing 'labelled' elements may not
    ## preserve their attributes

    ## Warning in bind_rows_(x, .id): Vectorizing 'labelled' elements may not
    ## preserve their attributes

    ## Warning in bind_rows_(x, .id): Vectorizing 'labelled' elements may not
    ## preserve their attributes

    ## Warning in bind_rows_(x, .id): Vectorizing 'labelled' elements may not
    ## preserve their attributes

    ## Warning in bind_rows_(x, .id): Vectorizing 'labelled' elements may not
    ## preserve their attributes

    ## Warning in bind_rows_(x, .id): Vectorizing 'labelled' elements may not
    ## preserve their attributes

    ## Warning in bind_rows_(x, .id): Vectorizing 'labelled' elements may not
    ## preserve their attributes

    ## Warning in bind_rows_(x, .id): Vectorizing 'labelled' elements may not
    ## preserve their attributes

    ## Warning in bind_rows_(x, .id): Vectorizing 'labelled' elements may not
    ## preserve their attributes

    ## Warning in bind_rows_(x, .id): Vectorizing 'labelled' elements may not
    ## preserve their attributes

    ## Warning in bind_rows_(x, .id): Vectorizing 'labelled' elements may not
    ## preserve their attributes

    ## Warning in bind_rows_(x, .id): Vectorizing 'labelled' elements may not
    ## preserve their attributes

    ## Warning in bind_rows_(x, .id): Vectorizing 'labelled' elements may not
    ## preserve their attributes

    ## Warning in bind_rows_(x, .id): Vectorizing 'labelled' elements may not
    ## preserve their attributes

    ## Warning in bind_rows_(x, .id): Vectorizing 'labelled' elements may not
    ## preserve their attributes

    ## Warning in bind_rows_(x, .id): Vectorizing 'labelled' elements may not
    ## preserve their attributes

    ## Warning in bind_rows_(x, .id): Vectorizing 'labelled' elements may not
    ## preserve their attributes

    ## Warning in bind_rows_(x, .id): Vectorizing 'labelled' elements may not
    ## preserve their attributes

    ## Warning in bind_rows_(x, .id): Vectorizing 'labelled' elements may not
    ## preserve their attributes

    ## Warning in bind_rows_(x, .id): Vectorizing 'labelled' elements may not
    ## preserve their attributes

    ## Warning in bind_rows_(x, .id): Vectorizing 'labelled' elements may not
    ## preserve their attributes

    ## Warning in bind_rows_(x, .id): Vectorizing 'labelled' elements may not
    ## preserve their attributes

    ## Warning in bind_rows_(x, .id): Vectorizing 'labelled' elements may not
    ## preserve their attributes

    ## Warning in bind_rows_(x, .id): Vectorizing 'labelled' elements may not
    ## preserve their attributes

    ## Warning in bind_rows_(x, .id): Vectorizing 'labelled' elements may not
    ## preserve their attributes

    ## Warning in bind_rows_(x, .id): Vectorizing 'labelled' elements may not
    ## preserve their attributes

    ## Warning in bind_rows_(x, .id): Vectorizing 'labelled' elements may not
    ## preserve their attributes

    ## Warning in bind_rows_(x, .id): Vectorizing 'labelled' elements may not
    ## preserve their attributes

    ## Warning in bind_rows_(x, .id): Vectorizing 'labelled' elements may not
    ## preserve their attributes

    ## Warning in bind_rows_(x, .id): Vectorizing 'labelled' elements may not
    ## preserve their attributes

    ## Warning in bind_rows_(x, .id): Vectorizing 'labelled' elements may not
    ## preserve their attributes

    ## Warning in bind_rows_(x, .id): Vectorizing 'labelled' elements may not
    ## preserve their attributes

    ## Warning in bind_rows_(x, .id): Vectorizing 'labelled' elements may not
    ## preserve their attributes

    ## Warning in bind_rows_(x, .id): Vectorizing 'labelled' elements may not
    ## preserve their attributes

    ## Warning in bind_rows_(x, .id): Vectorizing 'labelled' elements may not
    ## preserve their attributes

    ## Warning in bind_rows_(x, .id): Vectorizing 'labelled' elements may not
    ## preserve their attributes

    ## Warning in bind_rows_(x, .id): Vectorizing 'labelled' elements may not
    ## preserve their attributes

    ## Warning in bind_rows_(x, .id): Vectorizing 'labelled' elements may not
    ## preserve their attributes

    ## Warning in bind_rows_(x, .id): Vectorizing 'labelled' elements may not
    ## preserve their attributes

    ## Warning in bind_rows_(x, .id): Vectorizing 'labelled' elements may not
    ## preserve their attributes

    ## Warning in bind_rows_(x, .id): Vectorizing 'labelled' elements may not
    ## preserve their attributes

    ## Warning in bind_rows_(x, .id): Vectorizing 'labelled' elements may not
    ## preserve their attributes

    ## Warning in bind_rows_(x, .id): Vectorizing 'labelled' elements may not
    ## preserve their attributes

    ## Warning in bind_rows_(x, .id): Vectorizing 'labelled' elements may not
    ## preserve their attributes

    ## Warning in bind_rows_(x, .id): Vectorizing 'labelled' elements may not
    ## preserve their attributes

    ## Warning in bind_rows_(x, .id): Vectorizing 'labelled' elements may not
    ## preserve their attributes

    ## Warning in bind_rows_(x, .id): Vectorizing 'labelled' elements may not
    ## preserve their attributes

    ## Warning in bind_rows_(x, .id): Vectorizing 'labelled' elements may not
    ## preserve their attributes

    ## Warning in bind_rows_(x, .id): Vectorizing 'labelled' elements may not
    ## preserve their attributes

    ## Warning in bind_rows_(x, .id): Vectorizing 'labelled' elements may not
    ## preserve their attributes

    ## Warning in bind_rows_(x, .id): Vectorizing 'labelled' elements may not
    ## preserve their attributes

    ## Warning in bind_rows_(x, .id): Vectorizing 'labelled' elements may not
    ## preserve their attributes

    ## Warning in bind_rows_(x, .id): Vectorizing 'labelled' elements may not
    ## preserve their attributes

    ## Warning in bind_rows_(x, .id): Vectorizing 'labelled' elements may not
    ## preserve their attributes

    ## Warning in bind_rows_(x, .id): Vectorizing 'labelled' elements may not
    ## preserve their attributes

    ## Warning in bind_rows_(x, .id): Vectorizing 'labelled' elements may not
    ## preserve their attributes

    ## Warning in bind_rows_(x, .id): Vectorizing 'labelled' elements may not
    ## preserve their attributes

    ## Warning in bind_rows_(x, .id): Vectorizing 'labelled' elements may not
    ## preserve their attributes

    ## Warning in bind_rows_(x, .id): Vectorizing 'labelled' elements may not
    ## preserve their attributes

    ## Warning in bind_rows_(x, .id): Vectorizing 'labelled' elements may not
    ## preserve their attributes

    ## Warning in bind_rows_(x, .id): Vectorizing 'labelled' elements may not
    ## preserve their attributes

    ## Warning in bind_rows_(x, .id): Vectorizing 'labelled' elements may not
    ## preserve their attributes

    ## Warning in bind_rows_(x, .id): Vectorizing 'labelled' elements may not
    ## preserve their attributes

    ## Warning in bind_rows_(x, .id): Vectorizing 'labelled' elements may not
    ## preserve their attributes

    ## Warning in bind_rows_(x, .id): Vectorizing 'labelled' elements may not
    ## preserve their attributes

    ## Warning in bind_rows_(x, .id): Vectorizing 'labelled' elements may not
    ## preserve their attributes

    ## Warning in bind_rows_(x, .id): Vectorizing 'labelled' elements may not
    ## preserve their attributes

    ## Warning in bind_rows_(x, .id): Vectorizing 'labelled' elements may not
    ## preserve their attributes

    ## Warning in bind_rows_(x, .id): Vectorizing 'labelled' elements may not
    ## preserve their attributes

    ## Warning in bind_rows_(x, .id): Vectorizing 'labelled' elements may not
    ## preserve their attributes

    ## Warning in bind_rows_(x, .id): Vectorizing 'labelled' elements may not
    ## preserve their attributes

    ## Warning in bind_rows_(x, .id): Vectorizing 'labelled' elements may not
    ## preserve their attributes

    ## Warning in bind_rows_(x, .id): Vectorizing 'labelled' elements may not
    ## preserve their attributes

    ## Warning in bind_rows_(x, .id): Vectorizing 'labelled' elements may not
    ## preserve their attributes

    ## Warning in bind_rows_(x, .id): Vectorizing 'labelled' elements may not
    ## preserve their attributes

    ## Warning in bind_rows_(x, .id): Vectorizing 'labelled' elements may not
    ## preserve their attributes

    ## Warning in bind_rows_(x, .id): Vectorizing 'labelled' elements may not
    ## preserve their attributes

    ## Warning in bind_rows_(x, .id): Vectorizing 'labelled' elements may not
    ## preserve their attributes

    ## Warning in bind_rows_(x, .id): Vectorizing 'labelled' elements may not
    ## preserve their attributes

    ## Warning in bind_rows_(x, .id): Vectorizing 'labelled' elements may not
    ## preserve their attributes

    ## Warning in bind_rows_(x, .id): Vectorizing 'labelled' elements may not
    ## preserve their attributes

    ## Warning in bind_rows_(x, .id): Vectorizing 'labelled' elements may not
    ## preserve their attributes

    ## Warning in bind_rows_(x, .id): Vectorizing 'labelled' elements may not
    ## preserve their attributes

    ## Warning in bind_rows_(x, .id): Vectorizing 'labelled' elements may not
    ## preserve their attributes

    ## Warning in bind_rows_(x, .id): Vectorizing 'labelled' elements may not
    ## preserve their attributes

    ## Warning in bind_rows_(x, .id): Vectorizing 'labelled' elements may not
    ## preserve their attributes

    ## Warning in bind_rows_(x, .id): Vectorizing 'labelled' elements may not
    ## preserve their attributes

    ## Warning in bind_rows_(x, .id): Vectorizing 'labelled' elements may not
    ## preserve their attributes

    ## Warning in bind_rows_(x, .id): Vectorizing 'labelled' elements may not
    ## preserve their attributes

    ## Warning in bind_rows_(x, .id): Vectorizing 'labelled' elements may not
    ## preserve their attributes

    ## Warning in bind_rows_(x, .id): Vectorizing 'labelled' elements may not
    ## preserve their attributes

    ## Warning in bind_rows_(x, .id): Vectorizing 'labelled' elements may not
    ## preserve their attributes

    ## Warning in bind_rows_(x, .id): Vectorizing 'labelled' elements may not
    ## preserve their attributes

    ## Warning in bind_rows_(x, .id): Vectorizing 'labelled' elements may not
    ## preserve their attributes

    ## Warning in bind_rows_(x, .id): Vectorizing 'labelled' elements may not
    ## preserve their attributes

    ## Warning in bind_rows_(x, .id): Vectorizing 'labelled' elements may not
    ## preserve their attributes

    ## Warning in bind_rows_(x, .id): Vectorizing 'labelled' elements may not
    ## preserve their attributes

    ## Warning in bind_rows_(x, .id): Vectorizing 'labelled' elements may not
    ## preserve their attributes

    ## Warning in bind_rows_(x, .id): Vectorizing 'labelled' elements may not
    ## preserve their attributes

    ## Warning in bind_rows_(x, .id): Vectorizing 'labelled' elements may not
    ## preserve their attributes

    ## Warning in bind_rows_(x, .id): Vectorizing 'labelled' elements may not
    ## preserve their attributes

    ## Warning in bind_rows_(x, .id): Vectorizing 'labelled' elements may not
    ## preserve their attributes

    ## Warning in bind_rows_(x, .id): Vectorizing 'labelled' elements may not
    ## preserve their attributes

    ## Warning in bind_rows_(x, .id): Vectorizing 'labelled' elements may not
    ## preserve their attributes

    ## Warning in bind_rows_(x, .id): Vectorizing 'labelled' elements may not
    ## preserve their attributes

    ## Warning in bind_rows_(x, .id): Vectorizing 'labelled' elements may not
    ## preserve their attributes

    ## Warning in bind_rows_(x, .id): Vectorizing 'labelled' elements may not
    ## preserve their attributes

    ## Warning in bind_rows_(x, .id): Vectorizing 'labelled' elements may not
    ## preserve their attributes

    ## Warning in bind_rows_(x, .id): Vectorizing 'labelled' elements may not
    ## preserve their attributes

    ## Warning in bind_rows_(x, .id): Vectorizing 'labelled' elements may not
    ## preserve their attributes

    ## Warning in bind_rows_(x, .id): Vectorizing 'labelled' elements may not
    ## preserve their attributes

    ## Warning in bind_rows_(x, .id): Vectorizing 'labelled' elements may not
    ## preserve their attributes

    ## Warning in bind_rows_(x, .id): Vectorizing 'labelled' elements may not
    ## preserve their attributes

    ## Warning in bind_rows_(x, .id): Vectorizing 'labelled' elements may not
    ## preserve their attributes

    ## Warning in bind_rows_(x, .id): Vectorizing 'labelled' elements may not
    ## preserve their attributes

    ## Warning in bind_rows_(x, .id): Vectorizing 'labelled' elements may not
    ## preserve their attributes

    ## Warning in bind_rows_(x, .id): Vectorizing 'labelled' elements may not
    ## preserve their attributes

    ## Warning in bind_rows_(x, .id): Vectorizing 'labelled' elements may not
    ## preserve their attributes

    ## Warning in bind_rows_(x, .id): Vectorizing 'labelled' elements may not
    ## preserve their attributes

    ## Warning in bind_rows_(x, .id): Vectorizing 'labelled' elements may not
    ## preserve their attributes

    ## Warning in bind_rows_(x, .id): Vectorizing 'labelled' elements may not
    ## preserve their attributes

    ## Warning in bind_rows_(x, .id): Vectorizing 'labelled' elements may not
    ## preserve their attributes

    ## Warning in bind_rows_(x, .id): Vectorizing 'labelled' elements may not
    ## preserve their attributes

    ## Warning in bind_rows_(x, .id): Vectorizing 'labelled' elements may not
    ## preserve their attributes

    ## Warning in bind_rows_(x, .id): Vectorizing 'labelled' elements may not
    ## preserve their attributes

    ## Warning in bind_rows_(x, .id): Vectorizing 'labelled' elements may not
    ## preserve their attributes

    ## Warning in bind_rows_(x, .id): Vectorizing 'labelled' elements may not
    ## preserve their attributes

    ## Warning in bind_rows_(x, .id): Vectorizing 'labelled' elements may not
    ## preserve their attributes

    ## Warning in bind_rows_(x, .id): Vectorizing 'labelled' elements may not
    ## preserve their attributes

    ## Warning in bind_rows_(x, .id): Vectorizing 'labelled' elements may not
    ## preserve their attributes

    ## Warning in bind_rows_(x, .id): Vectorizing 'labelled' elements may not
    ## preserve their attributes

    ## Warning in bind_rows_(x, .id): Vectorizing 'labelled' elements may not
    ## preserve their attributes

    ## Warning in bind_rows_(x, .id): Vectorizing 'labelled' elements may not
    ## preserve their attributes

    ## Warning in bind_rows_(x, .id): Vectorizing 'labelled' elements may not
    ## preserve their attributes

    ## Warning in bind_rows_(x, .id): Vectorizing 'labelled' elements may not
    ## preserve their attributes

    ## Warning in bind_rows_(x, .id): Vectorizing 'labelled' elements may not
    ## preserve their attributes

    ## Warning in bind_rows_(x, .id): Vectorizing 'labelled' elements may not
    ## preserve their attributes

    ## Warning in bind_rows_(x, .id): Vectorizing 'labelled' elements may not
    ## preserve their attributes

    ## Warning in bind_rows_(x, .id): Vectorizing 'labelled' elements may not
    ## preserve their attributes

    ## Warning in bind_rows_(x, .id): Vectorizing 'labelled' elements may not
    ## preserve their attributes

    ## Warning in bind_rows_(x, .id): Vectorizing 'labelled' elements may not
    ## preserve their attributes

    ## Warning in bind_rows_(x, .id): Vectorizing 'labelled' elements may not
    ## preserve their attributes

    ## Warning in bind_rows_(x, .id): Vectorizing 'labelled' elements may not
    ## preserve their attributes

    ## Warning in bind_rows_(x, .id): Vectorizing 'labelled' elements may not
    ## preserve their attributes

    ## Warning in bind_rows_(x, .id): Vectorizing 'labelled' elements may not
    ## preserve their attributes

    ## Warning in bind_rows_(x, .id): Vectorizing 'labelled' elements may not
    ## preserve their attributes

    ## Warning in bind_rows_(x, .id): Vectorizing 'labelled' elements may not
    ## preserve their attributes

    ## Warning in bind_rows_(x, .id): Vectorizing 'labelled' elements may not
    ## preserve their attributes

    ## Warning in bind_rows_(x, .id): Vectorizing 'labelled' elements may not
    ## preserve their attributes

    ## Warning in bind_rows_(x, .id): Vectorizing 'labelled' elements may not
    ## preserve their attributes

    ## Warning in bind_rows_(x, .id): Vectorizing 'labelled' elements may not
    ## preserve their attributes

    ## Warning in bind_rows_(x, .id): Vectorizing 'labelled' elements may not
    ## preserve their attributes

    ## Warning in bind_rows_(x, .id): Vectorizing 'labelled' elements may not
    ## preserve their attributes

    ## Warning in bind_rows_(x, .id): Vectorizing 'labelled' elements may not
    ## preserve their attributes

    ## Warning in bind_rows_(x, .id): Vectorizing 'labelled' elements may not
    ## preserve their attributes

    ## Warning in bind_rows_(x, .id): Vectorizing 'labelled' elements may not
    ## preserve their attributes

    ## Warning in bind_rows_(x, .id): Vectorizing 'labelled' elements may not
    ## preserve their attributes

    ## Warning in bind_rows_(x, .id): Vectorizing 'labelled' elements may not
    ## preserve their attributes

    ## Warning in bind_rows_(x, .id): Vectorizing 'labelled' elements may not
    ## preserve their attributes

    ## Warning in bind_rows_(x, .id): Vectorizing 'labelled' elements may not
    ## preserve their attributes

    ## Warning in bind_rows_(x, .id): Vectorizing 'labelled' elements may not
    ## preserve their attributes

    ## Warning in bind_rows_(x, .id): Vectorizing 'labelled' elements may not
    ## preserve their attributes

    ## Warning in bind_rows_(x, .id): Vectorizing 'labelled' elements may not
    ## preserve their attributes

    ## Warning in bind_rows_(x, .id): Vectorizing 'labelled' elements may not
    ## preserve their attributes

    ## Warning in bind_rows_(x, .id): Vectorizing 'labelled' elements may not
    ## preserve their attributes

    ## Warning in bind_rows_(x, .id): Vectorizing 'labelled' elements may not
    ## preserve their attributes

    ## Warning in bind_rows_(x, .id): Vectorizing 'labelled' elements may not
    ## preserve their attributes

    ## Warning in bind_rows_(x, .id): Vectorizing 'labelled' elements may not
    ## preserve their attributes

    ## Warning in bind_rows_(x, .id): Vectorizing 'labelled' elements may not
    ## preserve their attributes

    ## Warning in bind_rows_(x, .id): Vectorizing 'labelled' elements may not
    ## preserve their attributes

    ## Warning in bind_rows_(x, .id): Vectorizing 'labelled' elements may not
    ## preserve their attributes

    ## Warning in bind_rows_(x, .id): Vectorizing 'labelled' elements may not
    ## preserve their attributes

    ## Warning in bind_rows_(x, .id): Vectorizing 'labelled' elements may not
    ## preserve their attributes

    ## Warning in bind_rows_(x, .id): Vectorizing 'labelled' elements may not
    ## preserve their attributes

    ## Warning in bind_rows_(x, .id): Vectorizing 'labelled' elements may not
    ## preserve their attributes

    ## Warning in bind_rows_(x, .id): Vectorizing 'labelled' elements may not
    ## preserve their attributes

    ## Warning in bind_rows_(x, .id): Vectorizing 'labelled' elements may not
    ## preserve their attributes

    ## Warning in bind_rows_(x, .id): Vectorizing 'labelled' elements may not
    ## preserve their attributes

    ## Warning in bind_rows_(x, .id): Vectorizing 'labelled' elements may not
    ## preserve their attributes

    ## Warning in bind_rows_(x, .id): Vectorizing 'labelled' elements may not
    ## preserve their attributes

    ## Warning in bind_rows_(x, .id): Vectorizing 'labelled' elements may not
    ## preserve their attributes

    ## Warning in bind_rows_(x, .id): Vectorizing 'labelled' elements may not
    ## preserve their attributes

    ## Warning in bind_rows_(x, .id): Vectorizing 'labelled' elements may not
    ## preserve their attributes

    ## Warning in bind_rows_(x, .id): Vectorizing 'labelled' elements may not
    ## preserve their attributes

    ## Warning in bind_rows_(x, .id): Vectorizing 'labelled' elements may not
    ## preserve their attributes

    ## Warning in bind_rows_(x, .id): Vectorizing 'labelled' elements may not
    ## preserve their attributes

    ## Warning in bind_rows_(x, .id): Vectorizing 'labelled' elements may not
    ## preserve their attributes

    ## Warning in bind_rows_(x, .id): Vectorizing 'labelled' elements may not
    ## preserve their attributes

    ## Warning in bind_rows_(x, .id): Vectorizing 'labelled' elements may not
    ## preserve their attributes

    ## Warning in bind_rows_(x, .id): Vectorizing 'labelled' elements may not
    ## preserve their attributes

    ## Warning in bind_rows_(x, .id): Vectorizing 'labelled' elements may not
    ## preserve their attributes

    ## Warning in bind_rows_(x, .id): Vectorizing 'labelled' elements may not
    ## preserve their attributes

    ## Warning in bind_rows_(x, .id): Vectorizing 'labelled' elements may not
    ## preserve their attributes

    ## Warning in bind_rows_(x, .id): Vectorizing 'labelled' elements may not
    ## preserve their attributes

    ## Warning in bind_rows_(x, .id): Vectorizing 'labelled' elements may not
    ## preserve their attributes

    ## Warning in bind_rows_(x, .id): Vectorizing 'labelled' elements may not
    ## preserve their attributes

    ## Warning in bind_rows_(x, .id): Vectorizing 'labelled' elements may not
    ## preserve their attributes

    ## Warning in bind_rows_(x, .id): Vectorizing 'labelled' elements may not
    ## preserve their attributes

    ## Warning in bind_rows_(x, .id): Vectorizing 'labelled' elements may not
    ## preserve their attributes

    ## Warning in bind_rows_(x, .id): Vectorizing 'labelled' elements may not
    ## preserve their attributes

    ## Warning in bind_rows_(x, .id): Vectorizing 'labelled' elements may not
    ## preserve their attributes

    ## Warning in bind_rows_(x, .id): Vectorizing 'labelled' elements may not
    ## preserve their attributes

    ## Warning in bind_rows_(x, .id): Vectorizing 'labelled' elements may not
    ## preserve their attributes

    ## Warning in bind_rows_(x, .id): Vectorizing 'labelled' elements may not
    ## preserve their attributes

    ## Warning in bind_rows_(x, .id): Vectorizing 'labelled' elements may not
    ## preserve their attributes

    ## Warning in bind_rows_(x, .id): Vectorizing 'labelled' elements may not
    ## preserve their attributes

    ## Warning in bind_rows_(x, .id): Vectorizing 'labelled' elements may not
    ## preserve their attributes

    ## Warning in bind_rows_(x, .id): Vectorizing 'labelled' elements may not
    ## preserve their attributes

    ## Warning in bind_rows_(x, .id): Vectorizing 'labelled' elements may not
    ## preserve their attributes

    ## Warning in bind_rows_(x, .id): Vectorizing 'labelled' elements may not
    ## preserve their attributes

    ## Warning in bind_rows_(x, .id): Vectorizing 'labelled' elements may not
    ## preserve their attributes

    ## Warning in bind_rows_(x, .id): Vectorizing 'labelled' elements may not
    ## preserve their attributes

    ## Warning in bind_rows_(x, .id): Vectorizing 'labelled' elements may not
    ## preserve their attributes

    ## Warning in bind_rows_(x, .id): Vectorizing 'labelled' elements may not
    ## preserve their attributes

    ## Warning in bind_rows_(x, .id): Vectorizing 'labelled' elements may not
    ## preserve their attributes

    ## Warning in bind_rows_(x, .id): Vectorizing 'labelled' elements may not
    ## preserve their attributes

    ## Warning in bind_rows_(x, .id): Vectorizing 'labelled' elements may not
    ## preserve their attributes

    ## Warning in bind_rows_(x, .id): Vectorizing 'labelled' elements may not
    ## preserve their attributes

    ## Warning in bind_rows_(x, .id): Vectorizing 'labelled' elements may not
    ## preserve their attributes

    ## Warning in bind_rows_(x, .id): Vectorizing 'labelled' elements may not
    ## preserve their attributes

    ## Warning in bind_rows_(x, .id): Vectorizing 'labelled' elements may not
    ## preserve their attributes

    ## Warning in bind_rows_(x, .id): Vectorizing 'labelled' elements may not
    ## preserve their attributes

    ## Warning in bind_rows_(x, .id): Vectorizing 'labelled' elements may not
    ## preserve their attributes

    ## Warning in bind_rows_(x, .id): Vectorizing 'labelled' elements may not
    ## preserve their attributes

    ## Warning in bind_rows_(x, .id): Vectorizing 'labelled' elements may not
    ## preserve their attributes

    ## Warning in bind_rows_(x, .id): Vectorizing 'labelled' elements may not
    ## preserve their attributes

    ## Warning in bind_rows_(x, .id): Vectorizing 'labelled' elements may not
    ## preserve their attributes

    ## Warning in bind_rows_(x, .id): Vectorizing 'labelled' elements may not
    ## preserve their attributes

    ## Warning in bind_rows_(x, .id): Vectorizing 'labelled' elements may not
    ## preserve their attributes

    ## Warning in bind_rows_(x, .id): Vectorizing 'labelled' elements may not
    ## preserve their attributes

    ## Warning in bind_rows_(x, .id): Vectorizing 'labelled' elements may not
    ## preserve their attributes

    ## Warning in bind_rows_(x, .id): Vectorizing 'labelled' elements may not
    ## preserve their attributes

    ## Warning in bind_rows_(x, .id): Vectorizing 'labelled' elements may not
    ## preserve their attributes

    ## Warning in bind_rows_(x, .id): Vectorizing 'labelled' elements may not
    ## preserve their attributes

    ## Warning in bind_rows_(x, .id): Vectorizing 'labelled' elements may not
    ## preserve their attributes

    ## Warning in bind_rows_(x, .id): Vectorizing 'labelled' elements may not
    ## preserve their attributes

    ## Warning in bind_rows_(x, .id): Vectorizing 'labelled' elements may not
    ## preserve their attributes

    ## Warning in bind_rows_(x, .id): Vectorizing 'labelled' elements may not
    ## preserve their attributes

    ## Warning in bind_rows_(x, .id): Vectorizing 'labelled' elements may not
    ## preserve their attributes

    ## Warning in bind_rows_(x, .id): Vectorizing 'labelled' elements may not
    ## preserve their attributes

    ## Warning in bind_rows_(x, .id): Vectorizing 'labelled' elements may not
    ## preserve their attributes

    ## Warning in bind_rows_(x, .id): Vectorizing 'labelled' elements may not
    ## preserve their attributes

    ## Warning in bind_rows_(x, .id): Vectorizing 'labelled' elements may not
    ## preserve their attributes

    ## Warning in bind_rows_(x, .id): Vectorizing 'labelled' elements may not
    ## preserve their attributes

    ## Warning in bind_rows_(x, .id): Vectorizing 'labelled' elements may not
    ## preserve their attributes

    ## Warning in bind_rows_(x, .id): Vectorizing 'labelled' elements may not
    ## preserve their attributes

    ## Warning in bind_rows_(x, .id): Vectorizing 'labelled' elements may not
    ## preserve their attributes

    ## Warning in bind_rows_(x, .id): Vectorizing 'labelled' elements may not
    ## preserve their attributes

    ## Warning in bind_rows_(x, .id): Vectorizing 'labelled' elements may not
    ## preserve their attributes

    ## Warning in bind_rows_(x, .id): Vectorizing 'labelled' elements may not
    ## preserve their attributes

    ## Warning in bind_rows_(x, .id): Vectorizing 'labelled' elements may not
    ## preserve their attributes

    ## Warning in bind_rows_(x, .id): Vectorizing 'labelled' elements may not
    ## preserve their attributes

    ## Warning in bind_rows_(x, .id): Vectorizing 'labelled' elements may not
    ## preserve their attributes

    ## Warning in bind_rows_(x, .id): Vectorizing 'labelled' elements may not
    ## preserve their attributes

    ## Warning in bind_rows_(x, .id): Vectorizing 'labelled' elements may not
    ## preserve their attributes

    ## Warning in bind_rows_(x, .id): Vectorizing 'labelled' elements may not
    ## preserve their attributes

    ## Warning in bind_rows_(x, .id): Vectorizing 'labelled' elements may not
    ## preserve their attributes

    ## Warning in bind_rows_(x, .id): Vectorizing 'labelled' elements may not
    ## preserve their attributes

    ## Warning in bind_rows_(x, .id): Vectorizing 'labelled' elements may not
    ## preserve their attributes

    ## Warning in bind_rows_(x, .id): Vectorizing 'labelled' elements may not
    ## preserve their attributes

    ## Warning in bind_rows_(x, .id): Vectorizing 'labelled' elements may not
    ## preserve their attributes

    ## Warning in bind_rows_(x, .id): Vectorizing 'labelled' elements may not
    ## preserve their attributes

    ## Warning in bind_rows_(x, .id): Vectorizing 'labelled' elements may not
    ## preserve their attributes

    ## Warning in bind_rows_(x, .id): Vectorizing 'labelled' elements may not
    ## preserve their attributes

    ## Warning in bind_rows_(x, .id): Vectorizing 'labelled' elements may not
    ## preserve their attributes

    ## Warning in bind_rows_(x, .id): Vectorizing 'labelled' elements may not
    ## preserve their attributes

    ## Warning in bind_rows_(x, .id): Vectorizing 'labelled' elements may not
    ## preserve their attributes

    ## Warning in bind_rows_(x, .id): Vectorizing 'labelled' elements may not
    ## preserve their attributes

    ## Warning in bind_rows_(x, .id): Vectorizing 'labelled' elements may not
    ## preserve their attributes

    ## Warning in bind_rows_(x, .id): Vectorizing 'labelled' elements may not
    ## preserve their attributes

    ## Warning in bind_rows_(x, .id): Vectorizing 'labelled' elements may not
    ## preserve their attributes

    ## Warning in bind_rows_(x, .id): Vectorizing 'labelled' elements may not
    ## preserve their attributes

    ## Warning in bind_rows_(x, .id): Vectorizing 'labelled' elements may not
    ## preserve their attributes

    ## Warning in bind_rows_(x, .id): Vectorizing 'labelled' elements may not
    ## preserve their attributes

    ## Warning in bind_rows_(x, .id): Vectorizing 'labelled' elements may not
    ## preserve their attributes

    ## Warning in bind_rows_(x, .id): Vectorizing 'labelled' elements may not
    ## preserve their attributes

    ## Warning in bind_rows_(x, .id): Vectorizing 'labelled' elements may not
    ## preserve their attributes

    ## Warning in bind_rows_(x, .id): Vectorizing 'labelled' elements may not
    ## preserve their attributes

    ## Warning in bind_rows_(x, .id): Vectorizing 'labelled' elements may not
    ## preserve their attributes

    ## Warning in bind_rows_(x, .id): Vectorizing 'labelled' elements may not
    ## preserve their attributes

    ## Warning in bind_rows_(x, .id): Vectorizing 'labelled' elements may not
    ## preserve their attributes

    ## Warning in bind_rows_(x, .id): Vectorizing 'labelled' elements may not
    ## preserve their attributes

    ## Warning in bind_rows_(x, .id): Vectorizing 'labelled' elements may not
    ## preserve their attributes

    ## Warning in bind_rows_(x, .id): Vectorizing 'labelled' elements may not
    ## preserve their attributes

    ## Warning in bind_rows_(x, .id): Vectorizing 'labelled' elements may not
    ## preserve their attributes

    ## Warning in bind_rows_(x, .id): Vectorizing 'labelled' elements may not
    ## preserve their attributes

    ## Warning in bind_rows_(x, .id): Vectorizing 'labelled' elements may not
    ## preserve their attributes

    ## Warning in bind_rows_(x, .id): Vectorizing 'labelled' elements may not
    ## preserve their attributes

    ## Warning in bind_rows_(x, .id): Vectorizing 'labelled' elements may not
    ## preserve their attributes

    ## Warning in bind_rows_(x, .id): Vectorizing 'labelled' elements may not
    ## preserve their attributes

    ## Warning in bind_rows_(x, .id): Vectorizing 'labelled' elements may not
    ## preserve their attributes

    ## Warning in bind_rows_(x, .id): Vectorizing 'labelled' elements may not
    ## preserve their attributes

    ## Warning in bind_rows_(x, .id): Vectorizing 'labelled' elements may not
    ## preserve their attributes

    ## Warning in bind_rows_(x, .id): Vectorizing 'labelled' elements may not
    ## preserve their attributes

    ## Warning in bind_rows_(x, .id): Vectorizing 'labelled' elements may not
    ## preserve their attributes

    ## Warning in bind_rows_(x, .id): Vectorizing 'labelled' elements may not
    ## preserve their attributes

    ## Warning in bind_rows_(x, .id): Vectorizing 'labelled' elements may not
    ## preserve their attributes

    ## Warning in bind_rows_(x, .id): Vectorizing 'labelled' elements may not
    ## preserve their attributes

    ## Warning in bind_rows_(x, .id): Vectorizing 'labelled' elements may not
    ## preserve their attributes

    ## Warning in bind_rows_(x, .id): Vectorizing 'labelled' elements may not
    ## preserve their attributes

    ## Warning in bind_rows_(x, .id): Vectorizing 'labelled' elements may not
    ## preserve their attributes

    ## Warning in bind_rows_(x, .id): Vectorizing 'labelled' elements may not
    ## preserve their attributes

    ## Warning in bind_rows_(x, .id): Vectorizing 'labelled' elements may not
    ## preserve their attributes

    ## Warning in bind_rows_(x, .id): Vectorizing 'labelled' elements may not
    ## preserve their attributes

    ## Warning in bind_rows_(x, .id): Vectorizing 'labelled' elements may not
    ## preserve their attributes

    ## Warning in bind_rows_(x, .id): Vectorizing 'labelled' elements may not
    ## preserve their attributes

    ## Warning in bind_rows_(x, .id): Unequal factor levels: coercing to character

    ## Warning in bind_rows_(x, .id): binding character and factor vector,
    ## coercing into character vector

    ## Warning in bind_rows_(x, .id): binding character and factor vector,
    ## coercing into character vector

    ## Warning in bind_rows_(x, .id): Vectorizing 'labelled' elements may not
    ## preserve their attributes

    ## Warning in bind_rows_(x, .id): Vectorizing 'labelled' elements may not
    ## preserve their attributes

    ## Warning in bind_rows_(x, .id): Vectorizing 'labelled' elements may not
    ## preserve their attributes

    ## Warning in bind_rows_(x, .id): Vectorizing 'labelled' elements may not
    ## preserve their attributes

    ## Warning in bind_rows_(x, .id): Vectorizing 'labelled' elements may not
    ## preserve their attributes

    ## Warning in bind_rows_(x, .id): Vectorizing 'labelled' elements may not
    ## preserve their attributes

    ## Warning in bind_rows_(x, .id): Vectorizing 'labelled' elements may not
    ## preserve their attributes

    ## Warning in bind_rows_(x, .id): Vectorizing 'labelled' elements may not
    ## preserve their attributes

    ## Warning in bind_rows_(x, .id): Vectorizing 'labelled' elements may not
    ## preserve their attributes

    ## Warning in bind_rows_(x, .id): Vectorizing 'labelled' elements may not
    ## preserve their attributes

    ## Warning in bind_rows_(x, .id): Vectorizing 'labelled' elements may not
    ## preserve their attributes

    ## Warning in bind_rows_(x, .id): Vectorizing 'labelled' elements may not
    ## preserve their attributes

    ## Warning in bind_rows_(x, .id): Vectorizing 'labelled' elements may not
    ## preserve their attributes

    ## Warning in bind_rows_(x, .id): Vectorizing 'labelled' elements may not
    ## preserve their attributes

    ## Warning in bind_rows_(x, .id): Vectorizing 'labelled' elements may not
    ## preserve their attributes

    ## Warning in bind_rows_(x, .id): Vectorizing 'labelled' elements may not
    ## preserve their attributes

    ## Warning in bind_rows_(x, .id): Vectorizing 'labelled' elements may not
    ## preserve their attributes

    ## Warning in bind_rows_(x, .id): Vectorizing 'labelled' elements may not
    ## preserve their attributes

    ## Warning in bind_rows_(x, .id): Vectorizing 'labelled' elements may not
    ## preserve their attributes

    ## Warning in bind_rows_(x, .id): Vectorizing 'labelled' elements may not
    ## preserve their attributes

    ## Warning in bind_rows_(x, .id): Vectorizing 'labelled' elements may not
    ## preserve their attributes

    ## Warning in bind_rows_(x, .id): Vectorizing 'labelled' elements may not
    ## preserve their attributes

    ## Warning in bind_rows_(x, .id): Vectorizing 'labelled' elements may not
    ## preserve their attributes

    ## Warning in bind_rows_(x, .id): Vectorizing 'labelled' elements may not
    ## preserve their attributes

    ## Warning in bind_rows_(x, .id): Vectorizing 'labelled' elements may not
    ## preserve their attributes

    ## Warning in bind_rows_(x, .id): Vectorizing 'labelled' elements may not
    ## preserve their attributes

    ## Warning in bind_rows_(x, .id): Vectorizing 'labelled' elements may not
    ## preserve their attributes

    ## Warning in bind_rows_(x, .id): Vectorizing 'labelled' elements may not
    ## preserve their attributes

    ## Warning in bind_rows_(x, .id): Vectorizing 'labelled' elements may not
    ## preserve their attributes

    ## Warning in bind_rows_(x, .id): Vectorizing 'labelled' elements may not
    ## preserve their attributes

    ## Warning in bind_rows_(x, .id): Vectorizing 'labelled' elements may not
    ## preserve their attributes

    ## Warning in bind_rows_(x, .id): Vectorizing 'labelled' elements may not
    ## preserve their attributes

    ## Warning in bind_rows_(x, .id): Vectorizing 'labelled' elements may not
    ## preserve their attributes

    ## Warning in bind_rows_(x, .id): Vectorizing 'labelled' elements may not
    ## preserve their attributes

    ## Warning in bind_rows_(x, .id): Vectorizing 'labelled' elements may not
    ## preserve their attributes

    ## Warning in bind_rows_(x, .id): Vectorizing 'labelled' elements may not
    ## preserve their attributes

    ## Warning in bind_rows_(x, .id): Vectorizing 'labelled' elements may not
    ## preserve their attributes

    ## Warning in bind_rows_(x, .id): Vectorizing 'labelled' elements may not
    ## preserve their attributes

    ## Warning in bind_rows_(x, .id): Vectorizing 'labelled' elements may not
    ## preserve their attributes

    ## Warning in bind_rows_(x, .id): Vectorizing 'labelled' elements may not
    ## preserve their attributes

    ## Warning in bind_rows_(x, .id): Vectorizing 'labelled' elements may not
    ## preserve their attributes

    ## Warning in bind_rows_(x, .id): Vectorizing 'labelled' elements may not
    ## preserve their attributes

    ## Warning in bind_rows_(x, .id): Vectorizing 'labelled' elements may not
    ## preserve their attributes

    ## Warning in bind_rows_(x, .id): Vectorizing 'labelled' elements may not
    ## preserve their attributes

    ## Warning in bind_rows_(x, .id): Vectorizing 'labelled' elements may not
    ## preserve their attributes

    ## Warning in bind_rows_(x, .id): Vectorizing 'labelled' elements may not
    ## preserve their attributes

    ## Warning in bind_rows_(x, .id): Vectorizing 'labelled' elements may not
    ## preserve their attributes

    ## Warning in bind_rows_(x, .id): Vectorizing 'labelled' elements may not
    ## preserve their attributes

    ## Warning in bind_rows_(x, .id): Vectorizing 'labelled' elements may not
    ## preserve their attributes

    ## Warning in bind_rows_(x, .id): Vectorizing 'labelled' elements may not
    ## preserve their attributes

    ## Warning in bind_rows_(x, .id): Vectorizing 'labelled' elements may not
    ## preserve their attributes

    ## Warning in bind_rows_(x, .id): Vectorizing 'labelled' elements may not
    ## preserve their attributes

    ## Warning in bind_rows_(x, .id): Vectorizing 'labelled' elements may not
    ## preserve their attributes

    ## Warning in bind_rows_(x, .id): Vectorizing 'labelled' elements may not
    ## preserve their attributes

    ## Warning in bind_rows_(x, .id): Vectorizing 'labelled' elements may not
    ## preserve their attributes

    ## Warning in bind_rows_(x, .id): Vectorizing 'labelled' elements may not
    ## preserve their attributes

    ## Warning in bind_rows_(x, .id): Vectorizing 'labelled' elements may not
    ## preserve their attributes

    ## Warning in bind_rows_(x, .id): Vectorizing 'labelled' elements may not
    ## preserve their attributes

    ## Warning in bind_rows_(x, .id): Vectorizing 'labelled' elements may not
    ## preserve their attributes

    ## Warning in bind_rows_(x, .id): Vectorizing 'labelled' elements may not
    ## preserve their attributes

    ## Warning in bind_rows_(x, .id): Vectorizing 'labelled' elements may not
    ## preserve their attributes

    ## Warning in bind_rows_(x, .id): Vectorizing 'labelled' elements may not
    ## preserve their attributes

    ## Warning in bind_rows_(x, .id): Vectorizing 'labelled' elements may not
    ## preserve their attributes

    ## Warning in bind_rows_(x, .id): Vectorizing 'labelled' elements may not
    ## preserve their attributes

    ## Warning in bind_rows_(x, .id): Vectorizing 'labelled' elements may not
    ## preserve their attributes

    ## Warning in bind_rows_(x, .id): Vectorizing 'labelled' elements may not
    ## preserve their attributes

    ## Warning in bind_rows_(x, .id): Vectorizing 'labelled' elements may not
    ## preserve their attributes

    ## Warning in bind_rows_(x, .id): Vectorizing 'labelled' elements may not
    ## preserve their attributes

    ## Warning in bind_rows_(x, .id): Vectorizing 'labelled' elements may not
    ## preserve their attributes

    ## Warning in bind_rows_(x, .id): Vectorizing 'labelled' elements may not
    ## preserve their attributes

    ## Warning in bind_rows_(x, .id): Vectorizing 'labelled' elements may not
    ## preserve their attributes

    ## Warning in bind_rows_(x, .id): Vectorizing 'labelled' elements may not
    ## preserve their attributes

    ## Warning in bind_rows_(x, .id): Vectorizing 'labelled' elements may not
    ## preserve their attributes

    ## Warning in bind_rows_(x, .id): Vectorizing 'labelled' elements may not
    ## preserve their attributes

    ## Warning in bind_rows_(x, .id): Vectorizing 'labelled' elements may not
    ## preserve their attributes

    ## Warning in bind_rows_(x, .id): Vectorizing 'labelled' elements may not
    ## preserve their attributes

    ## Warning in bind_rows_(x, .id): Vectorizing 'labelled' elements may not
    ## preserve their attributes

    ## Warning in bind_rows_(x, .id): Vectorizing 'labelled' elements may not
    ## preserve their attributes

    ## Warning in bind_rows_(x, .id): Vectorizing 'labelled' elements may not
    ## preserve their attributes

    ## Warning in bind_rows_(x, .id): Vectorizing 'labelled' elements may not
    ## preserve their attributes

    ## Warning in bind_rows_(x, .id): Vectorizing 'labelled' elements may not
    ## preserve their attributes

    ## Warning in bind_rows_(x, .id): Vectorizing 'labelled' elements may not
    ## preserve their attributes

    ## Warning in bind_rows_(x, .id): Vectorizing 'labelled' elements may not
    ## preserve their attributes

    ## Warning in bind_rows_(x, .id): Vectorizing 'labelled' elements may not
    ## preserve their attributes

    ## Warning in bind_rows_(x, .id): Vectorizing 'labelled' elements may not
    ## preserve their attributes

    ## Warning in bind_rows_(x, .id): Vectorizing 'labelled' elements may not
    ## preserve their attributes

    ## Warning in bind_rows_(x, .id): Vectorizing 'labelled' elements may not
    ## preserve their attributes

    ## Warning in bind_rows_(x, .id): Vectorizing 'labelled' elements may not
    ## preserve their attributes

    ## Warning in bind_rows_(x, .id): Vectorizing 'labelled' elements may not
    ## preserve their attributes

    ## Warning in bind_rows_(x, .id): Vectorizing 'labelled' elements may not
    ## preserve their attributes

    ## Warning in bind_rows_(x, .id): Vectorizing 'labelled' elements may not
    ## preserve their attributes

    ## Warning in bind_rows_(x, .id): Vectorizing 'labelled' elements may not
    ## preserve their attributes

    ## Warning in bind_rows_(x, .id): Vectorizing 'labelled' elements may not
    ## preserve their attributes

    ## Warning in bind_rows_(x, .id): Vectorizing 'labelled' elements may not
    ## preserve their attributes

    ## Warning in bind_rows_(x, .id): Vectorizing 'labelled' elements may not
    ## preserve their attributes

    ## Warning in bind_rows_(x, .id): Vectorizing 'labelled' elements may not
    ## preserve their attributes

    ## Warning in bind_rows_(x, .id): Vectorizing 'labelled' elements may not
    ## preserve their attributes

    ## Warning in bind_rows_(x, .id): Vectorizing 'labelled' elements may not
    ## preserve their attributes

    ## Warning in bind_rows_(x, .id): Vectorizing 'labelled' elements may not
    ## preserve their attributes

    ## Warning in bind_rows_(x, .id): Vectorizing 'labelled' elements may not
    ## preserve their attributes

    ## Warning in bind_rows_(x, .id): Vectorizing 'labelled' elements may not
    ## preserve their attributes

    ## Warning in bind_rows_(x, .id): Vectorizing 'labelled' elements may not
    ## preserve their attributes

    ## Warning in bind_rows_(x, .id): Vectorizing 'labelled' elements may not
    ## preserve their attributes

    ## Warning in bind_rows_(x, .id): Vectorizing 'labelled' elements may not
    ## preserve their attributes

    ## Warning in bind_rows_(x, .id): Vectorizing 'labelled' elements may not
    ## preserve their attributes

    ## Warning in bind_rows_(x, .id): Vectorizing 'labelled' elements may not
    ## preserve their attributes

    ## Warning in bind_rows_(x, .id): Vectorizing 'labelled' elements may not
    ## preserve their attributes

    ## Warning in bind_rows_(x, .id): Vectorizing 'labelled' elements may not
    ## preserve their attributes

    ## Warning in bind_rows_(x, .id): Vectorizing 'labelled' elements may not
    ## preserve their attributes

    ## Warning in bind_rows_(x, .id): Vectorizing 'labelled' elements may not
    ## preserve their attributes

    ## Warning in bind_rows_(x, .id): Vectorizing 'labelled' elements may not
    ## preserve their attributes

    ## Warning in bind_rows_(x, .id): Vectorizing 'labelled' elements may not
    ## preserve their attributes

    ## Warning in bind_rows_(x, .id): Vectorizing 'labelled' elements may not
    ## preserve their attributes

    ## Warning in bind_rows_(x, .id): Vectorizing 'labelled' elements may not
    ## preserve their attributes

    ## Warning in bind_rows_(x, .id): Vectorizing 'labelled' elements may not
    ## preserve their attributes

    ## Warning in bind_rows_(x, .id): Vectorizing 'labelled' elements may not
    ## preserve their attributes

    ## Warning in bind_rows_(x, .id): Vectorizing 'labelled' elements may not
    ## preserve their attributes

    ## Warning in bind_rows_(x, .id): Vectorizing 'labelled' elements may not
    ## preserve their attributes

    ## Warning in bind_rows_(x, .id): Vectorizing 'labelled' elements may not
    ## preserve their attributes

    ## Warning in bind_rows_(x, .id): Vectorizing 'labelled' elements may not
    ## preserve their attributes

    ## Warning in bind_rows_(x, .id): Vectorizing 'labelled' elements may not
    ## preserve their attributes

    ## Warning in bind_rows_(x, .id): Vectorizing 'labelled' elements may not
    ## preserve their attributes

    ## Warning in bind_rows_(x, .id): Vectorizing 'labelled' elements may not
    ## preserve their attributes

    ## Warning in bind_rows_(x, .id): Vectorizing 'labelled' elements may not
    ## preserve their attributes

    ## Warning in bind_rows_(x, .id): Vectorizing 'labelled' elements may not
    ## preserve their attributes

    ## Warning in bind_rows_(x, .id): Vectorizing 'labelled' elements may not
    ## preserve their attributes

    ## Warning in bind_rows_(x, .id): Vectorizing 'labelled' elements may not
    ## preserve their attributes

    ## Warning in bind_rows_(x, .id): Vectorizing 'labelled' elements may not
    ## preserve their attributes

    ## Warning in bind_rows_(x, .id): Vectorizing 'labelled' elements may not
    ## preserve their attributes

    ## Warning in bind_rows_(x, .id): Vectorizing 'labelled' elements may not
    ## preserve their attributes

    ## Warning in bind_rows_(x, .id): Vectorizing 'labelled' elements may not
    ## preserve their attributes

    ## Warning in bind_rows_(x, .id): Vectorizing 'labelled' elements may not
    ## preserve their attributes

    ## Warning in bind_rows_(x, .id): Vectorizing 'labelled' elements may not
    ## preserve their attributes

    ## Warning in bind_rows_(x, .id): Vectorizing 'labelled' elements may not
    ## preserve their attributes

    ## Warning in bind_rows_(x, .id): Vectorizing 'labelled' elements may not
    ## preserve their attributes

    ## Warning in bind_rows_(x, .id): Vectorizing 'labelled' elements may not
    ## preserve their attributes

    ## Warning in bind_rows_(x, .id): Vectorizing 'labelled' elements may not
    ## preserve their attributes

    ## Warning in bind_rows_(x, .id): Vectorizing 'labelled' elements may not
    ## preserve their attributes

    ## Warning in bind_rows_(x, .id): Vectorizing 'labelled' elements may not
    ## preserve their attributes

    ## Warning in bind_rows_(x, .id): Vectorizing 'labelled' elements may not
    ## preserve their attributes

    ## Warning in bind_rows_(x, .id): Vectorizing 'labelled' elements may not
    ## preserve their attributes

    ## Warning in bind_rows_(x, .id): Vectorizing 'labelled' elements may not
    ## preserve their attributes

    ## Warning in bind_rows_(x, .id): Vectorizing 'labelled' elements may not
    ## preserve their attributes

    ## Warning in bind_rows_(x, .id): Vectorizing 'labelled' elements may not
    ## preserve their attributes

    ## Warning in bind_rows_(x, .id): Vectorizing 'labelled' elements may not
    ## preserve their attributes

    ## Warning in bind_rows_(x, .id): Vectorizing 'labelled' elements may not
    ## preserve their attributes

    ## Warning in bind_rows_(x, .id): Vectorizing 'labelled' elements may not
    ## preserve their attributes

    ## Warning in bind_rows_(x, .id): Vectorizing 'labelled' elements may not
    ## preserve their attributes

    ## Warning in bind_rows_(x, .id): Vectorizing 'labelled' elements may not
    ## preserve their attributes

    ## Warning in bind_rows_(x, .id): Vectorizing 'labelled' elements may not
    ## preserve their attributes

    ## Warning in bind_rows_(x, .id): Vectorizing 'labelled' elements may not
    ## preserve their attributes

    ## Warning in bind_rows_(x, .id): Vectorizing 'labelled' elements may not
    ## preserve their attributes

    ## Warning in bind_rows_(x, .id): Vectorizing 'labelled' elements may not
    ## preserve their attributes

    ## Warning in bind_rows_(x, .id): Vectorizing 'labelled' elements may not
    ## preserve their attributes

    ## Warning in bind_rows_(x, .id): Vectorizing 'labelled' elements may not
    ## preserve their attributes

    ## Warning in bind_rows_(x, .id): Vectorizing 'labelled' elements may not
    ## preserve their attributes

    ## Warning in bind_rows_(x, .id): Vectorizing 'labelled' elements may not
    ## preserve their attributes

    ## Warning in bind_rows_(x, .id): Vectorizing 'labelled' elements may not
    ## preserve their attributes

    ## Warning in bind_rows_(x, .id): Vectorizing 'labelled' elements may not
    ## preserve their attributes

    ## Warning in bind_rows_(x, .id): Vectorizing 'labelled' elements may not
    ## preserve their attributes

    ## Warning in bind_rows_(x, .id): Vectorizing 'labelled' elements may not
    ## preserve their attributes

    ## Warning in bind_rows_(x, .id): Vectorizing 'labelled' elements may not
    ## preserve their attributes

    ## Warning in bind_rows_(x, .id): Vectorizing 'labelled' elements may not
    ## preserve their attributes

    ## Warning in bind_rows_(x, .id): Vectorizing 'labelled' elements may not
    ## preserve their attributes

    ## Warning in bind_rows_(x, .id): Vectorizing 'labelled' elements may not
    ## preserve their attributes

    ## Warning in bind_rows_(x, .id): Vectorizing 'labelled' elements may not
    ## preserve their attributes

    ## Warning in bind_rows_(x, .id): Vectorizing 'labelled' elements may not
    ## preserve their attributes

    ## Warning in bind_rows_(x, .id): Vectorizing 'labelled' elements may not
    ## preserve their attributes

    ## Warning in bind_rows_(x, .id): Vectorizing 'labelled' elements may not
    ## preserve their attributes

    ## Warning in bind_rows_(x, .id): Vectorizing 'labelled' elements may not
    ## preserve their attributes

    ## Warning in bind_rows_(x, .id): Vectorizing 'labelled' elements may not
    ## preserve their attributes

    ## Warning in bind_rows_(x, .id): Vectorizing 'labelled' elements may not
    ## preserve their attributes

    ## Warning in bind_rows_(x, .id): Vectorizing 'labelled' elements may not
    ## preserve their attributes

    ## Warning in bind_rows_(x, .id): Vectorizing 'labelled' elements may not
    ## preserve their attributes

    ## Warning in bind_rows_(x, .id): Vectorizing 'labelled' elements may not
    ## preserve their attributes

    ## Warning in bind_rows_(x, .id): Vectorizing 'labelled' elements may not
    ## preserve their attributes

    ## Warning in bind_rows_(x, .id): Vectorizing 'labelled' elements may not
    ## preserve their attributes

    ## Warning in bind_rows_(x, .id): Vectorizing 'labelled' elements may not
    ## preserve their attributes

    ## Warning in bind_rows_(x, .id): Vectorizing 'labelled' elements may not
    ## preserve their attributes

    ## Warning in bind_rows_(x, .id): Vectorizing 'labelled' elements may not
    ## preserve their attributes

    ## Warning in bind_rows_(x, .id): Vectorizing 'labelled' elements may not
    ## preserve their attributes

    ## Warning in bind_rows_(x, .id): Vectorizing 'labelled' elements may not
    ## preserve their attributes

    ## Warning in bind_rows_(x, .id): Vectorizing 'labelled' elements may not
    ## preserve their attributes

    ## Warning in bind_rows_(x, .id): Vectorizing 'labelled' elements may not
    ## preserve their attributes

    ## Warning in bind_rows_(x, .id): Vectorizing 'labelled' elements may not
    ## preserve their attributes

    ## Warning in bind_rows_(x, .id): Vectorizing 'labelled' elements may not
    ## preserve their attributes

    ## Warning in bind_rows_(x, .id): Vectorizing 'labelled' elements may not
    ## preserve their attributes

    ## Warning in bind_rows_(x, .id): Vectorizing 'labelled' elements may not
    ## preserve their attributes

    ## Warning in bind_rows_(x, .id): Vectorizing 'labelled' elements may not
    ## preserve their attributes

    ## Warning in bind_rows_(x, .id): Vectorizing 'labelled' elements may not
    ## preserve their attributes

    ## Warning in bind_rows_(x, .id): Vectorizing 'labelled' elements may not
    ## preserve their attributes

    ## Warning in bind_rows_(x, .id): Vectorizing 'labelled' elements may not
    ## preserve their attributes

    ## Warning in bind_rows_(x, .id): Vectorizing 'labelled' elements may not
    ## preserve their attributes

    ## Warning in bind_rows_(x, .id): Vectorizing 'labelled' elements may not
    ## preserve their attributes

    ## Warning in bind_rows_(x, .id): Vectorizing 'labelled' elements may not
    ## preserve their attributes

    ## Warning in bind_rows_(x, .id): Vectorizing 'labelled' elements may not
    ## preserve their attributes

    ## Warning in bind_rows_(x, .id): Vectorizing 'labelled' elements may not
    ## preserve their attributes

    ## Warning in bind_rows_(x, .id): Vectorizing 'labelled' elements may not
    ## preserve their attributes

    ## Warning in bind_rows_(x, .id): Vectorizing 'labelled' elements may not
    ## preserve their attributes

    ## Warning in bind_rows_(x, .id): Vectorizing 'labelled' elements may not
    ## preserve their attributes

    ## Warning in bind_rows_(x, .id): Vectorizing 'labelled' elements may not
    ## preserve their attributes

    ## Warning in bind_rows_(x, .id): Vectorizing 'labelled' elements may not
    ## preserve their attributes

    ## Warning in bind_rows_(x, .id): Vectorizing 'labelled' elements may not
    ## preserve their attributes

    ## Warning in bind_rows_(x, .id): Vectorizing 'labelled' elements may not
    ## preserve their attributes

    ## Warning in bind_rows_(x, .id): Vectorizing 'labelled' elements may not
    ## preserve their attributes

    ## Warning in bind_rows_(x, .id): Vectorizing 'labelled' elements may not
    ## preserve their attributes

    ## Warning in bind_rows_(x, .id): Vectorizing 'labelled' elements may not
    ## preserve their attributes

    ## Warning in bind_rows_(x, .id): Vectorizing 'labelled' elements may not
    ## preserve their attributes

    ## Warning in bind_rows_(x, .id): Vectorizing 'labelled' elements may not
    ## preserve their attributes

    ## Warning in bind_rows_(x, .id): Vectorizing 'labelled' elements may not
    ## preserve their attributes

    ## Warning in bind_rows_(x, .id): Vectorizing 'labelled' elements may not
    ## preserve their attributes

    ## Warning in bind_rows_(x, .id): Vectorizing 'labelled' elements may not
    ## preserve their attributes

    ## Warning in bind_rows_(x, .id): Vectorizing 'labelled' elements may not
    ## preserve their attributes

    ## Warning in bind_rows_(x, .id): Vectorizing 'labelled' elements may not
    ## preserve their attributes

    ## Warning in bind_rows_(x, .id): Vectorizing 'labelled' elements may not
    ## preserve their attributes

    ## Warning in bind_rows_(x, .id): Vectorizing 'labelled' elements may not
    ## preserve their attributes

    ## Warning in bind_rows_(x, .id): Vectorizing 'labelled' elements may not
    ## preserve their attributes

    ## Warning in bind_rows_(x, .id): Vectorizing 'labelled' elements may not
    ## preserve their attributes

    ## Warning in bind_rows_(x, .id): Vectorizing 'labelled' elements may not
    ## preserve their attributes

    ## Warning in bind_rows_(x, .id): Vectorizing 'labelled' elements may not
    ## preserve their attributes

    ## Warning in bind_rows_(x, .id): Vectorizing 'labelled' elements may not
    ## preserve their attributes

    ## Warning in bind_rows_(x, .id): Vectorizing 'labelled' elements may not
    ## preserve their attributes

    ## Warning in bind_rows_(x, .id): Vectorizing 'labelled' elements may not
    ## preserve their attributes

    ## Warning in bind_rows_(x, .id): Vectorizing 'labelled' elements may not
    ## preserve their attributes

    ## Warning in bind_rows_(x, .id): Vectorizing 'labelled' elements may not
    ## preserve their attributes

    ## Warning in bind_rows_(x, .id): Vectorizing 'labelled' elements may not
    ## preserve their attributes

    ## Warning in bind_rows_(x, .id): Vectorizing 'labelled' elements may not
    ## preserve their attributes

    ## Warning in bind_rows_(x, .id): Vectorizing 'labelled' elements may not
    ## preserve their attributes

    ## Warning in bind_rows_(x, .id): Vectorizing 'labelled' elements may not
    ## preserve their attributes

    ## Warning in bind_rows_(x, .id): Vectorizing 'labelled' elements may not
    ## preserve their attributes

    ## Warning in bind_rows_(x, .id): Vectorizing 'labelled' elements may not
    ## preserve their attributes

    ## Warning in bind_rows_(x, .id): Vectorizing 'labelled' elements may not
    ## preserve their attributes

    ## Warning in bind_rows_(x, .id): Vectorizing 'labelled' elements may not
    ## preserve their attributes

    ## Warning in bind_rows_(x, .id): Vectorizing 'labelled' elements may not
    ## preserve their attributes

    ## Warning in bind_rows_(x, .id): Vectorizing 'labelled' elements may not
    ## preserve their attributes

    ## Warning in bind_rows_(x, .id): Vectorizing 'labelled' elements may not
    ## preserve their attributes

    ## Warning in bind_rows_(x, .id): Vectorizing 'labelled' elements may not
    ## preserve their attributes

    ## Warning in bind_rows_(x, .id): Vectorizing 'labelled' elements may not
    ## preserve their attributes

    ## Warning in bind_rows_(x, .id): Vectorizing 'labelled' elements may not
    ## preserve their attributes

    ## Warning in bind_rows_(x, .id): Vectorizing 'labelled' elements may not
    ## preserve their attributes

    ## Warning in bind_rows_(x, .id): Vectorizing 'labelled' elements may not
    ## preserve their attributes

    ## Warning in bind_rows_(x, .id): Vectorizing 'labelled' elements may not
    ## preserve their attributes

    ## Warning in bind_rows_(x, .id): Vectorizing 'labelled' elements may not
    ## preserve their attributes

    ## Warning in bind_rows_(x, .id): Vectorizing 'labelled' elements may not
    ## preserve their attributes

    ## Warning in bind_rows_(x, .id): Vectorizing 'labelled' elements may not
    ## preserve their attributes

    ## Warning in bind_rows_(x, .id): Vectorizing 'labelled' elements may not
    ## preserve their attributes

    ## Warning in bind_rows_(x, .id): Vectorizing 'labelled' elements may not
    ## preserve their attributes

    ## Warning in bind_rows_(x, .id): Vectorizing 'labelled' elements may not
    ## preserve their attributes

    ## Warning in bind_rows_(x, .id): Vectorizing 'labelled' elements may not
    ## preserve their attributes

    ## Warning in bind_rows_(x, .id): Vectorizing 'labelled' elements may not
    ## preserve their attributes

    ## Warning in bind_rows_(x, .id): Vectorizing 'labelled' elements may not
    ## preserve their attributes

    ## Warning in bind_rows_(x, .id): Vectorizing 'labelled' elements may not
    ## preserve their attributes

    ## Warning in bind_rows_(x, .id): Vectorizing 'labelled' elements may not
    ## preserve their attributes

    ## Warning in bind_rows_(x, .id): Vectorizing 'labelled' elements may not
    ## preserve their attributes

    ## Warning in bind_rows_(x, .id): Vectorizing 'labelled' elements may not
    ## preserve their attributes

    ## Warning in bind_rows_(x, .id): Vectorizing 'labelled' elements may not
    ## preserve their attributes

    ## Warning in bind_rows_(x, .id): Vectorizing 'labelled' elements may not
    ## preserve their attributes

    ## Warning in bind_rows_(x, .id): Vectorizing 'labelled' elements may not
    ## preserve their attributes

    ## Warning in bind_rows_(x, .id): Vectorizing 'labelled' elements may not
    ## preserve their attributes

    ## Warning in bind_rows_(x, .id): Vectorizing 'labelled' elements may not
    ## preserve their attributes

    ## Warning in bind_rows_(x, .id): Vectorizing 'labelled' elements may not
    ## preserve their attributes

    ## Warning in bind_rows_(x, .id): Vectorizing 'labelled' elements may not
    ## preserve their attributes

    ## Warning in bind_rows_(x, .id): Vectorizing 'labelled' elements may not
    ## preserve their attributes

    ## Warning in bind_rows_(x, .id): Vectorizing 'labelled' elements may not
    ## preserve their attributes

    ## Warning in bind_rows_(x, .id): Vectorizing 'labelled' elements may not
    ## preserve their attributes

    ## Warning in bind_rows_(x, .id): Vectorizing 'labelled' elements may not
    ## preserve their attributes

    ## Warning in bind_rows_(x, .id): Vectorizing 'labelled' elements may not
    ## preserve their attributes

    ## Warning in bind_rows_(x, .id): Vectorizing 'labelled' elements may not
    ## preserve their attributes

    ## Warning in bind_rows_(x, .id): Vectorizing 'labelled' elements may not
    ## preserve their attributes

    ## Warning in bind_rows_(x, .id): Vectorizing 'labelled' elements may not
    ## preserve their attributes

    ## Warning in bind_rows_(x, .id): Vectorizing 'labelled' elements may not
    ## preserve their attributes

    ## Warning in bind_rows_(x, .id): Vectorizing 'labelled' elements may not
    ## preserve their attributes

    ## Warning in bind_rows_(x, .id): Vectorizing 'labelled' elements may not
    ## preserve their attributes

    ## Warning in bind_rows_(x, .id): Vectorizing 'labelled' elements may not
    ## preserve their attributes

    ## Warning in bind_rows_(x, .id): Vectorizing 'labelled' elements may not
    ## preserve their attributes

    ## Warning in bind_rows_(x, .id): Vectorizing 'labelled' elements may not
    ## preserve their attributes

    ## Warning in bind_rows_(x, .id): Vectorizing 'labelled' elements may not
    ## preserve their attributes

    ## Warning in bind_rows_(x, .id): Vectorizing 'labelled' elements may not
    ## preserve their attributes

    ## Warning in bind_rows_(x, .id): Vectorizing 'labelled' elements may not
    ## preserve their attributes

    ## Warning in bind_rows_(x, .id): Vectorizing 'labelled' elements may not
    ## preserve their attributes

    ## Warning in bind_rows_(x, .id): Vectorizing 'labelled' elements may not
    ## preserve their attributes

    ## Warning in bind_rows_(x, .id): Vectorizing 'labelled' elements may not
    ## preserve their attributes

    ## Warning in bind_rows_(x, .id): Vectorizing 'labelled' elements may not
    ## preserve their attributes

    ## Warning in bind_rows_(x, .id): Vectorizing 'labelled' elements may not
    ## preserve their attributes

    ## Warning in bind_rows_(x, .id): Vectorizing 'labelled' elements may not
    ## preserve their attributes

    ## Warning in bind_rows_(x, .id): Vectorizing 'labelled' elements may not
    ## preserve their attributes

    ## Warning in bind_rows_(x, .id): Vectorizing 'labelled' elements may not
    ## preserve their attributes

    ## Warning in bind_rows_(x, .id): Vectorizing 'labelled' elements may not
    ## preserve their attributes

    ## Warning in bind_rows_(x, .id): Vectorizing 'labelled' elements may not
    ## preserve their attributes

    ## Warning in bind_rows_(x, .id): Vectorizing 'labelled' elements may not
    ## preserve their attributes

    ## Warning in bind_rows_(x, .id): Vectorizing 'labelled' elements may not
    ## preserve their attributes

    ## Warning in bind_rows_(x, .id): Vectorizing 'labelled' elements may not
    ## preserve their attributes

    ## Warning in bind_rows_(x, .id): Vectorizing 'labelled' elements may not
    ## preserve their attributes

    ## Warning in bind_rows_(x, .id): Vectorizing 'labelled' elements may not
    ## preserve their attributes

    ## Warning in bind_rows_(x, .id): Vectorizing 'labelled' elements may not
    ## preserve their attributes

    ## Warning in bind_rows_(x, .id): Vectorizing 'labelled' elements may not
    ## preserve their attributes

    ## Warning in bind_rows_(x, .id): Vectorizing 'labelled' elements may not
    ## preserve their attributes

    ## Warning in bind_rows_(x, .id): Vectorizing 'labelled' elements may not
    ## preserve their attributes

    ## Warning in bind_rows_(x, .id): Vectorizing 'labelled' elements may not
    ## preserve their attributes

    ## Warning in bind_rows_(x, .id): Vectorizing 'labelled' elements may not
    ## preserve their attributes

    ## Warning in bind_rows_(x, .id): Vectorizing 'labelled' elements may not
    ## preserve their attributes

    ## Warning in bind_rows_(x, .id): Vectorizing 'labelled' elements may not
    ## preserve their attributes

    ## Warning in bind_rows_(x, .id): Vectorizing 'labelled' elements may not
    ## preserve their attributes

    ## Warning in bind_rows_(x, .id): Vectorizing 'labelled' elements may not
    ## preserve their attributes

    ## Warning in bind_rows_(x, .id): Vectorizing 'labelled' elements may not
    ## preserve their attributes

    ## Warning in bind_rows_(x, .id): Vectorizing 'labelled' elements may not
    ## preserve their attributes

    ## Warning in bind_rows_(x, .id): Vectorizing 'labelled' elements may not
    ## preserve their attributes

    ## Warning in bind_rows_(x, .id): Vectorizing 'labelled' elements may not
    ## preserve their attributes

    ## Warning in bind_rows_(x, .id): Vectorizing 'labelled' elements may not
    ## preserve their attributes

    ## Warning in bind_rows_(x, .id): Vectorizing 'labelled' elements may not
    ## preserve their attributes

    ## Warning in bind_rows_(x, .id): Vectorizing 'labelled' elements may not
    ## preserve their attributes

    ## Warning in bind_rows_(x, .id): Vectorizing 'labelled' elements may not
    ## preserve their attributes

    ## Warning in bind_rows_(x, .id): Vectorizing 'labelled' elements may not
    ## preserve their attributes

    ## Warning in bind_rows_(x, .id): Vectorizing 'labelled' elements may not
    ## preserve their attributes

    ## Warning in bind_rows_(x, .id): Vectorizing 'labelled' elements may not
    ## preserve their attributes

    ## Warning in bind_rows_(x, .id): Vectorizing 'labelled' elements may not
    ## preserve their attributes

    ## Warning in bind_rows_(x, .id): Vectorizing 'labelled' elements may not
    ## preserve their attributes

    ## Warning in bind_rows_(x, .id): Vectorizing 'labelled' elements may not
    ## preserve their attributes

    ## Warning in bind_rows_(x, .id): Vectorizing 'labelled' elements may not
    ## preserve their attributes

    ## Warning in bind_rows_(x, .id): Vectorizing 'labelled' elements may not
    ## preserve their attributes

    ## Warning in bind_rows_(x, .id): Vectorizing 'labelled' elements may not
    ## preserve their attributes

    ## Warning in bind_rows_(x, .id): Vectorizing 'labelled' elements may not
    ## preserve their attributes

    ## Warning in bind_rows_(x, .id): Vectorizing 'labelled' elements may not
    ## preserve their attributes

    ## Warning in bind_rows_(x, .id): Vectorizing 'labelled' elements may not
    ## preserve their attributes

    ## Warning in bind_rows_(x, .id): Vectorizing 'labelled' elements may not
    ## preserve their attributes

    ## Warning in bind_rows_(x, .id): Vectorizing 'labelled' elements may not
    ## preserve their attributes

    ## Warning in bind_rows_(x, .id): Vectorizing 'labelled' elements may not
    ## preserve their attributes

    ## Warning in bind_rows_(x, .id): Vectorizing 'labelled' elements may not
    ## preserve their attributes

    ## Warning in bind_rows_(x, .id): Vectorizing 'labelled' elements may not
    ## preserve their attributes

    ## Warning in bind_rows_(x, .id): Vectorizing 'labelled' elements may not
    ## preserve their attributes

    ## Warning in bind_rows_(x, .id): Vectorizing 'labelled' elements may not
    ## preserve their attributes

    ## Warning in bind_rows_(x, .id): Vectorizing 'labelled' elements may not
    ## preserve their attributes

    ## Warning in bind_rows_(x, .id): Vectorizing 'labelled' elements may not
    ## preserve their attributes

    ## Warning in bind_rows_(x, .id): Vectorizing 'labelled' elements may not
    ## preserve their attributes

    ## Warning in bind_rows_(x, .id): Vectorizing 'labelled' elements may not
    ## preserve their attributes

    ## Warning in bind_rows_(x, .id): Vectorizing 'labelled' elements may not
    ## preserve their attributes

    ## Warning in bind_rows_(x, .id): Vectorizing 'labelled' elements may not
    ## preserve their attributes

    ## Warning in bind_rows_(x, .id): Vectorizing 'labelled' elements may not
    ## preserve their attributes

    ## Warning in bind_rows_(x, .id): Vectorizing 'labelled' elements may not
    ## preserve their attributes

    ## Warning in bind_rows_(x, .id): Vectorizing 'labelled' elements may not
    ## preserve their attributes

    ## Warning in bind_rows_(x, .id): Vectorizing 'labelled' elements may not
    ## preserve their attributes

    ## Warning in bind_rows_(x, .id): Vectorizing 'labelled' elements may not
    ## preserve their attributes

    ## Warning in bind_rows_(x, .id): Vectorizing 'labelled' elements may not
    ## preserve their attributes

    ## Warning in bind_rows_(x, .id): Vectorizing 'labelled' elements may not
    ## preserve their attributes

    ## Warning in bind_rows_(x, .id): Vectorizing 'labelled' elements may not
    ## preserve their attributes

    ## Warning in bind_rows_(x, .id): Vectorizing 'labelled' elements may not
    ## preserve their attributes

    ## Warning in bind_rows_(x, .id): Vectorizing 'labelled' elements may not
    ## preserve their attributes

    ## Warning in bind_rows_(x, .id): Vectorizing 'labelled' elements may not
    ## preserve their attributes

    ## Warning in bind_rows_(x, .id): Vectorizing 'labelled' elements may not
    ## preserve their attributes

    ## Warning in bind_rows_(x, .id): Vectorizing 'labelled' elements may not
    ## preserve their attributes

    ## Warning in bind_rows_(x, .id): Vectorizing 'labelled' elements may not
    ## preserve their attributes

    ## Warning in bind_rows_(x, .id): Vectorizing 'labelled' elements may not
    ## preserve their attributes

    ## Warning in bind_rows_(x, .id): Vectorizing 'labelled' elements may not
    ## preserve their attributes

    ## Warning in bind_rows_(x, .id): Vectorizing 'labelled' elements may not
    ## preserve their attributes

    ## Warning in bind_rows_(x, .id): Vectorizing 'labelled' elements may not
    ## preserve their attributes

    ## Warning in bind_rows_(x, .id): Vectorizing 'labelled' elements may not
    ## preserve their attributes

    ## Warning in bind_rows_(x, .id): Vectorizing 'labelled' elements may not
    ## preserve their attributes

    ## Warning in bind_rows_(x, .id): Vectorizing 'labelled' elements may not
    ## preserve their attributes

    ## Warning in bind_rows_(x, .id): Vectorizing 'labelled' elements may not
    ## preserve their attributes

    ## Warning in bind_rows_(x, .id): Vectorizing 'labelled' elements may not
    ## preserve their attributes

    ## Warning in bind_rows_(x, .id): Vectorizing 'labelled' elements may not
    ## preserve their attributes

    ## Warning in bind_rows_(x, .id): Vectorizing 'labelled' elements may not
    ## preserve their attributes

    ## Warning in bind_rows_(x, .id): Vectorizing 'labelled' elements may not
    ## preserve their attributes

    ## Warning in bind_rows_(x, .id): Vectorizing 'labelled' elements may not
    ## preserve their attributes

    ## Warning in bind_rows_(x, .id): Vectorizing 'labelled' elements may not
    ## preserve their attributes

    ## Warning in bind_rows_(x, .id): Vectorizing 'labelled' elements may not
    ## preserve their attributes

    ## Warning in bind_rows_(x, .id): Vectorizing 'labelled' elements may not
    ## preserve their attributes

    ## Warning in bind_rows_(x, .id): Vectorizing 'labelled' elements may not
    ## preserve their attributes

    ## Warning in bind_rows_(x, .id): Vectorizing 'labelled' elements may not
    ## preserve their attributes

    ## Warning in bind_rows_(x, .id): Vectorizing 'labelled' elements may not
    ## preserve their attributes

    ## Warning in bind_rows_(x, .id): Vectorizing 'labelled' elements may not
    ## preserve their attributes

    ## Warning in bind_rows_(x, .id): Vectorizing 'labelled' elements may not
    ## preserve their attributes

    ## Warning in bind_rows_(x, .id): Vectorizing 'labelled' elements may not
    ## preserve their attributes

    ## Warning in bind_rows_(x, .id): Vectorizing 'labelled' elements may not
    ## preserve their attributes

    ## Warning in bind_rows_(x, .id): Vectorizing 'labelled' elements may not
    ## preserve their attributes

    ## Warning in bind_rows_(x, .id): Vectorizing 'labelled' elements may not
    ## preserve their attributes

    ## Warning in bind_rows_(x, .id): Vectorizing 'labelled' elements may not
    ## preserve their attributes

    ## Warning in bind_rows_(x, .id): Vectorizing 'labelled' elements may not
    ## preserve their attributes

    ## Warning in bind_rows_(x, .id): Vectorizing 'labelled' elements may not
    ## preserve their attributes

    ## Warning in bind_rows_(x, .id): Vectorizing 'labelled' elements may not
    ## preserve their attributes

    ## Warning in bind_rows_(x, .id): Vectorizing 'labelled' elements may not
    ## preserve their attributes

    ## Warning in bind_rows_(x, .id): Vectorizing 'labelled' elements may not
    ## preserve their attributes

    ## Warning in bind_rows_(x, .id): Vectorizing 'labelled' elements may not
    ## preserve their attributes

    ## Warning in bind_rows_(x, .id): Vectorizing 'labelled' elements may not
    ## preserve their attributes

    ## Warning in bind_rows_(x, .id): Vectorizing 'labelled' elements may not
    ## preserve their attributes

    ## Warning in bind_rows_(x, .id): Vectorizing 'labelled' elements may not
    ## preserve their attributes

    ## Warning in bind_rows_(x, .id): Vectorizing 'labelled' elements may not
    ## preserve their attributes

    ## Warning in bind_rows_(x, .id): Vectorizing 'labelled' elements may not
    ## preserve their attributes

    ## Warning in bind_rows_(x, .id): Vectorizing 'labelled' elements may not
    ## preserve their attributes

    ## Warning in bind_rows_(x, .id): Vectorizing 'labelled' elements may not
    ## preserve their attributes

    ## Warning in bind_rows_(x, .id): Vectorizing 'labelled' elements may not
    ## preserve their attributes

    ## Warning in bind_rows_(x, .id): Vectorizing 'labelled' elements may not
    ## preserve their attributes

    ## Warning in bind_rows_(x, .id): Vectorizing 'labelled' elements may not
    ## preserve their attributes

    ## Warning in bind_rows_(x, .id): Vectorizing 'labelled' elements may not
    ## preserve their attributes

    ## Warning in bind_rows_(x, .id): Vectorizing 'labelled' elements may not
    ## preserve their attributes

    ## Warning in bind_rows_(x, .id): Vectorizing 'labelled' elements may not
    ## preserve their attributes

    ## Warning in bind_rows_(x, .id): Vectorizing 'labelled' elements may not
    ## preserve their attributes

    ## Warning in bind_rows_(x, .id): Vectorizing 'labelled' elements may not
    ## preserve their attributes

    ## Warning in bind_rows_(x, .id): Vectorizing 'labelled' elements may not
    ## preserve their attributes

    ## Warning in bind_rows_(x, .id): Vectorizing 'labelled' elements may not
    ## preserve their attributes

    ## Warning in bind_rows_(x, .id): Vectorizing 'labelled' elements may not
    ## preserve their attributes

    ## Warning in bind_rows_(x, .id): Vectorizing 'labelled' elements may not
    ## preserve their attributes

    ## Warning in bind_rows_(x, .id): Vectorizing 'labelled' elements may not
    ## preserve their attributes

    ## Warning in bind_rows_(x, .id): Vectorizing 'labelled' elements may not
    ## preserve their attributes

    ## Warning in bind_rows_(x, .id): Vectorizing 'labelled' elements may not
    ## preserve their attributes

    ## Warning in bind_rows_(x, .id): Vectorizing 'labelled' elements may not
    ## preserve their attributes

    ## Warning in bind_rows_(x, .id): Vectorizing 'labelled' elements may not
    ## preserve their attributes

    ## Warning in bind_rows_(x, .id): Vectorizing 'labelled' elements may not
    ## preserve their attributes

    ## Warning in bind_rows_(x, .id): Vectorizing 'labelled' elements may not
    ## preserve their attributes

    ## Warning in bind_rows_(x, .id): Vectorizing 'labelled' elements may not
    ## preserve their attributes

    ## Warning in bind_rows_(x, .id): Vectorizing 'labelled' elements may not
    ## preserve their attributes

    ## Warning in bind_rows_(x, .id): Vectorizing 'labelled' elements may not
    ## preserve their attributes

    ## Warning in bind_rows_(x, .id): Vectorizing 'labelled' elements may not
    ## preserve their attributes

    ## Warning in bind_rows_(x, .id): Vectorizing 'labelled' elements may not
    ## preserve their attributes

    ## Warning in bind_rows_(x, .id): Vectorizing 'labelled' elements may not
    ## preserve their attributes

    ## Warning in bind_rows_(x, .id): Vectorizing 'labelled' elements may not
    ## preserve their attributes

    ## Warning in bind_rows_(x, .id): Vectorizing 'labelled' elements may not
    ## preserve their attributes

    ## Warning in bind_rows_(x, .id): Vectorizing 'labelled' elements may not
    ## preserve their attributes

    ## Warning in bind_rows_(x, .id): Vectorizing 'labelled' elements may not
    ## preserve their attributes

    ## Warning in bind_rows_(x, .id): Vectorizing 'labelled' elements may not
    ## preserve their attributes

    ## Warning in bind_rows_(x, .id): Vectorizing 'labelled' elements may not
    ## preserve their attributes

    ## Warning in bind_rows_(x, .id): Vectorizing 'labelled' elements may not
    ## preserve their attributes

    ## Warning in bind_rows_(x, .id): Vectorizing 'labelled' elements may not
    ## preserve their attributes

    ## Warning in bind_rows_(x, .id): Vectorizing 'labelled' elements may not
    ## preserve their attributes

    ## Warning in bind_rows_(x, .id): Vectorizing 'labelled' elements may not
    ## preserve their attributes

    ## Warning in bind_rows_(x, .id): Vectorizing 'labelled' elements may not
    ## preserve their attributes

    ## Warning in bind_rows_(x, .id): Vectorizing 'labelled' elements may not
    ## preserve their attributes

    ## Warning in bind_rows_(x, .id): Vectorizing 'labelled' elements may not
    ## preserve their attributes

    ## Warning in bind_rows_(x, .id): Vectorizing 'labelled' elements may not
    ## preserve their attributes

    ## Warning in bind_rows_(x, .id): Vectorizing 'labelled' elements may not
    ## preserve their attributes

    ## Warning in bind_rows_(x, .id): Vectorizing 'labelled' elements may not
    ## preserve their attributes

    ## Warning in bind_rows_(x, .id): Vectorizing 'labelled' elements may not
    ## preserve their attributes

    ## Warning in bind_rows_(x, .id): Vectorizing 'labelled' elements may not
    ## preserve their attributes

    ## Warning in bind_rows_(x, .id): Vectorizing 'labelled' elements may not
    ## preserve their attributes

    ## Warning in bind_rows_(x, .id): Vectorizing 'labelled' elements may not
    ## preserve their attributes

    ## Warning in bind_rows_(x, .id): Vectorizing 'labelled' elements may not
    ## preserve their attributes

    ## Warning in bind_rows_(x, .id): Vectorizing 'labelled' elements may not
    ## preserve their attributes

    ## Warning in bind_rows_(x, .id): Vectorizing 'labelled' elements may not
    ## preserve their attributes

    ## Warning in bind_rows_(x, .id): Vectorizing 'labelled' elements may not
    ## preserve their attributes

    ## Warning in bind_rows_(x, .id): Vectorizing 'labelled' elements may not
    ## preserve their attributes

    ## Warning in bind_rows_(x, .id): Vectorizing 'labelled' elements may not
    ## preserve their attributes

    ## Warning in bind_rows_(x, .id): Vectorizing 'labelled' elements may not
    ## preserve their attributes

    ## Warning in bind_rows_(x, .id): Vectorizing 'labelled' elements may not
    ## preserve their attributes

    ## Warning in bind_rows_(x, .id): Vectorizing 'labelled' elements may not
    ## preserve their attributes

    ## Warning in bind_rows_(x, .id): Vectorizing 'labelled' elements may not
    ## preserve their attributes

    ## Warning in bind_rows_(x, .id): Vectorizing 'labelled' elements may not
    ## preserve their attributes

    ## Warning in bind_rows_(x, .id): Vectorizing 'labelled' elements may not
    ## preserve their attributes

    ## Warning in bind_rows_(x, .id): Vectorizing 'labelled' elements may not
    ## preserve their attributes

    ## Warning in bind_rows_(x, .id): Vectorizing 'labelled' elements may not
    ## preserve their attributes

    ## Warning in bind_rows_(x, .id): Vectorizing 'labelled' elements may not
    ## preserve their attributes

    ## Warning in bind_rows_(x, .id): Vectorizing 'labelled' elements may not
    ## preserve their attributes

    ## Warning in bind_rows_(x, .id): Vectorizing 'labelled' elements may not
    ## preserve their attributes

    ## Warning in bind_rows_(x, .id): Vectorizing 'labelled' elements may not
    ## preserve their attributes

    ## Warning in bind_rows_(x, .id): Vectorizing 'labelled' elements may not
    ## preserve their attributes

    ## Warning in bind_rows_(x, .id): Vectorizing 'labelled' elements may not
    ## preserve their attributes

    ## Warning in bind_rows_(x, .id): Vectorizing 'labelled' elements may not
    ## preserve their attributes

    ## Warning in bind_rows_(x, .id): Vectorizing 'labelled' elements may not
    ## preserve their attributes

    ## Warning in bind_rows_(x, .id): Vectorizing 'labelled' elements may not
    ## preserve their attributes

    ## Warning in bind_rows_(x, .id): Vectorizing 'labelled' elements may not
    ## preserve their attributes

    ## Warning in bind_rows_(x, .id): Vectorizing 'labelled' elements may not
    ## preserve their attributes

    ## Warning in bind_rows_(x, .id): Vectorizing 'labelled' elements may not
    ## preserve their attributes

    ## Warning in bind_rows_(x, .id): Vectorizing 'labelled' elements may not
    ## preserve their attributes

    ## Warning in bind_rows_(x, .id): Vectorizing 'labelled' elements may not
    ## preserve their attributes

    ## Warning in bind_rows_(x, .id): Vectorizing 'labelled' elements may not
    ## preserve their attributes

    ## Warning in bind_rows_(x, .id): Vectorizing 'labelled' elements may not
    ## preserve their attributes

    ## Warning in bind_rows_(x, .id): Vectorizing 'labelled' elements may not
    ## preserve their attributes

    ## Warning in bind_rows_(x, .id): Vectorizing 'labelled' elements may not
    ## preserve their attributes

    ## Warning in bind_rows_(x, .id): Vectorizing 'labelled' elements may not
    ## preserve their attributes

    ## Warning in bind_rows_(x, .id): Vectorizing 'labelled' elements may not
    ## preserve their attributes

    ## Warning in bind_rows_(x, .id): Vectorizing 'labelled' elements may not
    ## preserve their attributes

    ## Warning in bind_rows_(x, .id): Vectorizing 'labelled' elements may not
    ## preserve their attributes

    ## Warning in bind_rows_(x, .id): Vectorizing 'labelled' elements may not
    ## preserve their attributes

    ## Warning in bind_rows_(x, .id): Vectorizing 'labelled' elements may not
    ## preserve their attributes

    ## Warning in bind_rows_(x, .id): Vectorizing 'labelled' elements may not
    ## preserve their attributes

    ## Warning in bind_rows_(x, .id): Vectorizing 'labelled' elements may not
    ## preserve their attributes

    ## Warning in bind_rows_(x, .id): Vectorizing 'labelled' elements may not
    ## preserve their attributes

    ## Warning in bind_rows_(x, .id): Vectorizing 'labelled' elements may not
    ## preserve their attributes

    ## Warning in bind_rows_(x, .id): Vectorizing 'labelled' elements may not
    ## preserve their attributes

    ## Warning in bind_rows_(x, .id): Vectorizing 'labelled' elements may not
    ## preserve their attributes

    ## Warning in bind_rows_(x, .id): Vectorizing 'labelled' elements may not
    ## preserve their attributes

    ## Warning in bind_rows_(x, .id): Vectorizing 'labelled' elements may not
    ## preserve their attributes

    ## Warning in bind_rows_(x, .id): Vectorizing 'labelled' elements may not
    ## preserve their attributes

    ## Warning in bind_rows_(x, .id): Vectorizing 'labelled' elements may not
    ## preserve their attributes

    ## Warning in bind_rows_(x, .id): Vectorizing 'labelled' elements may not
    ## preserve their attributes

    ## Warning in bind_rows_(x, .id): Vectorizing 'labelled' elements may not
    ## preserve their attributes

    ## Warning in bind_rows_(x, .id): Vectorizing 'labelled' elements may not
    ## preserve their attributes

    ## Warning in bind_rows_(x, .id): Vectorizing 'labelled' elements may not
    ## preserve their attributes

    ## Warning in bind_rows_(x, .id): Vectorizing 'labelled' elements may not
    ## preserve their attributes

    ## Warning in bind_rows_(x, .id): Vectorizing 'labelled' elements may not
    ## preserve their attributes

    ## Warning in bind_rows_(x, .id): Vectorizing 'labelled' elements may not
    ## preserve their attributes

    ## Warning in bind_rows_(x, .id): Vectorizing 'labelled' elements may not
    ## preserve their attributes

    ## Warning in bind_rows_(x, .id): Vectorizing 'labelled' elements may not
    ## preserve their attributes

    ## Warning in bind_rows_(x, .id): Vectorizing 'labelled' elements may not
    ## preserve their attributes

    ## Warning in bind_rows_(x, .id): Vectorizing 'labelled' elements may not
    ## preserve their attributes

    ## Warning in bind_rows_(x, .id): Vectorizing 'labelled' elements may not
    ## preserve their attributes

    ## Warning in bind_rows_(x, .id): Vectorizing 'labelled' elements may not
    ## preserve their attributes

    ## Warning in bind_rows_(x, .id): Vectorizing 'labelled' elements may not
    ## preserve their attributes

    ## Warning in bind_rows_(x, .id): Vectorizing 'labelled' elements may not
    ## preserve their attributes

    ## Warning in bind_rows_(x, .id): Vectorizing 'labelled' elements may not
    ## preserve their attributes

    ## Warning in bind_rows_(x, .id): Vectorizing 'labelled' elements may not
    ## preserve their attributes

    ## Warning in bind_rows_(x, .id): Vectorizing 'labelled' elements may not
    ## preserve their attributes

    ## Warning in bind_rows_(x, .id): Vectorizing 'labelled' elements may not
    ## preserve their attributes

    ## Warning in bind_rows_(x, .id): Vectorizing 'labelled' elements may not
    ## preserve their attributes

    ## Warning in bind_rows_(x, .id): Vectorizing 'labelled' elements may not
    ## preserve their attributes

    ## Warning in bind_rows_(x, .id): Vectorizing 'labelled' elements may not
    ## preserve their attributes

    ## Warning in bind_rows_(x, .id): Vectorizing 'labelled' elements may not
    ## preserve their attributes

    ## Warning in bind_rows_(x, .id): Vectorizing 'labelled' elements may not
    ## preserve their attributes

    ## Warning in bind_rows_(x, .id): Vectorizing 'labelled' elements may not
    ## preserve their attributes

    ## Warning in bind_rows_(x, .id): Vectorizing 'labelled' elements may not
    ## preserve their attributes

    ## Warning in bind_rows_(x, .id): Vectorizing 'labelled' elements may not
    ## preserve their attributes

    ## Warning in bind_rows_(x, .id): Vectorizing 'labelled' elements may not
    ## preserve their attributes

    ## Warning in bind_rows_(x, .id): Vectorizing 'labelled' elements may not
    ## preserve their attributes

    ## Warning in bind_rows_(x, .id): Vectorizing 'labelled' elements may not
    ## preserve their attributes

    ## Warning in bind_rows_(x, .id): Vectorizing 'labelled' elements may not
    ## preserve their attributes

    ## Warning in bind_rows_(x, .id): Vectorizing 'labelled' elements may not
    ## preserve their attributes

    ## Warning in bind_rows_(x, .id): Vectorizing 'labelled' elements may not
    ## preserve their attributes

    ## Warning in bind_rows_(x, .id): Vectorizing 'labelled' elements may not
    ## preserve their attributes

    ## Warning in bind_rows_(x, .id): Vectorizing 'labelled' elements may not
    ## preserve their attributes

    ## Warning in bind_rows_(x, .id): Vectorizing 'labelled' elements may not
    ## preserve their attributes

    ## Warning in bind_rows_(x, .id): Vectorizing 'labelled' elements may not
    ## preserve their attributes

    ## Warning in bind_rows_(x, .id): Vectorizing 'labelled' elements may not
    ## preserve their attributes

    ## Warning in bind_rows_(x, .id): Vectorizing 'labelled' elements may not
    ## preserve their attributes

    ## Warning in bind_rows_(x, .id): Vectorizing 'labelled' elements may not
    ## preserve their attributes

    ## Warning in bind_rows_(x, .id): Vectorizing 'labelled' elements may not
    ## preserve their attributes

    ## Warning in bind_rows_(x, .id): Vectorizing 'labelled' elements may not
    ## preserve their attributes

    ## Warning in bind_rows_(x, .id): Vectorizing 'labelled' elements may not
    ## preserve their attributes

    ## Warning in bind_rows_(x, .id): Vectorizing 'labelled' elements may not
    ## preserve their attributes

    ## Warning in bind_rows_(x, .id): Vectorizing 'labelled' elements may not
    ## preserve their attributes

    ## Warning in bind_rows_(x, .id): Vectorizing 'labelled' elements may not
    ## preserve their attributes

    ## Warning in bind_rows_(x, .id): Vectorizing 'labelled' elements may not
    ## preserve their attributes

    ## Warning in bind_rows_(x, .id): Vectorizing 'labelled' elements may not
    ## preserve their attributes

    ## Warning in bind_rows_(x, .id): Vectorizing 'labelled' elements may not
    ## preserve their attributes

    ## Warning in bind_rows_(x, .id): Vectorizing 'labelled' elements may not
    ## preserve their attributes

    ## Warning in bind_rows_(x, .id): Vectorizing 'labelled' elements may not
    ## preserve their attributes

    ## Warning in bind_rows_(x, .id): Vectorizing 'labelled' elements may not
    ## preserve their attributes

    ## Warning in bind_rows_(x, .id): Vectorizing 'labelled' elements may not
    ## preserve their attributes

    ## Warning in bind_rows_(x, .id): Vectorizing 'labelled' elements may not
    ## preserve their attributes

    ## Warning in bind_rows_(x, .id): Vectorizing 'labelled' elements may not
    ## preserve their attributes

    ## Warning in bind_rows_(x, .id): Vectorizing 'labelled' elements may not
    ## preserve their attributes

    ## Warning in bind_rows_(x, .id): Vectorizing 'labelled' elements may not
    ## preserve their attributes

    ## Warning in bind_rows_(x, .id): Vectorizing 'labelled' elements may not
    ## preserve their attributes

    ## Warning in bind_rows_(x, .id): Vectorizing 'labelled' elements may not
    ## preserve their attributes

    ## Warning in bind_rows_(x, .id): Vectorizing 'labelled' elements may not
    ## preserve their attributes

    ## Warning in bind_rows_(x, .id): Vectorizing 'labelled' elements may not
    ## preserve their attributes

    ## Warning in bind_rows_(x, .id): Vectorizing 'labelled' elements may not
    ## preserve their attributes

    ## Warning in bind_rows_(x, .id): Vectorizing 'labelled' elements may not
    ## preserve their attributes

    ## Warning in bind_rows_(x, .id): Vectorizing 'labelled' elements may not
    ## preserve their attributes

    ## Warning in bind_rows_(x, .id): Vectorizing 'labelled' elements may not
    ## preserve their attributes

    ## Warning in bind_rows_(x, .id): Vectorizing 'labelled' elements may not
    ## preserve their attributes

    ## Warning in bind_rows_(x, .id): Vectorizing 'labelled' elements may not
    ## preserve their attributes

    ## Warning in bind_rows_(x, .id): Vectorizing 'labelled' elements may not
    ## preserve their attributes

    ## Warning in bind_rows_(x, .id): Vectorizing 'labelled' elements may not
    ## preserve their attributes

    ## Warning in bind_rows_(x, .id): Vectorizing 'labelled' elements may not
    ## preserve their attributes

    ## Warning in bind_rows_(x, .id): Vectorizing 'labelled' elements may not
    ## preserve their attributes

    ## Warning in bind_rows_(x, .id): Vectorizing 'labelled' elements may not
    ## preserve their attributes

    ## Warning in bind_rows_(x, .id): Vectorizing 'labelled' elements may not
    ## preserve their attributes

    ## Warning in bind_rows_(x, .id): Vectorizing 'labelled' elements may not
    ## preserve their attributes

    ## Warning in bind_rows_(x, .id): Vectorizing 'labelled' elements may not
    ## preserve their attributes

    ## Warning in bind_rows_(x, .id): Vectorizing 'labelled' elements may not
    ## preserve their attributes

    ## Warning in bind_rows_(x, .id): Vectorizing 'labelled' elements may not
    ## preserve their attributes

    ## Warning in bind_rows_(x, .id): Vectorizing 'labelled' elements may not
    ## preserve their attributes

    ## Warning in bind_rows_(x, .id): Vectorizing 'labelled' elements may not
    ## preserve their attributes

    ## Warning in bind_rows_(x, .id): Vectorizing 'labelled' elements may not
    ## preserve their attributes

    ## Warning in bind_rows_(x, .id): Vectorizing 'labelled' elements may not
    ## preserve their attributes

    ## Warning in bind_rows_(x, .id): Vectorizing 'labelled' elements may not
    ## preserve their attributes

    ## Warning in bind_rows_(x, .id): Vectorizing 'labelled' elements may not
    ## preserve their attributes

    ## Warning in bind_rows_(x, .id): Vectorizing 'labelled' elements may not
    ## preserve their attributes

    ## Warning in bind_rows_(x, .id): Vectorizing 'labelled' elements may not
    ## preserve their attributes

    ## Warning in bind_rows_(x, .id): Vectorizing 'labelled' elements may not
    ## preserve their attributes

    ## Warning in bind_rows_(x, .id): Vectorizing 'labelled' elements may not
    ## preserve their attributes

    ## Warning in bind_rows_(x, .id): Vectorizing 'labelled' elements may not
    ## preserve their attributes

    ## Warning in bind_rows_(x, .id): Vectorizing 'labelled' elements may not
    ## preserve their attributes

    ## Warning in bind_rows_(x, .id): Vectorizing 'labelled' elements may not
    ## preserve their attributes

    ## Warning in bind_rows_(x, .id): Vectorizing 'labelled' elements may not
    ## preserve their attributes

    ## Warning in bind_rows_(x, .id): Vectorizing 'labelled' elements may not
    ## preserve their attributes

    ## Warning in bind_rows_(x, .id): Vectorizing 'labelled' elements may not
    ## preserve their attributes

    ## Warning in bind_rows_(x, .id): Vectorizing 'labelled' elements may not
    ## preserve their attributes

    ## Warning in bind_rows_(x, .id): Vectorizing 'labelled' elements may not
    ## preserve their attributes

    ## Warning in bind_rows_(x, .id): Vectorizing 'labelled' elements may not
    ## preserve their attributes

    ## Warning in bind_rows_(x, .id): Vectorizing 'labelled' elements may not
    ## preserve their attributes

    ## Warning in bind_rows_(x, .id): Vectorizing 'labelled' elements may not
    ## preserve their attributes

    ## Warning in bind_rows_(x, .id): Vectorizing 'labelled' elements may not
    ## preserve their attributes

    ## Warning in bind_rows_(x, .id): Vectorizing 'labelled' elements may not
    ## preserve their attributes

    ## Warning in bind_rows_(x, .id): Vectorizing 'labelled' elements may not
    ## preserve their attributes

    ## Warning in bind_rows_(x, .id): Vectorizing 'labelled' elements may not
    ## preserve their attributes

    ## Warning in bind_rows_(x, .id): Vectorizing 'labelled' elements may not
    ## preserve their attributes

    ## Warning in bind_rows_(x, .id): Vectorizing 'labelled' elements may not
    ## preserve their attributes

    ## Warning in bind_rows_(x, .id): Vectorizing 'labelled' elements may not
    ## preserve their attributes

    ## Warning in bind_rows_(x, .id): Vectorizing 'labelled' elements may not
    ## preserve their attributes

    ## Warning in bind_rows_(x, .id): Vectorizing 'labelled' elements may not
    ## preserve their attributes

    ## Warning in bind_rows_(x, .id): Vectorizing 'labelled' elements may not
    ## preserve their attributes

    ## Warning in bind_rows_(x, .id): Vectorizing 'labelled' elements may not
    ## preserve their attributes

    ## Warning in bind_rows_(x, .id): Vectorizing 'labelled' elements may not
    ## preserve their attributes

    ## Warning in bind_rows_(x, .id): Vectorizing 'labelled' elements may not
    ## preserve their attributes

    ## Warning in bind_rows_(x, .id): Vectorizing 'labelled' elements may not
    ## preserve their attributes

    ## Warning in bind_rows_(x, .id): Vectorizing 'labelled' elements may not
    ## preserve their attributes

    ## Warning in bind_rows_(x, .id): Vectorizing 'labelled' elements may not
    ## preserve their attributes

    ## Warning in bind_rows_(x, .id): Vectorizing 'labelled' elements may not
    ## preserve their attributes

    ## Warning in bind_rows_(x, .id): Vectorizing 'labelled' elements may not
    ## preserve their attributes

    ## Warning in bind_rows_(x, .id): Vectorizing 'labelled' elements may not
    ## preserve their attributes

    ## Warning in bind_rows_(x, .id): Vectorizing 'labelled' elements may not
    ## preserve their attributes

    ## Warning in bind_rows_(x, .id): Vectorizing 'labelled' elements may not
    ## preserve their attributes

    ## Warning in bind_rows_(x, .id): Vectorizing 'labelled' elements may not
    ## preserve their attributes

    ## Warning in bind_rows_(x, .id): Vectorizing 'labelled' elements may not
    ## preserve their attributes

    ## Warning in bind_rows_(x, .id): Vectorizing 'labelled' elements may not
    ## preserve their attributes

    ## Warning in bind_rows_(x, .id): Vectorizing 'labelled' elements may not
    ## preserve their attributes

    ## Warning in bind_rows_(x, .id): Vectorizing 'labelled' elements may not
    ## preserve their attributes

    ## Warning in bind_rows_(x, .id): Vectorizing 'labelled' elements may not
    ## preserve their attributes

    ## Warning in bind_rows_(x, .id): Vectorizing 'labelled' elements may not
    ## preserve their attributes

    ## Warning in bind_rows_(x, .id): Vectorizing 'labelled' elements may not
    ## preserve their attributes

    ## Warning in bind_rows_(x, .id): Vectorizing 'labelled' elements may not
    ## preserve their attributes

    ## Warning in bind_rows_(x, .id): Vectorizing 'labelled' elements may not
    ## preserve their attributes

    ## Warning in bind_rows_(x, .id): Vectorizing 'labelled' elements may not
    ## preserve their attributes

    ## Warning in bind_rows_(x, .id): Vectorizing 'labelled' elements may not
    ## preserve their attributes

    ## Warning in bind_rows_(x, .id): Vectorizing 'labelled' elements may not
    ## preserve their attributes

    ## Warning in bind_rows_(x, .id): Vectorizing 'labelled' elements may not
    ## preserve their attributes

    ## Warning in bind_rows_(x, .id): Vectorizing 'labelled' elements may not
    ## preserve their attributes

    ## Warning in bind_rows_(x, .id): Vectorizing 'labelled' elements may not
    ## preserve their attributes

    ## Warning in bind_rows_(x, .id): Vectorizing 'labelled' elements may not
    ## preserve their attributes

    ## Warning in bind_rows_(x, .id): Vectorizing 'labelled' elements may not
    ## preserve their attributes

    ## Warning in bind_rows_(x, .id): Vectorizing 'labelled' elements may not
    ## preserve their attributes

    ## Warning in bind_rows_(x, .id): Vectorizing 'labelled' elements may not
    ## preserve their attributes

    ## Warning in bind_rows_(x, .id): Vectorizing 'labelled' elements may not
    ## preserve their attributes

    ## Warning in bind_rows_(x, .id): Vectorizing 'labelled' elements may not
    ## preserve their attributes

    ## Warning in bind_rows_(x, .id): Vectorizing 'labelled' elements may not
    ## preserve their attributes

    ## Warning in bind_rows_(x, .id): Vectorizing 'labelled' elements may not
    ## preserve their attributes

    ## Warning in bind_rows_(x, .id): Vectorizing 'labelled' elements may not
    ## preserve their attributes

    ## Warning in bind_rows_(x, .id): Vectorizing 'labelled' elements may not
    ## preserve their attributes

    ## Warning in bind_rows_(x, .id): Vectorizing 'labelled' elements may not
    ## preserve their attributes

    ## Warning in bind_rows_(x, .id): Vectorizing 'labelled' elements may not
    ## preserve their attributes

    ## Warning in bind_rows_(x, .id): Vectorizing 'labelled' elements may not
    ## preserve their attributes

    ## Warning in bind_rows_(x, .id): Vectorizing 'labelled' elements may not
    ## preserve their attributes

    ## Warning in bind_rows_(x, .id): Vectorizing 'labelled' elements may not
    ## preserve their attributes

    ## Warning in bind_rows_(x, .id): Vectorizing 'labelled' elements may not
    ## preserve their attributes

    ## Warning in bind_rows_(x, .id): Vectorizing 'labelled' elements may not
    ## preserve their attributes

    ## Warning in bind_rows_(x, .id): Vectorizing 'labelled' elements may not
    ## preserve their attributes

    ## Warning in bind_rows_(x, .id): Vectorizing 'labelled' elements may not
    ## preserve their attributes

    ## Warning in bind_rows_(x, .id): Vectorizing 'labelled' elements may not
    ## preserve their attributes

    ## Warning in bind_rows_(x, .id): Vectorizing 'labelled' elements may not
    ## preserve their attributes

    ## Warning in bind_rows_(x, .id): Vectorizing 'labelled' elements may not
    ## preserve their attributes

    ## Warning in bind_rows_(x, .id): Vectorizing 'labelled' elements may not
    ## preserve their attributes

    ## Warning in bind_rows_(x, .id): Vectorizing 'labelled' elements may not
    ## preserve their attributes

    ## Warning in bind_rows_(x, .id): Vectorizing 'labelled' elements may not
    ## preserve their attributes

    ## Warning in bind_rows_(x, .id): Vectorizing 'labelled' elements may not
    ## preserve their attributes

    ## Warning in bind_rows_(x, .id): Vectorizing 'labelled' elements may not
    ## preserve their attributes

    ## Warning in bind_rows_(x, .id): Vectorizing 'labelled' elements may not
    ## preserve their attributes

    ## Warning in bind_rows_(x, .id): Vectorizing 'labelled' elements may not
    ## preserve their attributes

    ## Warning in bind_rows_(x, .id): Vectorizing 'labelled' elements may not
    ## preserve their attributes

    ## Warning in bind_rows_(x, .id): Vectorizing 'labelled' elements may not
    ## preserve their attributes

    ## Warning in bind_rows_(x, .id): Vectorizing 'labelled' elements may not
    ## preserve their attributes

    ## Warning in bind_rows_(x, .id): Vectorizing 'labelled' elements may not
    ## preserve their attributes

    ## Warning in bind_rows_(x, .id): Vectorizing 'labelled' elements may not
    ## preserve their attributes

    ## Warning in bind_rows_(x, .id): Vectorizing 'labelled' elements may not
    ## preserve their attributes

    ## Warning in bind_rows_(x, .id): Vectorizing 'labelled' elements may not
    ## preserve their attributes

    ## Warning in bind_rows_(x, .id): Vectorizing 'labelled' elements may not
    ## preserve their attributes

    ## Warning in bind_rows_(x, .id): Vectorizing 'labelled' elements may not
    ## preserve their attributes

    ## Warning in bind_rows_(x, .id): Vectorizing 'labelled' elements may not
    ## preserve their attributes

    ## Warning in bind_rows_(x, .id): Vectorizing 'labelled' elements may not
    ## preserve their attributes

    ## Warning in bind_rows_(x, .id): Vectorizing 'labelled' elements may not
    ## preserve their attributes

    ## Warning in bind_rows_(x, .id): Vectorizing 'labelled' elements may not
    ## preserve their attributes

    ## Warning in bind_rows_(x, .id): Vectorizing 'labelled' elements may not
    ## preserve their attributes

    ## Warning in bind_rows_(x, .id): Vectorizing 'labelled' elements may not
    ## preserve their attributes

    ## Warning in bind_rows_(x, .id): Vectorizing 'labelled' elements may not
    ## preserve their attributes

    ## Warning in bind_rows_(x, .id): Vectorizing 'labelled' elements may not
    ## preserve their attributes

    ## Warning in bind_rows_(x, .id): Vectorizing 'labelled' elements may not
    ## preserve their attributes

    ## Warning in bind_rows_(x, .id): Vectorizing 'labelled' elements may not
    ## preserve their attributes

    ## Warning in bind_rows_(x, .id): Vectorizing 'labelled' elements may not
    ## preserve their attributes

    ## Warning in bind_rows_(x, .id): Vectorizing 'labelled' elements may not
    ## preserve their attributes

    ## Warning in bind_rows_(x, .id): Vectorizing 'labelled' elements may not
    ## preserve their attributes

    ## Warning in bind_rows_(x, .id): Vectorizing 'labelled' elements may not
    ## preserve their attributes

    ## Warning in bind_rows_(x, .id): Vectorizing 'labelled' elements may not
    ## preserve their attributes

    ## Warning in bind_rows_(x, .id): Vectorizing 'labelled' elements may not
    ## preserve their attributes

    ## Warning in bind_rows_(x, .id): Vectorizing 'labelled' elements may not
    ## preserve their attributes

    ## Warning in bind_rows_(x, .id): Vectorizing 'labelled' elements may not
    ## preserve their attributes

    ## Warning in bind_rows_(x, .id): Vectorizing 'labelled' elements may not
    ## preserve their attributes

    ## Warning in bind_rows_(x, .id): Vectorizing 'labelled' elements may not
    ## preserve their attributes

    ## Warning in bind_rows_(x, .id): Vectorizing 'labelled' elements may not
    ## preserve their attributes

    ## Warning in bind_rows_(x, .id): Vectorizing 'labelled' elements may not
    ## preserve their attributes

    ## Warning in bind_rows_(x, .id): Vectorizing 'labelled' elements may not
    ## preserve their attributes

    ## Warning in bind_rows_(x, .id): Vectorizing 'labelled' elements may not
    ## preserve their attributes

    ## Warning in bind_rows_(x, .id): Vectorizing 'labelled' elements may not
    ## preserve their attributes

    ## Warning in bind_rows_(x, .id): Vectorizing 'labelled' elements may not
    ## preserve their attributes

    ## Warning in bind_rows_(x, .id): Vectorizing 'labelled' elements may not
    ## preserve their attributes

    ## Warning in bind_rows_(x, .id): Vectorizing 'labelled' elements may not
    ## preserve their attributes

    ## Warning in bind_rows_(x, .id): Vectorizing 'labelled' elements may not
    ## preserve their attributes

    ## Warning in bind_rows_(x, .id): Vectorizing 'labelled' elements may not
    ## preserve their attributes

    ## Warning in bind_rows_(x, .id): Vectorizing 'labelled' elements may not
    ## preserve their attributes

    ## Warning in bind_rows_(x, .id): Vectorizing 'labelled' elements may not
    ## preserve their attributes

    ## Warning in bind_rows_(x, .id): Vectorizing 'labelled' elements may not
    ## preserve their attributes

    ## Warning in bind_rows_(x, .id): Vectorizing 'labelled' elements may not
    ## preserve their attributes

    ## Warning in bind_rows_(x, .id): Vectorizing 'labelled' elements may not
    ## preserve their attributes

    ## Warning in bind_rows_(x, .id): Vectorizing 'labelled' elements may not
    ## preserve their attributes

    ## Warning in bind_rows_(x, .id): Vectorizing 'labelled' elements may not
    ## preserve their attributes

    ## Warning in bind_rows_(x, .id): Vectorizing 'labelled' elements may not
    ## preserve their attributes

    ## Warning in bind_rows_(x, .id): Vectorizing 'labelled' elements may not
    ## preserve their attributes

    ## Warning in bind_rows_(x, .id): Vectorizing 'labelled' elements may not
    ## preserve their attributes

    ## Warning in bind_rows_(x, .id): Vectorizing 'labelled' elements may not
    ## preserve their attributes

    ## Warning in bind_rows_(x, .id): Vectorizing 'labelled' elements may not
    ## preserve their attributes

    ## Warning in bind_rows_(x, .id): Vectorizing 'labelled' elements may not
    ## preserve their attributes

    ## Warning in bind_rows_(x, .id): Vectorizing 'labelled' elements may not
    ## preserve their attributes

    ## Warning in bind_rows_(x, .id): Vectorizing 'labelled' elements may not
    ## preserve their attributes

    ## Warning in bind_rows_(x, .id): Vectorizing 'labelled' elements may not
    ## preserve their attributes

    ## Warning in bind_rows_(x, .id): Vectorizing 'labelled' elements may not
    ## preserve their attributes

    ## Warning in bind_rows_(x, .id): Vectorizing 'labelled' elements may not
    ## preserve their attributes

    ## Warning in bind_rows_(x, .id): Vectorizing 'labelled' elements may not
    ## preserve their attributes

    ## Warning in bind_rows_(x, .id): Vectorizing 'labelled' elements may not
    ## preserve their attributes

    ## Warning in bind_rows_(x, .id): Vectorizing 'labelled' elements may not
    ## preserve their attributes

    ## Warning in bind_rows_(x, .id): Vectorizing 'labelled' elements may not
    ## preserve their attributes

    ## Warning in bind_rows_(x, .id): Vectorizing 'labelled' elements may not
    ## preserve their attributes

    ## Warning in bind_rows_(x, .id): Vectorizing 'labelled' elements may not
    ## preserve their attributes

    ## Warning in bind_rows_(x, .id): Vectorizing 'labelled' elements may not
    ## preserve their attributes

    ## Warning in bind_rows_(x, .id): Vectorizing 'labelled' elements may not
    ## preserve their attributes

    ## Warning in bind_rows_(x, .id): Vectorizing 'labelled' elements may not
    ## preserve their attributes

    ## Warning in bind_rows_(x, .id): Vectorizing 'labelled' elements may not
    ## preserve their attributes

    ## Warning in bind_rows_(x, .id): Vectorizing 'labelled' elements may not
    ## preserve their attributes

    ## Warning in bind_rows_(x, .id): Vectorizing 'labelled' elements may not
    ## preserve their attributes

    ## Warning in bind_rows_(x, .id): Vectorizing 'labelled' elements may not
    ## preserve their attributes

    ## Warning in bind_rows_(x, .id): Vectorizing 'labelled' elements may not
    ## preserve their attributes

    ## Warning in bind_rows_(x, .id): Vectorizing 'labelled' elements may not
    ## preserve their attributes

    ## Warning in bind_rows_(x, .id): Vectorizing 'labelled' elements may not
    ## preserve their attributes

    ## Warning in bind_rows_(x, .id): Vectorizing 'labelled' elements may not
    ## preserve their attributes

    ## Warning in bind_rows_(x, .id): Vectorizing 'labelled' elements may not
    ## preserve their attributes

    ## Warning in bind_rows_(x, .id): Vectorizing 'labelled' elements may not
    ## preserve their attributes

    ## Warning in bind_rows_(x, .id): Vectorizing 'labelled' elements may not
    ## preserve their attributes

    ## Warning in bind_rows_(x, .id): Vectorizing 'labelled' elements may not
    ## preserve their attributes

    ## Warning in bind_rows_(x, .id): Vectorizing 'labelled' elements may not
    ## preserve their attributes

    ## Warning in bind_rows_(x, .id): Vectorizing 'labelled' elements may not
    ## preserve their attributes

    ## Warning in bind_rows_(x, .id): Vectorizing 'labelled' elements may not
    ## preserve their attributes

    ## Warning in bind_rows_(x, .id): Vectorizing 'labelled' elements may not
    ## preserve their attributes

    ## Warning in bind_rows_(x, .id): Vectorizing 'labelled' elements may not
    ## preserve their attributes

    ## Warning in bind_rows_(x, .id): Vectorizing 'labelled' elements may not
    ## preserve their attributes

    ## Warning in bind_rows_(x, .id): Vectorizing 'labelled' elements may not
    ## preserve their attributes

    ## Warning in bind_rows_(x, .id): Vectorizing 'labelled' elements may not
    ## preserve their attributes

    ## Warning in bind_rows_(x, .id): Vectorizing 'labelled' elements may not
    ## preserve their attributes

    ## Warning in bind_rows_(x, .id): Vectorizing 'labelled' elements may not
    ## preserve their attributes

    ## Warning in bind_rows_(x, .id): Vectorizing 'labelled' elements may not
    ## preserve their attributes

    ## Warning in bind_rows_(x, .id): Vectorizing 'labelled' elements may not
    ## preserve their attributes

    ## Warning in bind_rows_(x, .id): Vectorizing 'labelled' elements may not
    ## preserve their attributes

    ## Warning in bind_rows_(x, .id): Vectorizing 'labelled' elements may not
    ## preserve their attributes

    ## Warning in bind_rows_(x, .id): Vectorizing 'labelled' elements may not
    ## preserve their attributes

    ## Warning in bind_rows_(x, .id): Vectorizing 'labelled' elements may not
    ## preserve their attributes

    ## Warning in bind_rows_(x, .id): Vectorizing 'labelled' elements may not
    ## preserve their attributes

    ## Warning in bind_rows_(x, .id): Vectorizing 'labelled' elements may not
    ## preserve their attributes

    ## Warning in bind_rows_(x, .id): Vectorizing 'labelled' elements may not
    ## preserve their attributes

    ## Warning in bind_rows_(x, .id): Vectorizing 'labelled' elements may not
    ## preserve their attributes

    ## Warning in bind_rows_(x, .id): Vectorizing 'labelled' elements may not
    ## preserve their attributes

    ## Warning in bind_rows_(x, .id): Vectorizing 'labelled' elements may not
    ## preserve their attributes

    ## Warning in bind_rows_(x, .id): Vectorizing 'labelled' elements may not
    ## preserve their attributes

    ## Warning in bind_rows_(x, .id): Vectorizing 'labelled' elements may not
    ## preserve their attributes

    ## Warning in bind_rows_(x, .id): Vectorizing 'labelled' elements may not
    ## preserve their attributes

    ## Warning in bind_rows_(x, .id): Vectorizing 'labelled' elements may not
    ## preserve their attributes

    ## Warning in bind_rows_(x, .id): Vectorizing 'labelled' elements may not
    ## preserve their attributes

    ## Warning in bind_rows_(x, .id): Vectorizing 'labelled' elements may not
    ## preserve their attributes

    ## Warning in bind_rows_(x, .id): Vectorizing 'labelled' elements may not
    ## preserve their attributes

    ## Warning in bind_rows_(x, .id): Vectorizing 'labelled' elements may not
    ## preserve their attributes

    ## Warning in bind_rows_(x, .id): Vectorizing 'labelled' elements may not
    ## preserve their attributes

    ## Warning in bind_rows_(x, .id): Vectorizing 'labelled' elements may not
    ## preserve their attributes

    ## Warning in bind_rows_(x, .id): Vectorizing 'labelled' elements may not
    ## preserve their attributes

    ## Warning in bind_rows_(x, .id): Vectorizing 'labelled' elements may not
    ## preserve their attributes

    ## Warning in bind_rows_(x, .id): Vectorizing 'labelled' elements may not
    ## preserve their attributes

    ## Warning in bind_rows_(x, .id): Vectorizing 'labelled' elements may not
    ## preserve their attributes

    ## Warning in bind_rows_(x, .id): Vectorizing 'labelled' elements may not
    ## preserve their attributes

    ## Warning in bind_rows_(x, .id): Vectorizing 'labelled' elements may not
    ## preserve their attributes

    ## Warning in bind_rows_(x, .id): Vectorizing 'labelled' elements may not
    ## preserve their attributes

    ## Warning in bind_rows_(x, .id): Vectorizing 'labelled' elements may not
    ## preserve their attributes

    ## Warning in bind_rows_(x, .id): Vectorizing 'labelled' elements may not
    ## preserve their attributes

    ## Warning in bind_rows_(x, .id): Vectorizing 'labelled' elements may not
    ## preserve their attributes

    ## Warning in bind_rows_(x, .id): Vectorizing 'labelled' elements may not
    ## preserve their attributes

    ## Warning in bind_rows_(x, .id): Vectorizing 'labelled' elements may not
    ## preserve their attributes

    ## Warning in bind_rows_(x, .id): Vectorizing 'labelled' elements may not
    ## preserve their attributes

    ## Warning in bind_rows_(x, .id): Vectorizing 'labelled' elements may not
    ## preserve their attributes

    ## Warning in bind_rows_(x, .id): Vectorizing 'labelled' elements may not
    ## preserve their attributes

    ## Warning in bind_rows_(x, .id): Vectorizing 'labelled' elements may not
    ## preserve their attributes

    ## Warning in bind_rows_(x, .id): Vectorizing 'labelled' elements may not
    ## preserve their attributes

    ## Warning in bind_rows_(x, .id): Vectorizing 'labelled' elements may not
    ## preserve their attributes

    ## Warning in bind_rows_(x, .id): Vectorizing 'labelled' elements may not
    ## preserve their attributes

    ## Warning in bind_rows_(x, .id): Vectorizing 'labelled' elements may not
    ## preserve their attributes

    ## Warning in bind_rows_(x, .id): Vectorizing 'labelled' elements may not
    ## preserve their attributes

    ## Warning in bind_rows_(x, .id): Vectorizing 'labelled' elements may not
    ## preserve their attributes

    ## Warning in bind_rows_(x, .id): Vectorizing 'labelled' elements may not
    ## preserve their attributes

    ## Warning in bind_rows_(x, .id): Vectorizing 'labelled' elements may not
    ## preserve their attributes

    ## Warning in bind_rows_(x, .id): Vectorizing 'labelled' elements may not
    ## preserve their attributes

    ## Warning in bind_rows_(x, .id): Vectorizing 'labelled' elements may not
    ## preserve their attributes

    ## Warning in bind_rows_(x, .id): Vectorizing 'labelled' elements may not
    ## preserve their attributes

    ## Warning in bind_rows_(x, .id): Vectorizing 'labelled' elements may not
    ## preserve their attributes

    ## Warning in bind_rows_(x, .id): Vectorizing 'labelled' elements may not
    ## preserve their attributes

    ## Warning in bind_rows_(x, .id): Vectorizing 'labelled' elements may not
    ## preserve their attributes

    ## Warning in bind_rows_(x, .id): Vectorizing 'labelled' elements may not
    ## preserve their attributes

    ## Warning in bind_rows_(x, .id): Vectorizing 'labelled' elements may not
    ## preserve their attributes

    ## Warning in bind_rows_(x, .id): Vectorizing 'labelled' elements may not
    ## preserve their attributes

    ## Warning in bind_rows_(x, .id): Vectorizing 'labelled' elements may not
    ## preserve their attributes

    ## Warning in bind_rows_(x, .id): Vectorizing 'labelled' elements may not
    ## preserve their attributes

    ## Warning in bind_rows_(x, .id): Vectorizing 'labelled' elements may not
    ## preserve their attributes

    ## Warning in bind_rows_(x, .id): Vectorizing 'labelled' elements may not
    ## preserve their attributes

    ## Warning in bind_rows_(x, .id): Vectorizing 'labelled' elements may not
    ## preserve their attributes

    ## Warning in bind_rows_(x, .id): Vectorizing 'labelled' elements may not
    ## preserve their attributes

    ## Warning in bind_rows_(x, .id): Vectorizing 'labelled' elements may not
    ## preserve their attributes

    ## Warning in bind_rows_(x, .id): Vectorizing 'labelled' elements may not
    ## preserve their attributes

    ## Warning in bind_rows_(x, .id): Vectorizing 'labelled' elements may not
    ## preserve their attributes

    ## Warning in bind_rows_(x, .id): Vectorizing 'labelled' elements may not
    ## preserve their attributes

    ## Warning in bind_rows_(x, .id): Vectorizing 'labelled' elements may not
    ## preserve their attributes

    ## Warning in bind_rows_(x, .id): Vectorizing 'labelled' elements may not
    ## preserve their attributes

    ## Warning in bind_rows_(x, .id): Vectorizing 'labelled' elements may not
    ## preserve their attributes

    ## Warning in bind_rows_(x, .id): Vectorizing 'labelled' elements may not
    ## preserve their attributes

    ## Warning in bind_rows_(x, .id): Vectorizing 'labelled' elements may not
    ## preserve their attributes

    ## Warning in bind_rows_(x, .id): Vectorizing 'labelled' elements may not
    ## preserve their attributes

    ## Warning in bind_rows_(x, .id): Vectorizing 'labelled' elements may not
    ## preserve their attributes

    ## Warning in bind_rows_(x, .id): Vectorizing 'labelled' elements may not
    ## preserve their attributes

    ## Warning in bind_rows_(x, .id): Vectorizing 'labelled' elements may not
    ## preserve their attributes

    ## Warning in bind_rows_(x, .id): Vectorizing 'labelled' elements may not
    ## preserve their attributes

    ## Warning in bind_rows_(x, .id): Vectorizing 'labelled' elements may not
    ## preserve their attributes

    ## Warning in bind_rows_(x, .id): Vectorizing 'labelled' elements may not
    ## preserve their attributes

    ## Warning in bind_rows_(x, .id): Vectorizing 'labelled' elements may not
    ## preserve their attributes

    ## Warning in bind_rows_(x, .id): Vectorizing 'labelled' elements may not
    ## preserve their attributes

    ## Warning in bind_rows_(x, .id): Vectorizing 'labelled' elements may not
    ## preserve their attributes

    ## Warning in bind_rows_(x, .id): Vectorizing 'labelled' elements may not
    ## preserve their attributes

    ## Warning in bind_rows_(x, .id): Vectorizing 'labelled' elements may not
    ## preserve their attributes

    ## Warning in bind_rows_(x, .id): Vectorizing 'labelled' elements may not
    ## preserve their attributes

    ## Warning in bind_rows_(x, .id): Vectorizing 'labelled' elements may not
    ## preserve their attributes

    ## Warning in bind_rows_(x, .id): Vectorizing 'labelled' elements may not
    ## preserve their attributes

    ## Warning in bind_rows_(x, .id): Vectorizing 'labelled' elements may not
    ## preserve their attributes

    ## Warning in bind_rows_(x, .id): Vectorizing 'labelled' elements may not
    ## preserve their attributes

    ## Warning in bind_rows_(x, .id): Vectorizing 'labelled' elements may not
    ## preserve their attributes

    ## Warning in bind_rows_(x, .id): Vectorizing 'labelled' elements may not
    ## preserve their attributes

    ## Warning in bind_rows_(x, .id): Vectorizing 'labelled' elements may not
    ## preserve their attributes

    ## Warning in bind_rows_(x, .id): Vectorizing 'labelled' elements may not
    ## preserve their attributes

    ## Warning in bind_rows_(x, .id): Vectorizing 'labelled' elements may not
    ## preserve their attributes

    ## Warning in bind_rows_(x, .id): Vectorizing 'labelled' elements may not
    ## preserve their attributes

    ## Warning in bind_rows_(x, .id): Vectorizing 'labelled' elements may not
    ## preserve their attributes

    ## Warning in bind_rows_(x, .id): Vectorizing 'labelled' elements may not
    ## preserve their attributes

    ## Warning in bind_rows_(x, .id): Vectorizing 'labelled' elements may not
    ## preserve their attributes

    ## Warning in bind_rows_(x, .id): Vectorizing 'labelled' elements may not
    ## preserve their attributes

    ## Warning in bind_rows_(x, .id): Vectorizing 'labelled' elements may not
    ## preserve their attributes

    ## Warning in bind_rows_(x, .id): Vectorizing 'labelled' elements may not
    ## preserve their attributes

    ## Warning in bind_rows_(x, .id): Vectorizing 'labelled' elements may not
    ## preserve their attributes

    ## Warning in bind_rows_(x, .id): Vectorizing 'labelled' elements may not
    ## preserve their attributes

    ## Warning in bind_rows_(x, .id): Vectorizing 'labelled' elements may not
    ## preserve their attributes

    ## Warning in bind_rows_(x, .id): Vectorizing 'labelled' elements may not
    ## preserve their attributes

    ## Warning in bind_rows_(x, .id): Vectorizing 'labelled' elements may not
    ## preserve their attributes

    ## Warning in bind_rows_(x, .id): Vectorizing 'labelled' elements may not
    ## preserve their attributes

    ## Warning in bind_rows_(x, .id): Vectorizing 'labelled' elements may not
    ## preserve their attributes

    ## Warning in bind_rows_(x, .id): Vectorizing 'labelled' elements may not
    ## preserve their attributes

    ## Warning in bind_rows_(x, .id): Vectorizing 'labelled' elements may not
    ## preserve their attributes

    ## Warning in bind_rows_(x, .id): Vectorizing 'labelled' elements may not
    ## preserve their attributes

    ## Warning in bind_rows_(x, .id): Vectorizing 'labelled' elements may not
    ## preserve their attributes

    ## Warning in bind_rows_(x, .id): Vectorizing 'labelled' elements may not
    ## preserve their attributes

    ## Warning in bind_rows_(x, .id): Vectorizing 'labelled' elements may not
    ## preserve their attributes

    ## Warning in bind_rows_(x, .id): Vectorizing 'labelled' elements may not
    ## preserve their attributes

    ## Warning in bind_rows_(x, .id): Vectorizing 'labelled' elements may not
    ## preserve their attributes

    ## Warning in bind_rows_(x, .id): Vectorizing 'labelled' elements may not
    ## preserve their attributes

    ## Warning in bind_rows_(x, .id): Vectorizing 'labelled' elements may not
    ## preserve their attributes

    ## Warning in bind_rows_(x, .id): Vectorizing 'labelled' elements may not
    ## preserve their attributes

    ## Warning in bind_rows_(x, .id): Vectorizing 'labelled' elements may not
    ## preserve their attributes

    ## Warning in bind_rows_(x, .id): Vectorizing 'labelled' elements may not
    ## preserve their attributes

    ## Warning in bind_rows_(x, .id): Vectorizing 'labelled' elements may not
    ## preserve their attributes

    ## Warning in bind_rows_(x, .id): Vectorizing 'labelled' elements may not
    ## preserve their attributes

    ## Warning in bind_rows_(x, .id): Vectorizing 'labelled' elements may not
    ## preserve their attributes

    ## Warning in bind_rows_(x, .id): Vectorizing 'labelled' elements may not
    ## preserve their attributes

    ## Warning in bind_rows_(x, .id): Vectorizing 'labelled' elements may not
    ## preserve their attributes

    ## Warning in bind_rows_(x, .id): Vectorizing 'labelled' elements may not
    ## preserve their attributes

    ## Warning in bind_rows_(x, .id): Vectorizing 'labelled' elements may not
    ## preserve their attributes

    ## Warning in bind_rows_(x, .id): Vectorizing 'labelled' elements may not
    ## preserve their attributes

    ## Warning in bind_rows_(x, .id): Vectorizing 'labelled' elements may not
    ## preserve their attributes

    ## Warning in bind_rows_(x, .id): Vectorizing 'labelled' elements may not
    ## preserve their attributes

    ## Warning in bind_rows_(x, .id): Vectorizing 'labelled' elements may not
    ## preserve their attributes

    ## Warning in bind_rows_(x, .id): Vectorizing 'labelled' elements may not
    ## preserve their attributes

    ## Warning in bind_rows_(x, .id): Vectorizing 'labelled' elements may not
    ## preserve their attributes

    ## Warning in bind_rows_(x, .id): Vectorizing 'labelled' elements may not
    ## preserve their attributes

    ## Warning in bind_rows_(x, .id): Vectorizing 'labelled' elements may not
    ## preserve their attributes

    ## Warning in bind_rows_(x, .id): Vectorizing 'labelled' elements may not
    ## preserve their attributes

    ## Warning in bind_rows_(x, .id): Vectorizing 'labelled' elements may not
    ## preserve their attributes

    ## Warning in bind_rows_(x, .id): Vectorizing 'labelled' elements may not
    ## preserve their attributes

    ## Warning in bind_rows_(x, .id): Vectorizing 'labelled' elements may not
    ## preserve their attributes

    ## Warning in bind_rows_(x, .id): Vectorizing 'labelled' elements may not
    ## preserve their attributes

    ## Warning in bind_rows_(x, .id): Vectorizing 'labelled' elements may not
    ## preserve their attributes

    ## Warning in bind_rows_(x, .id): Vectorizing 'labelled' elements may not
    ## preserve their attributes

    ## Warning in bind_rows_(x, .id): Vectorizing 'labelled' elements may not
    ## preserve their attributes

    ## Warning in bind_rows_(x, .id): Vectorizing 'labelled' elements may not
    ## preserve their attributes

    ## Warning in bind_rows_(x, .id): Vectorizing 'labelled' elements may not
    ## preserve their attributes

    ## Warning in bind_rows_(x, .id): Vectorizing 'labelled' elements may not
    ## preserve their attributes

    ## Warning in bind_rows_(x, .id): Vectorizing 'labelled' elements may not
    ## preserve their attributes

    ## Warning in bind_rows_(x, .id): Vectorizing 'labelled' elements may not
    ## preserve their attributes

    ## Warning in bind_rows_(x, .id): Vectorizing 'labelled' elements may not
    ## preserve their attributes

    ## Warning in bind_rows_(x, .id): Vectorizing 'labelled' elements may not
    ## preserve their attributes

    ## Warning in bind_rows_(x, .id): Vectorizing 'labelled' elements may not
    ## preserve their attributes

    ## Warning in bind_rows_(x, .id): Vectorizing 'labelled' elements may not
    ## preserve their attributes

    ## Warning in bind_rows_(x, .id): Vectorizing 'labelled' elements may not
    ## preserve their attributes

    ## Warning in bind_rows_(x, .id): Vectorizing 'labelled' elements may not
    ## preserve their attributes

    ## Warning in bind_rows_(x, .id): Vectorizing 'labelled' elements may not
    ## preserve their attributes

    ## Warning in bind_rows_(x, .id): Vectorizing 'labelled' elements may not
    ## preserve their attributes

    ## Warning in bind_rows_(x, .id): Vectorizing 'labelled' elements may not
    ## preserve their attributes

    ## Warning in bind_rows_(x, .id): Vectorizing 'labelled' elements may not
    ## preserve their attributes

    ## Warning in bind_rows_(x, .id): Vectorizing 'labelled' elements may not
    ## preserve their attributes

    ## Warning in bind_rows_(x, .id): Vectorizing 'labelled' elements may not
    ## preserve their attributes

    ## Warning in bind_rows_(x, .id): Vectorizing 'labelled' elements may not
    ## preserve their attributes

    ## Warning in bind_rows_(x, .id): Vectorizing 'labelled' elements may not
    ## preserve their attributes

    ## Warning in bind_rows_(x, .id): Vectorizing 'labelled' elements may not
    ## preserve their attributes

    ## Warning in bind_rows_(x, .id): Vectorizing 'labelled' elements may not
    ## preserve their attributes

    ## Warning in bind_rows_(x, .id): Vectorizing 'labelled' elements may not
    ## preserve their attributes

    ## Warning in bind_rows_(x, .id): Vectorizing 'labelled' elements may not
    ## preserve their attributes

    ## Warning in bind_rows_(x, .id): Vectorizing 'labelled' elements may not
    ## preserve their attributes

    ## Warning in bind_rows_(x, .id): Vectorizing 'labelled' elements may not
    ## preserve their attributes

    ## Warning in bind_rows_(x, .id): Vectorizing 'labelled' elements may not
    ## preserve their attributes

    ## Warning in bind_rows_(x, .id): Vectorizing 'labelled' elements may not
    ## preserve their attributes

    ## Warning in bind_rows_(x, .id): Vectorizing 'labelled' elements may not
    ## preserve their attributes

    ## Warning in bind_rows_(x, .id): Vectorizing 'labelled' elements may not
    ## preserve their attributes

    ## Warning in bind_rows_(x, .id): Vectorizing 'labelled' elements may not
    ## preserve their attributes

    ## Warning in bind_rows_(x, .id): Vectorizing 'labelled' elements may not
    ## preserve their attributes

    ## Warning in bind_rows_(x, .id): Vectorizing 'labelled' elements may not
    ## preserve their attributes

    ## Warning in bind_rows_(x, .id): Vectorizing 'labelled' elements may not
    ## preserve their attributes

    ## Warning in bind_rows_(x, .id): Vectorizing 'labelled' elements may not
    ## preserve their attributes

    ## Warning in bind_rows_(x, .id): Vectorizing 'labelled' elements may not
    ## preserve their attributes

    ## Warning in bind_rows_(x, .id): Vectorizing 'labelled' elements may not
    ## preserve their attributes

    ## Warning in bind_rows_(x, .id): Vectorizing 'labelled' elements may not
    ## preserve their attributes

    ## Warning in bind_rows_(x, .id): Vectorizing 'labelled' elements may not
    ## preserve their attributes

    ## Warning in bind_rows_(x, .id): Vectorizing 'labelled' elements may not
    ## preserve their attributes

    ## Warning in bind_rows_(x, .id): Vectorizing 'labelled' elements may not
    ## preserve their attributes

    ## Warning in bind_rows_(x, .id): Vectorizing 'labelled' elements may not
    ## preserve their attributes

    ## Warning in bind_rows_(x, .id): Vectorizing 'labelled' elements may not
    ## preserve their attributes

    ## Warning in bind_rows_(x, .id): Vectorizing 'labelled' elements may not
    ## preserve their attributes

    ## Warning in bind_rows_(x, .id): Vectorizing 'labelled' elements may not
    ## preserve their attributes

    ## Warning in bind_rows_(x, .id): Vectorizing 'labelled' elements may not
    ## preserve their attributes

    ## Warning in bind_rows_(x, .id): Vectorizing 'labelled' elements may not
    ## preserve their attributes

    ## Warning in bind_rows_(x, .id): Vectorizing 'labelled' elements may not
    ## preserve their attributes

    ## Warning in bind_rows_(x, .id): Vectorizing 'labelled' elements may not
    ## preserve their attributes

    ## Warning in bind_rows_(x, .id): Vectorizing 'labelled' elements may not
    ## preserve their attributes

    ## Warning in bind_rows_(x, .id): Vectorizing 'labelled' elements may not
    ## preserve their attributes

    ## Warning in bind_rows_(x, .id): Vectorizing 'labelled' elements may not
    ## preserve their attributes

    ## Warning in bind_rows_(x, .id): Vectorizing 'labelled' elements may not
    ## preserve their attributes

    ## Warning in bind_rows_(x, .id): Vectorizing 'labelled' elements may not
    ## preserve their attributes

    ## Warning in bind_rows_(x, .id): Vectorizing 'labelled' elements may not
    ## preserve their attributes

    ## Warning in bind_rows_(x, .id): Vectorizing 'labelled' elements may not
    ## preserve their attributes

    ## Warning in bind_rows_(x, .id): Vectorizing 'labelled' elements may not
    ## preserve their attributes

    ## Warning in bind_rows_(x, .id): Vectorizing 'labelled' elements may not
    ## preserve their attributes

    ## Warning in bind_rows_(x, .id): Vectorizing 'labelled' elements may not
    ## preserve their attributes

    ## Warning in bind_rows_(x, .id): Vectorizing 'labelled' elements may not
    ## preserve their attributes

    ## Warning in bind_rows_(x, .id): Vectorizing 'labelled' elements may not
    ## preserve their attributes

    ## Warning in bind_rows_(x, .id): Vectorizing 'labelled' elements may not
    ## preserve their attributes

    ## Warning in bind_rows_(x, .id): Vectorizing 'labelled' elements may not
    ## preserve their attributes

    ## Warning in bind_rows_(x, .id): Vectorizing 'labelled' elements may not
    ## preserve their attributes

    ## Warning in bind_rows_(x, .id): Vectorizing 'labelled' elements may not
    ## preserve their attributes

    ## Warning in bind_rows_(x, .id): Vectorizing 'labelled' elements may not
    ## preserve their attributes

    ## Warning in bind_rows_(x, .id): Vectorizing 'labelled' elements may not
    ## preserve their attributes

    ## Warning in bind_rows_(x, .id): Vectorizing 'labelled' elements may not
    ## preserve their attributes

    ## Warning in bind_rows_(x, .id): Vectorizing 'labelled' elements may not
    ## preserve their attributes

    ## Warning in bind_rows_(x, .id): Vectorizing 'labelled' elements may not
    ## preserve their attributes

    ## Warning in bind_rows_(x, .id): Vectorizing 'labelled' elements may not
    ## preserve their attributes

    ## Warning in bind_rows_(x, .id): Vectorizing 'labelled' elements may not
    ## preserve their attributes

    ## Warning in bind_rows_(x, .id): Vectorizing 'labelled' elements may not
    ## preserve their attributes

    ## Warning in bind_rows_(x, .id): Vectorizing 'labelled' elements may not
    ## preserve their attributes

    ## Warning in bind_rows_(x, .id): Vectorizing 'labelled' elements may not
    ## preserve their attributes

    ## Warning in bind_rows_(x, .id): Vectorizing 'labelled' elements may not
    ## preserve their attributes

    ## Warning in bind_rows_(x, .id): Vectorizing 'labelled' elements may not
    ## preserve their attributes

    ## Warning in bind_rows_(x, .id): Vectorizing 'labelled' elements may not
    ## preserve their attributes

    ## Warning in bind_rows_(x, .id): Vectorizing 'labelled' elements may not
    ## preserve their attributes

    ## Warning in bind_rows_(x, .id): Vectorizing 'labelled' elements may not
    ## preserve their attributes

    ## Warning in bind_rows_(x, .id): Vectorizing 'labelled' elements may not
    ## preserve their attributes

    ## Warning in bind_rows_(x, .id): Vectorizing 'labelled' elements may not
    ## preserve their attributes

    ## Warning in bind_rows_(x, .id): Vectorizing 'labelled' elements may not
    ## preserve their attributes

    ## Warning in bind_rows_(x, .id): Vectorizing 'labelled' elements may not
    ## preserve their attributes

    ## Warning in bind_rows_(x, .id): Vectorizing 'labelled' elements may not
    ## preserve their attributes

    ## Warning in bind_rows_(x, .id): Vectorizing 'labelled' elements may not
    ## preserve their attributes

    ## Warning in bind_rows_(x, .id): Vectorizing 'labelled' elements may not
    ## preserve their attributes

    ## Warning in bind_rows_(x, .id): Vectorizing 'labelled' elements may not
    ## preserve their attributes

    ## Warning in bind_rows_(x, .id): Vectorizing 'labelled' elements may not
    ## preserve their attributes

    ## Warning in bind_rows_(x, .id): Vectorizing 'labelled' elements may not
    ## preserve their attributes

    ## Warning in bind_rows_(x, .id): Vectorizing 'labelled' elements may not
    ## preserve their attributes

    ## Warning in bind_rows_(x, .id): Vectorizing 'labelled' elements may not
    ## preserve their attributes

    ## Warning in bind_rows_(x, .id): Vectorizing 'labelled' elements may not
    ## preserve their attributes

    ## Warning in bind_rows_(x, .id): Vectorizing 'labelled' elements may not
    ## preserve their attributes

    ## Warning in bind_rows_(x, .id): Vectorizing 'labelled' elements may not
    ## preserve their attributes

    ## Warning in bind_rows_(x, .id): Vectorizing 'labelled' elements may not
    ## preserve their attributes

    ## Warning in bind_rows_(x, .id): Vectorizing 'labelled' elements may not
    ## preserve their attributes

    ## Warning in bind_rows_(x, .id): Vectorizing 'labelled' elements may not
    ## preserve their attributes

    ## Warning in bind_rows_(x, .id): Vectorizing 'labelled' elements may not
    ## preserve their attributes

    ## Warning in bind_rows_(x, .id): Vectorizing 'labelled' elements may not
    ## preserve their attributes

    ## Warning in bind_rows_(x, .id): Vectorizing 'labelled' elements may not
    ## preserve their attributes

    ## Warning in bind_rows_(x, .id): Vectorizing 'labelled' elements may not
    ## preserve their attributes

    ## Warning in bind_rows_(x, .id): Vectorizing 'labelled' elements may not
    ## preserve their attributes

    ## Warning in bind_rows_(x, .id): Vectorizing 'labelled' elements may not
    ## preserve their attributes

    ## Warning in bind_rows_(x, .id): Vectorizing 'labelled' elements may not
    ## preserve their attributes

    ## Warning in bind_rows_(x, .id): Vectorizing 'labelled' elements may not
    ## preserve their attributes

    ## Warning in bind_rows_(x, .id): Vectorizing 'labelled' elements may not
    ## preserve their attributes

    ## Warning in bind_rows_(x, .id): Vectorizing 'labelled' elements may not
    ## preserve their attributes

    ## Warning in bind_rows_(x, .id): Vectorizing 'labelled' elements may not
    ## preserve their attributes

    ## Warning in bind_rows_(x, .id): Vectorizing 'labelled' elements may not
    ## preserve their attributes

    ## Warning in bind_rows_(x, .id): Vectorizing 'labelled' elements may not
    ## preserve their attributes

    ## Warning in bind_rows_(x, .id): Vectorizing 'labelled' elements may not
    ## preserve their attributes

    ## Warning in bind_rows_(x, .id): Vectorizing 'labelled' elements may not
    ## preserve their attributes

    ## Warning in bind_rows_(x, .id): Vectorizing 'labelled' elements may not
    ## preserve their attributes

    ## Warning in bind_rows_(x, .id): Vectorizing 'labelled' elements may not
    ## preserve their attributes

    ## Warning in bind_rows_(x, .id): Vectorizing 'labelled' elements may not
    ## preserve their attributes

    ## Warning in bind_rows_(x, .id): Vectorizing 'labelled' elements may not
    ## preserve their attributes

    ## Warning in bind_rows_(x, .id): Vectorizing 'labelled' elements may not
    ## preserve their attributes

    ## Warning in bind_rows_(x, .id): Vectorizing 'labelled' elements may not
    ## preserve their attributes

    ## Warning in bind_rows_(x, .id): Vectorizing 'labelled' elements may not
    ## preserve their attributes

    ## Warning in bind_rows_(x, .id): Vectorizing 'labelled' elements may not
    ## preserve their attributes

    ## Warning in bind_rows_(x, .id): Vectorizing 'labelled' elements may not
    ## preserve their attributes

    ## Warning in bind_rows_(x, .id): Vectorizing 'labelled' elements may not
    ## preserve their attributes

    ## Warning in bind_rows_(x, .id): Vectorizing 'labelled' elements may not
    ## preserve their attributes

    ## Warning in bind_rows_(x, .id): Vectorizing 'labelled' elements may not
    ## preserve their attributes

    ## Warning in bind_rows_(x, .id): Vectorizing 'labelled' elements may not
    ## preserve their attributes

    ## Warning in bind_rows_(x, .id): Vectorizing 'labelled' elements may not
    ## preserve their attributes

    ## Warning in bind_rows_(x, .id): Vectorizing 'labelled' elements may not
    ## preserve their attributes

    ## Warning in bind_rows_(x, .id): Vectorizing 'labelled' elements may not
    ## preserve their attributes

    ## Warning in bind_rows_(x, .id): Vectorizing 'labelled' elements may not
    ## preserve their attributes

    ## Warning in bind_rows_(x, .id): Vectorizing 'labelled' elements may not
    ## preserve their attributes

    ## Warning in bind_rows_(x, .id): Vectorizing 'labelled' elements may not
    ## preserve their attributes

    ## Warning in bind_rows_(x, .id): Vectorizing 'labelled' elements may not
    ## preserve their attributes

    ## Warning in bind_rows_(x, .id): Vectorizing 'labelled' elements may not
    ## preserve their attributes

    ## Warning in bind_rows_(x, .id): Vectorizing 'labelled' elements may not
    ## preserve their attributes

    ## Warning in bind_rows_(x, .id): Vectorizing 'labelled' elements may not
    ## preserve their attributes

    ## Warning in bind_rows_(x, .id): Vectorizing 'labelled' elements may not
    ## preserve their attributes

    ## Warning in bind_rows_(x, .id): Vectorizing 'labelled' elements may not
    ## preserve their attributes

    ## Warning in bind_rows_(x, .id): Vectorizing 'labelled' elements may not
    ## preserve their attributes

    ## Warning in bind_rows_(x, .id): Vectorizing 'labelled' elements may not
    ## preserve their attributes

    ## Warning in bind_rows_(x, .id): Vectorizing 'labelled' elements may not
    ## preserve their attributes

    ## Warning in bind_rows_(x, .id): Vectorizing 'labelled' elements may not
    ## preserve their attributes

    ## Warning in bind_rows_(x, .id): Vectorizing 'labelled' elements may not
    ## preserve their attributes

    ## Warning in bind_rows_(x, .id): Vectorizing 'labelled' elements may not
    ## preserve their attributes

    ## Warning in bind_rows_(x, .id): Vectorizing 'labelled' elements may not
    ## preserve their attributes

    ## Warning in bind_rows_(x, .id): Vectorizing 'labelled' elements may not
    ## preserve their attributes

    ## Warning in bind_rows_(x, .id): Vectorizing 'labelled' elements may not
    ## preserve their attributes

    ## Warning in bind_rows_(x, .id): Vectorizing 'labelled' elements may not
    ## preserve their attributes

    ## Warning in bind_rows_(x, .id): Vectorizing 'labelled' elements may not
    ## preserve their attributes

    ## Warning in bind_rows_(x, .id): Vectorizing 'labelled' elements may not
    ## preserve their attributes

    ## Warning in bind_rows_(x, .id): Vectorizing 'labelled' elements may not
    ## preserve their attributes

    ## Warning in bind_rows_(x, .id): Vectorizing 'labelled' elements may not
    ## preserve their attributes

    ## Warning in bind_rows_(x, .id): Vectorizing 'labelled' elements may not
    ## preserve their attributes

    ## Warning in bind_rows_(x, .id): Vectorizing 'labelled' elements may not
    ## preserve their attributes

    ## Warning in bind_rows_(x, .id): Vectorizing 'labelled' elements may not
    ## preserve their attributes

    ## Warning in bind_rows_(x, .id): Vectorizing 'labelled' elements may not
    ## preserve their attributes

    ## Warning in bind_rows_(x, .id): Vectorizing 'labelled' elements may not
    ## preserve their attributes

    ## Warning in bind_rows_(x, .id): Vectorizing 'labelled' elements may not
    ## preserve their attributes

    ## Warning in bind_rows_(x, .id): Vectorizing 'labelled' elements may not
    ## preserve their attributes

    ## Warning in bind_rows_(x, .id): Vectorizing 'labelled' elements may not
    ## preserve their attributes

    ## Warning in bind_rows_(x, .id): Vectorizing 'labelled' elements may not
    ## preserve their attributes

    ## Warning in bind_rows_(x, .id): Vectorizing 'labelled' elements may not
    ## preserve their attributes

    ## Warning in bind_rows_(x, .id): Vectorizing 'labelled' elements may not
    ## preserve their attributes

    ## Warning in bind_rows_(x, .id): Vectorizing 'labelled' elements may not
    ## preserve their attributes

    ## Warning in bind_rows_(x, .id): Vectorizing 'labelled' elements may not
    ## preserve their attributes

    ## Warning in bind_rows_(x, .id): Vectorizing 'labelled' elements may not
    ## preserve their attributes

    ## Warning in bind_rows_(x, .id): Vectorizing 'labelled' elements may not
    ## preserve their attributes

    ## Warning in bind_rows_(x, .id): Vectorizing 'labelled' elements may not
    ## preserve their attributes

    ## Warning in bind_rows_(x, .id): Vectorizing 'labelled' elements may not
    ## preserve their attributes

    ## Warning in bind_rows_(x, .id): Vectorizing 'labelled' elements may not
    ## preserve their attributes

    ## Warning in bind_rows_(x, .id): Vectorizing 'labelled' elements may not
    ## preserve their attributes

    ## Warning in bind_rows_(x, .id): Vectorizing 'labelled' elements may not
    ## preserve their attributes

    ## Warning in bind_rows_(x, .id): Vectorizing 'labelled' elements may not
    ## preserve their attributes

    ## Warning in bind_rows_(x, .id): Vectorizing 'labelled' elements may not
    ## preserve their attributes

    ## Warning in bind_rows_(x, .id): Vectorizing 'labelled' elements may not
    ## preserve their attributes

    ## Warning in bind_rows_(x, .id): Vectorizing 'labelled' elements may not
    ## preserve their attributes

    ## Warning in bind_rows_(x, .id): Vectorizing 'labelled' elements may not
    ## preserve their attributes

    ## Warning in bind_rows_(x, .id): Vectorizing 'labelled' elements may not
    ## preserve their attributes

    ## Warning in bind_rows_(x, .id): Vectorizing 'labelled' elements may not
    ## preserve their attributes

    ## Warning in bind_rows_(x, .id): Vectorizing 'labelled' elements may not
    ## preserve their attributes

    ## Warning in bind_rows_(x, .id): Vectorizing 'labelled' elements may not
    ## preserve their attributes

    ## Warning in bind_rows_(x, .id): Vectorizing 'labelled' elements may not
    ## preserve their attributes

    ## Warning in bind_rows_(x, .id): Vectorizing 'labelled' elements may not
    ## preserve their attributes

    ## Warning in bind_rows_(x, .id): Vectorizing 'labelled' elements may not
    ## preserve their attributes

    ## Warning in bind_rows_(x, .id): Vectorizing 'labelled' elements may not
    ## preserve their attributes

    ## Warning in bind_rows_(x, .id): Vectorizing 'labelled' elements may not
    ## preserve their attributes

    ## Warning in bind_rows_(x, .id): Vectorizing 'labelled' elements may not
    ## preserve their attributes

    ## Warning in bind_rows_(x, .id): Vectorizing 'labelled' elements may not
    ## preserve their attributes

    ## Warning in bind_rows_(x, .id): Vectorizing 'labelled' elements may not
    ## preserve their attributes

    ## Warning in bind_rows_(x, .id): Vectorizing 'labelled' elements may not
    ## preserve their attributes

    ## Warning in bind_rows_(x, .id): Vectorizing 'labelled' elements may not
    ## preserve their attributes

    ## Warning in bind_rows_(x, .id): Vectorizing 'labelled' elements may not
    ## preserve their attributes

    ## Warning in bind_rows_(x, .id): Vectorizing 'labelled' elements may not
    ## preserve their attributes

    ## Warning in bind_rows_(x, .id): Vectorizing 'labelled' elements may not
    ## preserve their attributes

    ## Warning in bind_rows_(x, .id): Vectorizing 'labelled' elements may not
    ## preserve their attributes

    ## Warning in bind_rows_(x, .id): Vectorizing 'labelled' elements may not
    ## preserve their attributes

    ## Warning in bind_rows_(x, .id): Vectorizing 'labelled' elements may not
    ## preserve their attributes

    ## Warning in bind_rows_(x, .id): Vectorizing 'labelled' elements may not
    ## preserve their attributes

    ## Warning in bind_rows_(x, .id): Vectorizing 'labelled' elements may not
    ## preserve their attributes

    ## Warning in bind_rows_(x, .id): Vectorizing 'labelled' elements may not
    ## preserve their attributes

    ## Warning in bind_rows_(x, .id): Vectorizing 'labelled' elements may not
    ## preserve their attributes

    ## Warning in bind_rows_(x, .id): Vectorizing 'labelled' elements may not
    ## preserve their attributes

    ## Warning in bind_rows_(x, .id): Vectorizing 'labelled' elements may not
    ## preserve their attributes

    ## Warning in bind_rows_(x, .id): Vectorizing 'labelled' elements may not
    ## preserve their attributes

    ## Warning in bind_rows_(x, .id): Vectorizing 'labelled' elements may not
    ## preserve their attributes

    ## Warning in bind_rows_(x, .id): Vectorizing 'labelled' elements may not
    ## preserve their attributes

    ## Warning in bind_rows_(x, .id): Vectorizing 'labelled' elements may not
    ## preserve their attributes

    ## Warning in bind_rows_(x, .id): Vectorizing 'labelled' elements may not
    ## preserve their attributes

    ## Warning in bind_rows_(x, .id): Vectorizing 'labelled' elements may not
    ## preserve their attributes

    ## Warning in bind_rows_(x, .id): Vectorizing 'labelled' elements may not
    ## preserve their attributes

    ## Warning in bind_rows_(x, .id): Vectorizing 'labelled' elements may not
    ## preserve their attributes

    ## Warning in bind_rows_(x, .id): Vectorizing 'labelled' elements may not
    ## preserve their attributes

    ## Warning in bind_rows_(x, .id): Vectorizing 'labelled' elements may not
    ## preserve their attributes

    ## Warning in bind_rows_(x, .id): Vectorizing 'labelled' elements may not
    ## preserve their attributes

    ## Warning in bind_rows_(x, .id): Vectorizing 'labelled' elements may not
    ## preserve their attributes

    ## Warning in bind_rows_(x, .id): Vectorizing 'labelled' elements may not
    ## preserve their attributes

    ## Warning in bind_rows_(x, .id): Vectorizing 'labelled' elements may not
    ## preserve their attributes

    ## Warning in bind_rows_(x, .id): Vectorizing 'labelled' elements may not
    ## preserve their attributes

    ## Warning in bind_rows_(x, .id): Vectorizing 'labelled' elements may not
    ## preserve their attributes

    ## Warning in bind_rows_(x, .id): Vectorizing 'labelled' elements may not
    ## preserve their attributes

    ## Warning in bind_rows_(x, .id): Vectorizing 'labelled' elements may not
    ## preserve their attributes

    ## Warning in bind_rows_(x, .id): Vectorizing 'labelled' elements may not
    ## preserve their attributes

    ## Warning in bind_rows_(x, .id): Vectorizing 'labelled' elements may not
    ## preserve their attributes

    ## Warning in bind_rows_(x, .id): Vectorizing 'labelled' elements may not
    ## preserve their attributes

    ## Warning in bind_rows_(x, .id): Vectorizing 'labelled' elements may not
    ## preserve their attributes

    ## Warning in bind_rows_(x, .id): Vectorizing 'labelled' elements may not
    ## preserve their attributes

    ## Warning in bind_rows_(x, .id): Vectorizing 'labelled' elements may not
    ## preserve their attributes

    ## Warning in bind_rows_(x, .id): Vectorizing 'labelled' elements may not
    ## preserve their attributes

    ## Warning in bind_rows_(x, .id): Vectorizing 'labelled' elements may not
    ## preserve their attributes

    ## Warning in bind_rows_(x, .id): Vectorizing 'labelled' elements may not
    ## preserve their attributes

    ## Warning in bind_rows_(x, .id): Vectorizing 'labelled' elements may not
    ## preserve their attributes

    ## Warning in bind_rows_(x, .id): Vectorizing 'labelled' elements may not
    ## preserve their attributes

    ## Warning in bind_rows_(x, .id): Vectorizing 'labelled' elements may not
    ## preserve their attributes

    ## Warning in bind_rows_(x, .id): Vectorizing 'labelled' elements may not
    ## preserve their attributes

    ## Warning in bind_rows_(x, .id): Vectorizing 'labelled' elements may not
    ## preserve their attributes

    ## Warning in bind_rows_(x, .id): Vectorizing 'labelled' elements may not
    ## preserve their attributes

    ## Warning in bind_rows_(x, .id): Vectorizing 'labelled' elements may not
    ## preserve their attributes

    ## Warning in bind_rows_(x, .id): Vectorizing 'labelled' elements may not
    ## preserve their attributes

    ## Warning in bind_rows_(x, .id): Vectorizing 'labelled' elements may not
    ## preserve their attributes

    ## Warning in bind_rows_(x, .id): Vectorizing 'labelled' elements may not
    ## preserve their attributes

    ## Warning in bind_rows_(x, .id): Vectorizing 'labelled' elements may not
    ## preserve their attributes

    ## Warning in bind_rows_(x, .id): Vectorizing 'labelled' elements may not
    ## preserve their attributes

    ## Warning in bind_rows_(x, .id): Vectorizing 'labelled' elements may not
    ## preserve their attributes

    ## Warning in bind_rows_(x, .id): Vectorizing 'labelled' elements may not
    ## preserve their attributes

    ## Warning in bind_rows_(x, .id): Vectorizing 'labelled' elements may not
    ## preserve their attributes

    ## Warning in bind_rows_(x, .id): Vectorizing 'labelled' elements may not
    ## preserve their attributes

    ## Warning in bind_rows_(x, .id): Vectorizing 'labelled' elements may not
    ## preserve their attributes

    ## Warning in bind_rows_(x, .id): Vectorizing 'labelled' elements may not
    ## preserve their attributes

    ## Warning in bind_rows_(x, .id): Vectorizing 'labelled' elements may not
    ## preserve their attributes

    ## Warning in bind_rows_(x, .id): Vectorizing 'labelled' elements may not
    ## preserve their attributes

    ## Warning in bind_rows_(x, .id): Vectorizing 'labelled' elements may not
    ## preserve their attributes

    ## Warning in bind_rows_(x, .id): Vectorizing 'labelled' elements may not
    ## preserve their attributes

    ## Warning in bind_rows_(x, .id): Vectorizing 'labelled' elements may not
    ## preserve their attributes

    ## Warning in bind_rows_(x, .id): Vectorizing 'labelled' elements may not
    ## preserve their attributes

    ## Warning in bind_rows_(x, .id): Vectorizing 'labelled' elements may not
    ## preserve their attributes

    ## Warning in bind_rows_(x, .id): Vectorizing 'labelled' elements may not
    ## preserve their attributes

    ## Warning in bind_rows_(x, .id): Vectorizing 'labelled' elements may not
    ## preserve their attributes

    ## Warning in bind_rows_(x, .id): Vectorizing 'labelled' elements may not
    ## preserve their attributes

    ## Warning in bind_rows_(x, .id): Vectorizing 'labelled' elements may not
    ## preserve their attributes

    ## Warning in bind_rows_(x, .id): Vectorizing 'labelled' elements may not
    ## preserve their attributes

    ## Warning in bind_rows_(x, .id): Vectorizing 'labelled' elements may not
    ## preserve their attributes

    ## Warning in bind_rows_(x, .id): Vectorizing 'labelled' elements may not
    ## preserve their attributes

    ## Warning in bind_rows_(x, .id): Vectorizing 'labelled' elements may not
    ## preserve their attributes

    ## Warning in bind_rows_(x, .id): Vectorizing 'labelled' elements may not
    ## preserve their attributes

    ## Warning in bind_rows_(x, .id): Vectorizing 'labelled' elements may not
    ## preserve their attributes

    ## Warning in bind_rows_(x, .id): Vectorizing 'labelled' elements may not
    ## preserve their attributes

    ## Warning in bind_rows_(x, .id): Vectorizing 'labelled' elements may not
    ## preserve their attributes

    ## Warning in bind_rows_(x, .id): Vectorizing 'labelled' elements may not
    ## preserve their attributes

    ## Warning in bind_rows_(x, .id): Vectorizing 'labelled' elements may not
    ## preserve their attributes

    ## Warning in bind_rows_(x, .id): Vectorizing 'labelled' elements may not
    ## preserve their attributes

    ## Warning in bind_rows_(x, .id): Vectorizing 'labelled' elements may not
    ## preserve their attributes

    ## Warning in bind_rows_(x, .id): Vectorizing 'labelled' elements may not
    ## preserve their attributes

    ## Warning in bind_rows_(x, .id): Vectorizing 'labelled' elements may not
    ## preserve their attributes

    ## Warning in bind_rows_(x, .id): Vectorizing 'labelled' elements may not
    ## preserve their attributes

    ## Warning in bind_rows_(x, .id): Vectorizing 'labelled' elements may not
    ## preserve their attributes

    ## Warning in bind_rows_(x, .id): Vectorizing 'labelled' elements may not
    ## preserve their attributes

    ## Warning in bind_rows_(x, .id): Vectorizing 'labelled' elements may not
    ## preserve their attributes

    ## Warning in bind_rows_(x, .id): Vectorizing 'labelled' elements may not
    ## preserve their attributes

    ## Warning in bind_rows_(x, .id): Vectorizing 'labelled' elements may not
    ## preserve their attributes

    ## Warning in bind_rows_(x, .id): Vectorizing 'labelled' elements may not
    ## preserve their attributes

    ## Warning in bind_rows_(x, .id): Vectorizing 'labelled' elements may not
    ## preserve their attributes

    ## Warning in bind_rows_(x, .id): Vectorizing 'labelled' elements may not
    ## preserve their attributes

    ## Warning in bind_rows_(x, .id): Vectorizing 'labelled' elements may not
    ## preserve their attributes

    ## Warning in bind_rows_(x, .id): Vectorizing 'labelled' elements may not
    ## preserve their attributes

    ## Warning in bind_rows_(x, .id): Vectorizing 'labelled' elements may not
    ## preserve their attributes

    ## Warning in bind_rows_(x, .id): Vectorizing 'labelled' elements may not
    ## preserve their attributes

    ## Warning in bind_rows_(x, .id): Vectorizing 'labelled' elements may not
    ## preserve their attributes

    ## Warning in bind_rows_(x, .id): Vectorizing 'labelled' elements may not
    ## preserve their attributes

    ## Warning in bind_rows_(x, .id): Vectorizing 'labelled' elements may not
    ## preserve their attributes

    ## Warning in bind_rows_(x, .id): Vectorizing 'labelled' elements may not
    ## preserve their attributes

    ## Warning in bind_rows_(x, .id): Vectorizing 'labelled' elements may not
    ## preserve their attributes

    ## Warning in bind_rows_(x, .id): Vectorizing 'labelled' elements may not
    ## preserve their attributes

    ## Warning in bind_rows_(x, .id): Vectorizing 'labelled' elements may not
    ## preserve their attributes

    ## Warning in bind_rows_(x, .id): Vectorizing 'labelled' elements may not
    ## preserve their attributes

    ## Warning in bind_rows_(x, .id): Vectorizing 'labelled' elements may not
    ## preserve their attributes

    ## Warning in bind_rows_(x, .id): Vectorizing 'labelled' elements may not
    ## preserve their attributes

    ## Warning in bind_rows_(x, .id): Vectorizing 'labelled' elements may not
    ## preserve their attributes

    ## Warning in bind_rows_(x, .id): Vectorizing 'labelled' elements may not
    ## preserve their attributes

    ## Warning in bind_rows_(x, .id): Vectorizing 'labelled' elements may not
    ## preserve their attributes

    ## Warning in bind_rows_(x, .id): Vectorizing 'labelled' elements may not
    ## preserve their attributes

    ## Warning in bind_rows_(x, .id): Vectorizing 'labelled' elements may not
    ## preserve their attributes

    ## Warning in bind_rows_(x, .id): Vectorizing 'labelled' elements may not
    ## preserve their attributes

    ## Warning in bind_rows_(x, .id): Vectorizing 'labelled' elements may not
    ## preserve their attributes

    ## Warning in bind_rows_(x, .id): Vectorizing 'labelled' elements may not
    ## preserve their attributes

    ## Warning in bind_rows_(x, .id): Vectorizing 'labelled' elements may not
    ## preserve their attributes

    ## Warning in bind_rows_(x, .id): Vectorizing 'labelled' elements may not
    ## preserve their attributes

    ## Warning in bind_rows_(x, .id): Vectorizing 'labelled' elements may not
    ## preserve their attributes

    ## Warning in bind_rows_(x, .id): Vectorizing 'labelled' elements may not
    ## preserve their attributes

    ## Warning in bind_rows_(x, .id): Vectorizing 'labelled' elements may not
    ## preserve their attributes

    ## Warning in bind_rows_(x, .id): Vectorizing 'labelled' elements may not
    ## preserve their attributes

    ## Warning in bind_rows_(x, .id): Vectorizing 'labelled' elements may not
    ## preserve their attributes

    ## Warning in bind_rows_(x, .id): Vectorizing 'labelled' elements may not
    ## preserve their attributes

    ## Warning in bind_rows_(x, .id): Vectorizing 'labelled' elements may not
    ## preserve their attributes

    ## Warning in bind_rows_(x, .id): Vectorizing 'labelled' elements may not
    ## preserve their attributes

    ## Warning in bind_rows_(x, .id): Vectorizing 'labelled' elements may not
    ## preserve their attributes

``` r
dim(ess_all)
```

    ## [1] 58618  1356

``` r
step1 <- ess_all %>%
    select(idno, cntry, cname, essround, hinctnta, polintr, lrscale,
         rlgatnd, pbldmn, ctzcntr, rlgatnd, edulvlb, eduyrs, yrbrn, gndr,
         dplyr::starts_with("trst"),
         dplyr::starts_with("stf"),
         #dplyr::starts_with("im"),
         imbgeco, imsmetn, imdfetn, impcntr, vote, vote_id
         #dplyr::starts_with("prtvt")
    )

table(step1$vote_id)
```

    ## 
    ##             1     10 101010 101044 101313 102828 104444   1057     11 
    ##  23372   7951   1302      1      1      1      1      1      1   1612 
    ##    111 111717    113   1133   1144 114444   1155    118    119     12 
    ##     44      1      4      5     36      1     10      3      1    521 
    ## 121544  12288  12344   1255     13 131716 134444  13855     14   1411 
    ##      1      1      1      1    430      1      1      1    119      1 
    ##   1417 141717 141733 141744  14175 141755 142544 144433  14444 144444 
    ##      2     16      2     10      1      4      1      1     12      1 
    ## 145544 145555     15  15133 151844   1533   1544 154444  15544  15555 
    ##      1      3    187      1      1      2      1      2      1      1 
    ##     16 161919 161944 164444  16448   1688     17     18 182144     19 
    ##    143      5      3      3      1      1      5     50      1     14 
    ##      2     20     21     22   2244     23     24     25     26     27 
    ##   5650     44     54    390      1     33     73     12      4      4 
    ##     28   2828     29      3     31   3144     32    321     33    331 
    ##      1      1      5   4138      4      1     10      1    136      3 
    ##   3311   3317    333   3333   3344    335   3355    337    338    339 
    ##      1      1     85      3     47      1      8      1      2      3 
    ##     34    343   3443  34444   3553  35555     36     37   3721    377 
    ##      5      1      3     16      1      8      2      3      1      1 
    ##     38   3811    388     39      4     41     42     43    433   4344 
    ##      1      2      1      2   2735     12     33      7      1      1 
    ##     44  44144   4444 444444  44544 445544  44811  44933     45     46 
    ##    130      1      5      6      1      1      1      1      5      1 
    ##     47     49      5     51    511    515     53     54  54444   5445 
    ##      3      4   2710      9      2      1      2      2      8      1 
    ##  54455     55    551  55155   5533    554   5544 554444    555  55544 
    ##      1     58      1      1      6      1     40      1     76      2 
    ##   5555 555544  55555 555555    558  55855    559     56     59      6 
    ##     18      1      3     12      2      1      1      2      1   2060 
    ##     61     62     64    644  64444  64455   6544    655  65555     66 
    ##      8      3      1      1      2      1      1      2      1     35 
    ##   6644    666     68      7     71    711     72     73    733     74 
    ##      1      5      3   1542      1      1      1      1      1      2 
    ##  74444  74455     75  75533  75555     77    771   7711   7719    773 
    ##      4      2      1      1      6      8      1      1      1      2 
    ##   7744   7755    777      8   8111  81111   8113  81133  81144  81155 
    ##     15     10     20   1172      1      9      1      1     13      1 
    ##   8118   8119     82  82844    833   8344    837  84444     85    855 
    ##     11      2      2      1      2      2      1     22      1      1 
    ##  85555     88    881   8811   8833   8844   8855    888    899      9 
    ##      4      6      3     16      9     57     21    131      2    682 
    ##     91    911   9144     92  92828     93   9333  94411  94444     95 
    ##      3      2      1      4      2      1      1      1      8      2 
    ##    955  95533  95544  95555     97   9833     99   9911   9933   9944 
    ##      1      1      2      4      2      1     10      2      5     23 
    ##    995   9955    998    999 
    ##      1      7      4     34

``` r
step2 <- step1 %>%
    mutate(country = countrycode::countrycode(cname, "country.name", "country.name")) %>%
    mutate(iso2 = countrycode::countrycode(country, "country.name", "iso2c")) %>%
    mutate(iso3 = countrycode::countrycode(cname, "country.name", "iso3c")) %>%
    select(-cntry, -cname)
colnames(step2)
```

    ##  [1] "idno"     "essround" "hinctnta" "polintr"  "lrscale"  "rlgatnd" 
    ##  [7] "pbldmn"   "ctzcntr"  "edulvlb"  "eduyrs"   "yrbrn"    "gndr"    
    ## [13] "trstprl"  "trstlgl"  "trstplc"  "trstplt"  "trstprt"  "trstep"  
    ## [19] "trstun"   "stflife"  "stfeco"   "stfgov"   "stfdem"   "stfedu"  
    ## [25] "stfhlth"  "stfjb"    "stfjbot"  "imbgeco"  "imsmetn"  "imdfetn" 
    ## [31] "impcntr"  "vote"     "vote_id"  "country"  "iso2"     "iso3"

``` r
step3 <- step2 %>%
    rename(
      id = idno, 
      round = essround, 
      income = hinctnta,
      pol_inter = polintr,
      gndr = gndr,
      year = yrbrn,
      edu_year = eduyrs,
      edu = edulvlb,
      rel = rlgatnd,
      demo = pbldmn,
      citz = ctzcntr,
      trust_parl = trstprl, 
      trust_pol = trstplt,
      trust_party = trstprt,
      trust_leg = trstlgl,
      trust_police = trstplc,
      trust_eu = trstep,
      s_life = stflife,
      s_econ = stfeco,
      s_gov = stfgov,
      s_dem = stfdem,
      imm_econ = imbgeco,
      imm_same = imsmetn,
      imm_diff = imdfetn,
      imm_poor = impcntr
    )

glimpse(step3)
```

    ## Observations: 58,618
    ## Variables: 36
    ## $ id           <dbl> 1, 2, 4, 6, 10, 11, 12, 13, 14, 15, 16, 17, 18, 1...
    ## $ round        <dbl> 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8...
    ## $ income       <dbl> NA, 5, 2, 4, 2, 10, 2, 8, 3, 4, 1, 3, 5, NA, 3, 5...
    ## $ pol_inter    <dbl> 1, 1, 3, 2, 3, 2, 3, 3, 4, 2, 3, 1, 2, 3, 3, 3, 2...
    ## $ lrscale      <dbl> 0, 1, 5, 0, 5, 5, 4, 5, 5, 5, 5, 8, 8, 5, NA, 6, ...
    ## $ rel          <dbl> 7, 7, 6, 6, 5, 5, 5, 5, 7, 5, 3, 7, 4, 5, 5, 5, 7...
    ## $ demo         <dbl> 2, 2, 2, 2, 2, 2, 2, 2, 2, 1, 2, 2, 2, 2, 2, 2, 2...
    ## $ citz         <dbl> 2, 2, 1, 1, 2, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1...
    ## $ edu          <dbl> 720, 313, 322, 322, 322, 313, 212, 423, 322, 322,...
    ## $ edu_year     <dbl> 21, 16, 13, 12, 13, 13, 8, 17, 14, 16, 9, 11, 12,...
    ## $ year         <dbl> 1982, 1964, 1948, 1962, 1996, 1951, 1964, 1972, 1...
    ## $ gndr         <dbl> 2, 1, 2, 1, 2, 2, 2, 2, 2, 2, 2, 1, 1, 2, 2, 1, 1...
    ## $ trust_parl   <dbl> 6, 5, 3, 1, 7, 4, 5, 6, 10, 7, 6, 0, 2, 6, 9, 8, ...
    ## $ trust_leg    <dbl> 7, 4, 5, 1, 7, 6, 6, 7, 10, 8, 5, 5, 8, 7, 9, 8, ...
    ## $ trust_police <dbl> 0, 3, 9, 2, 10, 7, 7, 9, 10, 7, 5, 7, 7, 7, 9, 10...
    ## $ trust_pol    <dbl> 2, 3, 3, 1, 6, 3, 2, 5, 9, 4, 3, 0, 6, 5, 7, 8, 5...
    ## $ trust_party  <dbl> 8, 4, 3, 1, 7, 3, 2, 3, 8, 4, 3, 0, 6, 5, 6, 8, 5...
    ## $ trust_eu     <dbl> 9, 6, 4, 1, 9, 2, NA, 4, 8, 1, 3, 0, 5, 7, 6, 8, ...
    ## $ trstun       <dbl> 10, 7, 5, 1, 10, 4, NA, 6, 10, 0, NA, 0, 4, 6, 3,...
    ## $ s_life       <dbl> 5, 5, 9, 7, 10, 7, 5, 9, 10, 7, 8, 7, 6, 8, 3, 8,...
    ## $ s_econ       <dbl> 4, 1, 6, 5, 7, 3, 5, 6, 10, 5, 0, 5, 6, 5, 5, 6, ...
    ## $ s_gov        <dbl> 4, 3, 3, 6, 5, 5, 6, 5, 10, 3, 4, 0, 2, 4, 6, 7, ...
    ## $ s_dem        <dbl> 6, 3, 6, 6, 9, 5, 7, 8, 10, 7, 5, 3, 8, 5, 8, 8, ...
    ## $ stfedu       <dbl> 3, 0, 8, 7, 10, 4, 4, 7, 9, 8, 10, 4, 5, 6, 3, 8,...
    ## $ stfhlth      <dbl> 5, 3, 6, 6, 10, 5, 9, 8, 10, 8, 10, 5, 7, 7, 9, 7...
    ## $ stfjb        <dbl> NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, N...
    ## $ stfjbot      <dbl> NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, N...
    ## $ imm_econ     <dbl> 10, 8, 5, 7, 2, 8, 6, 5, 9, 7, 5, 1, 2, 5, 0, 2, ...
    ## $ imm_same     <dbl> 1, 1, 2, NA, 2, 1, 1, 3, 1, 2, 3, 4, 2, 2, 2, 3, ...
    ## $ imm_diff     <dbl> 1, 1, 3, NA, 2, 1, 3, 3, 1, 2, 3, 4, 4, 2, 4, 4, ...
    ## $ imm_poor     <dbl> 1, 1, 3, NA, 1, 1, 3, 3, 2, 2, 3, 4, 4, 3, 3, 4, ...
    ## $ vote         <chr> "", "", "SPÖ", "", "", "ÖVP", "", "ÖVP", "", "ÖVP...
    ## $ vote_id      <chr> "", "", "1", "", "", "2", "", "2", "", "2", "2", ...
    ## $ country      <chr> "Austria", "Austria", "Austria", "Austria", "Aust...
    ## $ iso2         <chr> "AT", "AT", "AT", "AT", "AT", "AT", "AT", "AT", "...
    ## $ iso3         <chr> "AUT", "AUT", "AUT", "AUT", "AUT", "AUT", "AUT", ...

``` r
### Germany special 
step3$vote_id[step3$country == "Germany"] <- step3 %>%
  filter(country == "Germany") %>%
  mutate(vote_id = stringr::str_extract(vote_id, "\\d$")) %>%
  .$vote_id
```

``` r
step4 <- step3 %>%
  filter(!(country %in% c("Romania", "Malta", "Luxembourg", "Latvia", "Lithuania")))%>%
  mutate(vote_id = ifelse(vote_id == "", "", paste(iso2, vote_id, sep = "_")))
```

``` r
ess_parties <- step4 %>%
  filter(vote != "") %>%
  group_by(country, iso2, vote, vote_id) %>%
  tally %>%
  #mutate(n = NULL) %>%
  filter(!vote == "Other") %>%
  arrange(country, vote_id) %>%
  mutate(vote_id = paste(iso2, vote_id, sep = "_"))


library(openxlsx)
#openxlsx::write.xlsx(ess_parties, file = "data/ess_parties_new.xlsx")
# save(ess_parties, file = "shiny_match/ess_parties.Rdata")
# write.csv2(ess_parties, "data/ess_parties.csv")
matching_data <- openxlsx::read.xlsx("data/ess_parties_new.xlsx") %>%
  select(party_id, vote_id) %>%
  na.omit
```

Match party data
----------------

mapping ches data to ess

``` r
glimpse(step4)
```

    ## Observations: 56,368
    ## Variables: 36
    ## $ id           <dbl> 1, 2, 4, 6, 10, 11, 12, 13, 14, 15, 16, 17, 18, 1...
    ## $ round        <dbl> 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8...
    ## $ income       <dbl> NA, 5, 2, 4, 2, 10, 2, 8, 3, 4, 1, 3, 5, NA, 3, 5...
    ## $ pol_inter    <dbl> 1, 1, 3, 2, 3, 2, 3, 3, 4, 2, 3, 1, 2, 3, 3, 3, 2...
    ## $ lrscale      <dbl> 0, 1, 5, 0, 5, 5, 4, 5, 5, 5, 5, 8, 8, 5, NA, 6, ...
    ## $ rel          <dbl> 7, 7, 6, 6, 5, 5, 5, 5, 7, 5, 3, 7, 4, 5, 5, 5, 7...
    ## $ demo         <dbl> 2, 2, 2, 2, 2, 2, 2, 2, 2, 1, 2, 2, 2, 2, 2, 2, 2...
    ## $ citz         <dbl> 2, 2, 1, 1, 2, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1...
    ## $ edu          <dbl> 720, 313, 322, 322, 322, 313, 212, 423, 322, 322,...
    ## $ edu_year     <dbl> 21, 16, 13, 12, 13, 13, 8, 17, 14, 16, 9, 11, 12,...
    ## $ year         <dbl> 1982, 1964, 1948, 1962, 1996, 1951, 1964, 1972, 1...
    ## $ gndr         <dbl> 2, 1, 2, 1, 2, 2, 2, 2, 2, 2, 2, 1, 1, 2, 2, 1, 1...
    ## $ trust_parl   <dbl> 6, 5, 3, 1, 7, 4, 5, 6, 10, 7, 6, 0, 2, 6, 9, 8, ...
    ## $ trust_leg    <dbl> 7, 4, 5, 1, 7, 6, 6, 7, 10, 8, 5, 5, 8, 7, 9, 8, ...
    ## $ trust_police <dbl> 0, 3, 9, 2, 10, 7, 7, 9, 10, 7, 5, 7, 7, 7, 9, 10...
    ## $ trust_pol    <dbl> 2, 3, 3, 1, 6, 3, 2, 5, 9, 4, 3, 0, 6, 5, 7, 8, 5...
    ## $ trust_party  <dbl> 8, 4, 3, 1, 7, 3, 2, 3, 8, 4, 3, 0, 6, 5, 6, 8, 5...
    ## $ trust_eu     <dbl> 9, 6, 4, 1, 9, 2, NA, 4, 8, 1, 3, 0, 5, 7, 6, 8, ...
    ## $ trstun       <dbl> 10, 7, 5, 1, 10, 4, NA, 6, 10, 0, NA, 0, 4, 6, 3,...
    ## $ s_life       <dbl> 5, 5, 9, 7, 10, 7, 5, 9, 10, 7, 8, 7, 6, 8, 3, 8,...
    ## $ s_econ       <dbl> 4, 1, 6, 5, 7, 3, 5, 6, 10, 5, 0, 5, 6, 5, 5, 6, ...
    ## $ s_gov        <dbl> 4, 3, 3, 6, 5, 5, 6, 5, 10, 3, 4, 0, 2, 4, 6, 7, ...
    ## $ s_dem        <dbl> 6, 3, 6, 6, 9, 5, 7, 8, 10, 7, 5, 3, 8, 5, 8, 8, ...
    ## $ stfedu       <dbl> 3, 0, 8, 7, 10, 4, 4, 7, 9, 8, 10, 4, 5, 6, 3, 8,...
    ## $ stfhlth      <dbl> 5, 3, 6, 6, 10, 5, 9, 8, 10, 8, 10, 5, 7, 7, 9, 7...
    ## $ stfjb        <dbl> NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, N...
    ## $ stfjbot      <dbl> NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, N...
    ## $ imm_econ     <dbl> 10, 8, 5, 7, 2, 8, 6, 5, 9, 7, 5, 1, 2, 5, 0, 2, ...
    ## $ imm_same     <dbl> 1, 1, 2, NA, 2, 1, 1, 3, 1, 2, 3, 4, 2, 2, 2, 3, ...
    ## $ imm_diff     <dbl> 1, 1, 3, NA, 2, 1, 3, 3, 1, 2, 3, 4, 4, 2, 4, 4, ...
    ## $ imm_poor     <dbl> 1, 1, 3, NA, 1, 1, 3, 3, 2, 2, 3, 4, 4, 3, 3, 4, ...
    ## $ vote         <chr> "", "", "SPÖ", "", "", "ÖVP", "", "ÖVP", "", "ÖVP...
    ## $ vote_id      <chr> "", "", "AT_1", "", "", "AT_2", "", "AT_2", "", "...
    ## $ country      <chr> "Austria", "Austria", "Austria", "Austria", "Aust...
    ## $ iso2         <chr> "AT", "AT", "AT", "AT", "AT", "AT", "AT", "AT", "...
    ## $ iso3         <chr> "AUT", "AUT", "AUT", "AUT", "AUT", "AUT", "AUT", ...

``` r
step5 <- step4 %>%
  left_join(matching_data, by = "vote_id")

step5 %>%
  filter(vote != "") %>%
  count(is.na(party_id))
```

    ## # A tibble: 2 x 2
    ##   `is.na(party_id)`     n
    ##   <lgl>             <int>
    ## 1 FALSE             27852
    ## 2 TRUE               7852

CHES data
=========

``` r
# pacman::p_load(pdftables)
# 
# pdftable_api <- "opj9i5owyg40"
# pdftables::convert_pdf("data/2014_CHES_codebook.pdf",
#                         "data/party_tables.csv",
#                         api_key = pdftable_api)
# 
# pdftables::convert_pdf("codebooks/country_names.pdf",
#                         "codebooks/country_names.csv",
#                         api_key = pdftable_api)
# 
```

``` r
clist <- list(
  BE = "Belgium",
  DK  = "Denmark",
  GE  = "Germany",
  GR  = "Greece",
  ESP = "Spain",
  FR  = "France",
  IRL = "Ireland",
  IT  = "Italy",
  NL = "Netherlands",
  UK = "United Kingdom",
  POR=  "Portugal",
  AUS=  "Austria",
  FIN=  "Finland",
  SV = "Sweden",
  BUL=  "Bulgaria",
  CZ = "Czech Republic",
  EST=  "Estonia",
  HUN=  "Hungary",
  LAT=  "Latvia",
  LITH = "Lithuania",
  POL=  "Poland",
  ROM=  "Romania",
  SLO=  "Slovakia",
  SLE=  "Slovenia",
  CRO=  "Croatia",
  TUR=  "Turkey",
  NOR=  "Norway",
  SWI=  "Switzerland",
  MAL=  "Malta",
  LUX=  "Luxembourg",
  CYP=  "Cyprus"
)

library(haven)
ches <- read_dta("data/2014_CHES_dataset_means-2.dta") %>%
  mutate(country = tidyTX::tx_map_dict(country, dict = clist, key1 = 0, key2 = 1)) %>%
  mutate(party_id = as.integer(party_id))

ches <- ches %>%
  left_join(matching_data, by = "party_id")


# dt <- read_csv("data/party_tables.csv") %>%
#   mutate(country = tidyTX::tx_map_dict(country, dict = clist, key1 = 0, key2 = 1)) %>%
#   mutate(party_id = as.integer(party_id))

# ches_data <- ches %>%
#   select(-country, -party_name) %>%
#   mutate(party_id = as.integer(party_id)) %>%
#   left_join(dt, by = c("party_id")) %>%
#   mutate(country = countrycode::countrycode(country, "country.name", "country.name"))

# ches_parties <- ches_data %>% 
#   select(country, party_id, party_short, party_name_en) %>%
#   mutate(vote_id = NA) %>%
#   arrange(country)
# 
# table(ches_data$country)
# #ches_party <- nn %>%
# # save(ches_parties, file = "shiny_match/ches_parties.Rdata")
# #write.csv2(ches_parties, "data/ches_parties.csv")
# library(openxlsx)
# #openxlsx::write.xlsx(ches_parties, file = "data/ches_parties.xlsx")
# #readr::write_csv(ches_parties, path = "data/ches_parties.csv")
```

Ches
----

``` r
range01 <- function(x){(x - min(x, na.rm = T)) / (max(x, na.rm = T) - min(x, na.rm = T))}
ches <- ches %>% 
  mutate(populism = antielite_salience + corrupt_salience) %>% 
  mutate(populism2 = range01(range01(antielite_salience) + 
                               (1 - range01(eu_position)))*100) %>% #+ 
                           #    (1 - range01(eu_budgets)))*100) %>% 
  mutate(liberalism = sociallifestyle + civlib_laworder + galtan) %>% 
  mutate(populism = range01(populism)*100) %>% 
  mutate(liberalism = range01(liberalism)*100) #%>% 
  #filter(year > 2009)
```

``` r
ches %>% 
  ggplot(aes(liberalism, populism, colour = eu_position)) + 
  geom_point() +
  geom_smooth(method = "lm", formula = y ~ poly(x, 2))+
  #geom_text_repel(aes(liberalism, populism, label = party_cntry)) +
  ggthemes::theme_hc() +
  viridis::scale_color_viridis()
```

![](data_wranggle_files/figure-markdown_github/unnamed-chunk-12-1.png)

clustering
----------

``` r
set.seed(2018)
ches_cluster_data <- ches %>% 
  select(party_name, vote_id, liberalism, populism2) %>% 
  drop_na(liberalism, populism2) %>% 
  as.data.frame()

ches_cluster <- ches_cluster_data %>% 
  select(-party_name, -vote_id) %>% 
  mutate_all(scale) 
```

``` r
library(factoextra)
```

    ## Welcome! Related Books: `Practical Guide To Cluster Analysis in R` at https://goo.gl/13EFCZ

``` r
distance <- get_dist(ches_cluster)
fviz_dist(distance, 
 gradient = list(low = "#00AFBB", 
                 mid = "white",
                 high = "#FC4E07"))
```

![](data_wranggle_files/figure-markdown_github/unnamed-chunk-14-1.png)

``` r
k3 <- kmeans(ches_cluster, centers = 3, nstart = 25)
ggf <- fviz_cluster(k3, data = ches_cluster, show.clust.cent = T, text = "vote_id")
ggf
```

![](data_wranggle_files/figure-markdown_github/unnamed-chunk-15-1.png)

``` r
#plotly::ggplotly(ggf)

# library(ggrepel)
# 
# ches %>%
#   select(vote_id, liberalism, populism2) %>% 
#   na.omit() %>% 
#   mutate(cluster = k3$cluster,
#          state = row.names(ches_cluster)) %>%
#   ggplot(aes(liberalism, populism2, color = factor(cluster))) + 
#   geom_point() +
#   geom_text_repel(aes(liberalism, populism2, label = vote_id)) +
#   ggthemes::theme_hc() +
#   ggthemes::scale_color_gdocs()
```

``` r
library(highcharter)

df1 <- tibble(vote = paste(ches_cluster_data$party_name, ches_cluster_data$vote_id, sep = " "), cluster = k3$cluster, x = as.vector(ches_cluster$liberalism), y = as.vector(ches_cluster$populism2)) %>%
  filter(stringr::str_detect(vote, "DE_"))
hchart(df1, hcaes(x = x, y = y, name = vote, color = cluster), type = 'scatter') %>%
  hc_add_theme(hc_theme_smpl()) %>%
  hc_tooltip(
    formatter = JS("function(){
                    return ('Party: <strong>' + this.point.vote + '</strong><br> X: ' + this.x + ' <br> Y: ' + this.y + ' <br>')
                  }")) %>%
  hc_chart(zoomType = "xy")
```
