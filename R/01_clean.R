# 01_clean.R
# Clean PACES data for analysis

library(tidyverse)
library(haven)

# Load raw data
test_takers_raw <- read_sas("data/raw/tab5v1.sas7bdat")

paces_raw <- haven::read_sas("data/raw/aerdat4.sas7bdat")

dim(paces_raw)
"TAB3SMPL" %in% names(paces_raw)

# Create analysis dataset
paces <- paces_raw %>%
  select(
    ID,
    VOUCH0,
    
    DBOGOTA,
    DJAMUNDI,
    D1993,
    D1995,
    D1997,
    
    SVY,
    HSVISIT,
    DMONTH1:DMONTH12,
    DAREA1:DAREA19,
    
    STRATA1:STRATA6,
    STRATAMS,
    
    PHONE,
    AGE,
    AGE2,
    SEX_NAME,
    SEX2,
    SEX_MISS,
    
    BOG95ASD,
    BOG97ASD,
    JAM93ASD,
    
    BOG95SMP,
    BOG97SMP,
    JAM93SMP,
    
    MOM_SCH,
    DAD_SCH,
    MOM_AGE,
    DAD_AGE,
    MOM_MW,
    DAD_MW,
    
    TAB3SMPL,
    
    SCYFNSH,
    INSCHL,
    FINISH6,
    FINISH7,
    FINISH8,
    
    PRSCHA_1,
    PRSCHA_2,
    PRSCH_C,
    
    REPT6,
    REPT,
    NREPT,
    TOTSCYRS,
    
    USNGSCH
  )

dim(paces)

dim(test_takers_raw)
names(test_takers_raw)
test_takers_raw %>% count(VOUCH0)

names(paces_raw)

test_takers <- test_takers_raw %>%
  select(
    VOUCH0,
    SVY, HSVISIT,
    DBOGOTA, DJAMUNDI,
    D1993, D1995, D1997,
    DMONTH1:DMONTH12,
    DAREA1:DAREA19,
    AGE,
    SEX, SEX2,
    MOM_SCH, DAD_SCH,
    MOM_AGE, DAD_AGE,
    DAD_MW,
    BOG95SMP
  )

