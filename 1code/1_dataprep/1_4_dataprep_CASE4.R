setwd("replication")
rm(list = ls())
gc()
source('1code/helper.R')

# LOAD RAW DATA
ess_raw <- haven::read_dta("2data/0original/CASE4/ess_1-10.dta")

# SELECT VARIABLES
df <- ess_raw %>%
  rownames_to_column(., var = 'RESPONSEID') %>%
  mutate(turnout = as.numeric(vote == 1), 
         DEPtrustep = as.numeric(trstep),
         DEPtrustls = as.numeric(trstlgl),
         DEPtrustpol = as.numeric(trstplt),
         DEPtrustprl = as.numeric(trstprl),
         female = as.numeric(gndr == 2),
         age = as.numeric(agea), 
         eduyrs = as.numeric(eduyrs), 
         mbtru = as.numeric(mbtru == 1),
         married = as.numeric(marital == 1), 
         employed = as.numeric(empl == 1),
         lrscale = as.numeric(lrscale), 
         hhmmb = as.numeric(hhmmb),
         polinterest = as.numeric(polintr),
         hincfeel = as.numeric(hincfel),
         satisgov = as.numeric(stfgov),
         religious = as.numeric(rlgdgr),
         voted = as.numeric(vote == 1),
         health = as.numeric(health),
         bigcity = as.numeric(domicil %in% c(1, 2)),
         iso = as.character(cntry), year = as.character(essround))
df_temp <- df %>%
  dplyr::select(RESPONSEID, contains('DEPtrust')) %>%
  subset(., complete.cases(.)) %>%
  mutate(DEPtrustpca = prcomp(.[,c("DEPtrustep", "DEPtrustls", "DEPtrustpol", "DEPtrustprl")], 
                              center = TRUE, scale = TRUE, rank. = 1)[["x"]] %>% as.numeric()) %>%
  dplyr::select(RESPONSEID, DEPtrustpca)
df <- df %>% 
  left_join(., df_temp, by = 'RESPONSEID') %>%
  mutate(DEPtrustmean = (DEPtrustep+DEPtrustls+DEPtrustpol+DEPtrustprl)/4) %>%
  dplyr::select(iso, TSAMPLE = year, starts_with('DEP'),
                CONTfemale = female, CONTage = age, CONTeduyrs = eduyrs, CONTmbtru = mbtru, 
                CONTbigcity = bigcity, CONTreligious = religious, CONThhmmb = hhmmb, 
                CONTlrscale = lrscale, CONTpolinterest = polinterest, CONThincfeel = hincfeel,
                CONTsatisgov = satisgov, CONTvoted = voted, CONThealth = health) %>%
  mutate(FEunit = iso, FEtime = TSAMPLE)

# CREATE COUNTRY SAMPLES
df <- df %>%
  mutate(CSAMPLEeucore = as.numeric(FEunit %in% c('DE', 'FR', 'GB', 'IT', 'ES')),
         CSAMPLEeusmall = as.numeric(FEunit %in% c('AT', 'BE', 'CY', 'IE', 'PT', 'LU', 'NL')),
         CSAMPLEscan = as.numeric(FEunit %in% c('SE', 'FI', 'DK', 'NO', 'IS')),
         CSAMPLEees = as.numeric(FEunit %in% c('BG', 'CZ', 'EE', 'HR', 'LT', 'LV', 'PL', 'RO', 'SI', 'SK'))) %>%
  mutate(CSAMPLEall = 1)
  
# CREATE PERIOD SAMPLES
df <- df %>%
  mutate(TSAMPLE12 = as.numeric(FEtime %in% c('1', '2')),
         TSAMPLE34 = as.numeric(FEtime %in% c('3', '4')),
         TSAMPLE56 = as.numeric(FEtime %in% c('5', '6')),
         TSAMPLE78 = as.numeric(FEtime %in% c('7', '8')),
         TSAMPLE90 = as.numeric(FEtime %in% c('9', '10'))) %>%
  mutate(TSAMPLEall = 1) %>%
  dplyr::select(-TSAMPLE)

# SAVE DATA
save(df, file = '2data/1raw/CASE4/temp.RData')

###

# CREATE SUMMARY STATISTICS TABLE
load('2data/1raw/CASE4/temp.RData')
df %>% 
  dplyr::select_at(vars(starts_with('DEP'), starts_with('CONT'))) %>% 
  stargazer(as.data.frame(.), type = 'latex', omit.summary.stat = c('p25', 'p75'),
            out = '3res/summary_stats/CASE4_stats_table.tex')
df %>% 
  dplyr::select_at(vars(starts_with('DEP'), starts_with('CONT'))) %>% 
  stargazer(as.data.frame(.), type = 'html', omit.summary.stat = c('p25', 'p75'),
            out = '3res/summary_stats/CASE4_stats_table.html')
