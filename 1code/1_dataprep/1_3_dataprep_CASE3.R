setwd("replication")
rm(list = ls())
gc()
source('1code/helper.R')

# LOAD RAW DATA
raw <- read_dta("2data/0original/CASE3/final_jop_dataverse.dta") %>%
  mutate(FEunit = as.character(prefecture_id), FEtime = year)

# SELECT VARIABLES
df <- raw %>%
  arrange(FEunit, FEtime) %>%
  group_by(FEunit) %>% 
  mutate(lagoilsale = lag(oil_sale, 1), lagcoalsale = lag(coal_sale, 1), laggassale = lag(gas_sale, 1),
         lagsoe_share1 = lag(soe_share1, 1)) %>% 
  ungroup() %>%
  mutate(region = prov_id %>% str_sub(., 1, 2) %>% as.character() %>% gsub('5e', '50', .)) %>%
  dplyr::select(FEunit, FEtime, region,
                DEPlnschoolp = ln_school_p,
                DEPlnschools = ln_school_s,
                DEPlnschoolh = ln_school_h,
                DEPlnteacherp = ln_teacher_p,
                DEPlnteachers = ln_teacher_s,
                DEPlnteacherh = ln_teacher_h,
                DEPlnhospital = ln_hospital,
                DEPlnbed = ln_bed,
                DEPlndoctor = ln_doctor,
                CONTlagoilsale = lagoilsale,
                CONTlaggassale = laggassale,
                CONTlagcoalsale = lagcoalsale,
                CONTlnstudentp = ln_student_p,
                CONTlngdp = ln_gdp,
                CONTlnpop = ln_pop,
                CONTurbpop = urban_pop,
                CONTlnlandarea = ln_land_area,
                CONTmanuoutside = manu_outside,
                CONTlagsoeshare = lagsoe_share1,
                CONTbirthrate = ni_pop5)

# CREATE COUNTRY SAMPLES
df <- df %>%
  mutate(
    CSAMPLENorthChina = as.numeric(region %in% c("11", "12", "13", "14", "15")),   # North China
    CSAMPLENortheastChina = as.numeric(region %in% c("21", "22", "23")),       # Northeast China
    CSAMPLEEastChina = as.numeric(region %in% c("31", "32", "33", "34", "35", "36", "37")),  # East China
    CSAMPLECentralChina = as.numeric(region %in% c("41", "42", "43")),         # Central China
    CSAMPLESouthChina = as.numeric(region %in% c("44", "45", "46")),           # South China
    CSAMPLESouthwestChina = as.numeric(region %in% c("50", "51", "52", "53", "54")),  # Southwest China
    CSAMPLENorthwestChina = as.numeric(region %in% c("61", "62", "63", "64", "65"))  # Northwest China
  ) %>%
  mutate(CSAMPLEAll = 1)

# CREATE PERIOD SAMPLES
df <- df %>%
  mutate(TSAMPLE19921999 = as.numeric(FEtime %in% c(1992:1999)),
         TSAMPLE20002010 = as.numeric(FEtime %in% c(2000:2010))) %>%
  mutate(TSAMPLEall = 1)

# SAVE DATA
save(df, file = '2data/1raw/CASE3/temp.RData')

###

# CREATE SUMMARY STATISTICS TABLE
load('2data/1raw/CASE3/temp.RData')
df %>% 
  dplyr::select_at(vars(starts_with('DEP'), starts_with('CONT'))) %>% 
  stargazer(as.data.frame(.), type = 'latex', omit.summary.stat = c('p25', 'p75'),
            out = '3res/summary_stats/CASE3_stats_table.tex')
df %>% 
  dplyr::select_at(vars(starts_with('DEP'), starts_with('CONT'))) %>% 
  stargazer(as.data.frame(.), type = 'html', omit.summary.stat = c('p25', 'p75'),
            out = '3res/summary_stats/CASE3_stats_table.html')

