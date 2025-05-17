setwd("replication")
rm(list = ls())
gc()
source('1code/helper.R')

# LOAD RAW DATA
raw <- read.csv("2data/0original/CASE2/Support_democracy_ajps_correct.csv") %>%
  mutate(FEunit = countrycode(Country, 'country.name', 'iso3c'))
load('2data/0original/CASE2/democracy.rda')
democracy1 <- democracy %>%
  mutate(FEunit = countrycode(extended_country_name, 'country.name', 'iso3c')) %>%
  subset(., !is.na(FEunit)) %>%
  subset(., year %in% raw$Year & FEunit %in% raw$Cnt_code)
vars <- c("SP.POP.TOTL", "SE.SEC.ENRR", "SE.PRM.ENRR", "NE.TRD.GNFS.ZS", "EG.ELC.ACCS.ZS", "NV.IND.TOTL.ZS", "NV.AGR.TOTL.ZS")
wb_data <- wb_data(vars, start_date = 1980, end_date = 2020) %>%
  dplyr::select('Year' = 'date', 'Cnt_code' = 'iso3c', 'lnpop' = "SP.POP.TOTL", 
                'schoolsec' = "SE.SEC.ENRR", 'schoolprim' = "SE.PRM.ENRR", 
                'trade' = "NE.TRD.GNFS.ZS", 'elect' = "EG.ELC.ACCS.ZS", 
                'industry' = "NV.IND.TOTL.ZS", 'agricult' = "NV.AGR.TOTL.ZS")  %>%
  dplyr::mutate(lnpop = log(lnpop+1))

# SELECT VARIABLES
df <- raw %>%
  left_join(., wb_data, by = c('Year', 'Cnt_code')) %>%
  mutate(region = case_when(Region_VD == 1 ~ 'easteurope', Region_VD %in% c(2, 10) ~ 'southamerica', 
                            Region_VD == 3 ~ 'middleeast', Region_VD == 4 ~ 'subsahafrica', 
                            Region_VD == 5 ~ 'europeus', Region_VD %in% c(6, 7, 8, 9) ~ 'asia')) %>%
  dplyr::select(FEunit = Cnt_code, FEtime = Year, region,
                DEPlibdem = Libdem_VD, 
                DEPpolyarchy = Polyarchy_VD, 
                DEPregime = Regime_VD, 
                DEPchangedem = ChgDem,
                CONTdemsup = SupDem_m1,
                CONTnatres = Res_cp_WDI_di_m1,
                CONTmuslim = Pr_Muslim,
                CONTsatis = Satis_m1,
                CONTgdplog = lnGDP_imp_m1,
                CONTgdpgrowth = GDP_imp_grth_m1,
                CONTinflation = lnInflat_imp,
                CONTcorruption = Corrup_TI,
                CONTlnpop = lnpop, 
                CONTschoolsec = schoolsec, 
                CONTtrade = trade, 
                CONTindustry = industry, 
                CONTagri = agricult)

# CREATE COUNTRY SAMPLES
df <- df %>%
  mutate(CSAMPLEeasteurope = as.numeric(region == 'easteurope'),
         CSAMPLEeuropeus = as.numeric(region == 'europeus'),
         CSAMPLEmiddleeast = as.numeric(region == 'middleeast'),
         CSAMPLEsouthamerica = as.numeric(region == 'southamerica'),
         CSAMPLEasia = as.numeric(region == 'asia'),
         CSAMPLEsubsahafrica = as.numeric(region == 'subsahafrica')) %>%
  mutate(CSAMPLEall = 1)

# CREATE PERIOD SAMPLES
df <- df %>%
  mutate(TSAMPLE19871997 = as.numeric(FEtime %in% c(1987:1997)),
         TSAMPLE19982007 = as.numeric(FEtime %in% c(1998:2007)),
         TSAMPLE20082018 = as.numeric(FEtime %in% c(2008:2018))) %>%
  mutate(TSAMPLEall = 1)

# SAVE DATA
save(df, file = '2data/1raw/CASE2/temp.RData')

###

# CREATE SUMMARY STATISTICS TABLE
load('2data/1raw/CASE2/temp.RData')
df %>% 
  dplyr::select_at(vars(starts_with('DEP'), starts_with('CONT'))) %>% 
  stargazer(as.data.frame(.), type = 'latex', omit.summary.stat = c('p25', 'p75'),
            out = '3res/summary_stats/CASE2_stats_table.tex')
df %>% 
  dplyr::select_at(vars(starts_with('DEP'), starts_with('CONT'))) %>% 
  stargazer(as.data.frame(.), type = 'html', omit.summary.stat = c('p25', 'p75'),
            out = '3res/summary_stats/CASE2_stats_table.html')
