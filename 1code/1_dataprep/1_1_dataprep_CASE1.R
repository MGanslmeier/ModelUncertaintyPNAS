setwd("replication")
rm(list = ls())
gc()
source('1code/helper.R')

# CREATE SUMMARY STATISTICS TABLE
load('2data/1raw/CASE1/temp.RData')
df %>% 
  dplyr::select_at(vars(starts_with('DEP'), starts_with('CONT'))) %>% 
  stargazer(as.data.frame(.), type = 'latex', omit.summary.stat = c('p25', 'p75'),
            covariate.labels = xx,
            out = '3res/summary_stats/CASE1_stats_table.tex')
df %>% 
  dplyr::select_at(vars(starts_with('DEP'), starts_with('CONT'))) %>% 
  stargazer(as.data.frame(.), type = 'html', omit.summary.stat = c('p25', 'p75'),
            out = '3res/summary_stats/CASE1_stats_table.html')

