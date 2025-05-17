setwd("~/Dropbox/ebaGenerosity/replication")
rm(list = ls())
gc()
source('1code/helper.R')

#########

# PREPARATION, ESTIMATION AND PREDICTION
projects <- c('CASE1', 'CASE2', 'CASE3', 'CASE4')
projects <- c('CASE4')
for(kkk in 1:length(projects)){
  
  projectName <- projects[kkk]
  
  source('1code/2_1_spec.R')
  source('1code/3_1_est.R')
  source('1code/4_1_aggr.R')
  source('1code/5_1_pred.R')
  source('1code/6_1_nneigh.R')

}

###

# VISUALIZATIONS: FIGURE 1 to 4
projects <- c('CASE2', 'CASE1', 'CASE3', 'CASE4')
for(jj in 1:length(projects)){
  
  # LOAD RESULTS
  projectName <- projects[[jj]]
  load(paste0('2data/4aggr/', projectName, '/res.RData'))
  df <- addVariableLabels(df)
  
  # PLOT 1 & 2
  fileName <- paste0('3res/FIGURE1&2_', projectName, '.png')
  createPlotSignShareAll() %>% 
    ggsave(filename = fileName, width = 8, height = 4)
  
  # PLOT 3
  fileName <- paste0('3res/FIGURE3_', projectName, '.png')
  createModelSpecChangePlot(projectName = projectName) %>% 
    ggsave(filename = fileName, width = 8, height = 4)
  
  # PLOT 4
  fileName <- paste0('3res/FIGURE4_', projectName, '.png')
  createFIPPlot(projectName = projectName) %>% 
    ggsave(filename = fileName, width = 8, height = 4)
  
}

