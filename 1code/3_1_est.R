# LOAD DATA AND SPEC
fileName <- paste0('2data/1raw/', projectName, '/temp.RData')
load(fileName)
fileName <- paste0('2data/2spec/', projectName, '/spec.RData')
load(fileName)

###

# RUN ESTIMATIONS
res <- specs %>% pull(MODELID) %>% pbmclapply(., runEstimation, mc.cores = 3)

###

# SAVE ESTIMATIONS
fileName <- paste0('2data/3est/', projectName, '/est.RData')
save(res, file = fileName)
