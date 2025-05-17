# LOAD OBJECTS
fileName <- paste0("2data/1raw/", projectName, "/temp.RData")
load(fileName)
fileName <- paste0("2data/2spec/", projectName, "/spec.RData")
load(fileName)

###

# DEFINE SPECIFICATION DECISIONS
if(nrow(specs) >= 50000){
  specs <- specs %>% sample_n(50000)
}
specs_decision <- c(specs$contset %>% str_split(., '\\+') %>% unlist() %>% 
                      unique() %>% subset(., !grepl('split', .)),
                    specs$deps %>% unique(), specs$fes %>% unique(), 
                    specs$csamples %>% unique(), specs$tsamples %>% unique(),
                    c('SErobust', 'SEclustered'))

###

# RUN ESTIMATION FOR RANDOM MODEL AND 1-ORDER PERMUTATION
models <- specs %>% pull(MODELID) %>% unique()
res <- pbmclapply(1:length(models), function(i) {
  tryCatch({
    temp2 <- runEstimation_draw(modelID = models[i], baseline = 'baseline') %>%
      dplyr::select(-changeSpec)
    spec_types <- c('CONT', 'CSAMPLE', 'TSAMPLE', 'DEP', 'SE', 'FE')
    spec_type_temp <- sample(spec_types, 2)
    change_spec <- specs_decision %>% subset(., grepl(spec_type_temp[1], .)) %>% sample(., 1)
    temp3 <- runEstimation_draw(modelID = models[i], baseline = c(change_spec)) %>%
      dplyr::rename('outcome_change' = 'outcome')
    temp4 <- temp2 %>% 
      left_join(., temp3, by = c('modelID', 'IV')) %>%
      subset(., complete.cases(.) & changeSpec != '')
    return(temp4)
  }, error = function(e){})
}, mc.cores = 6) %>% bind_rows()

###

# STORE RESULTS
fileName <- paste0("2data/6nneigh/", projectName, "/neigh.RData")
save(res, file = fileName)
