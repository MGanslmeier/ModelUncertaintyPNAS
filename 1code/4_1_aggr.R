# LOAD ESTIMATIONS
fileName <- paste0('2data/3est/', projectName, '/est.RData')
load(fileName)

# MERGE RESULTS
df <- res %>% 
  Filter(Negate(is.null), .) %>%
  lapply(., function(x){if(nrow(x$est)>0){cbind(x$est, x$meta)}}) %>% bind_rows() %>%
  subset(., !is.nan(se)) %>%
  mutate(sig_sign = case_when(pval > 0.1 ~ 'not significant', 
                              pval <= 0.1 & coef < 0 ~ 'significant negative', 
                              pval <= 0.1 & coef >= 0 ~ 'significant positive')) %>%
  dplyr::select(MODELID, everything(.)) %>%
  group_by(IV, set, deps, fes, csamples, tsamples, sig_sign) %>% dplyr::summarize(freq = n()) %>% ungroup() %>%
  dplyr::mutate(fes = case_when(fes == '' ~ 'none', fes == 'factor(FEtime)' ~ 'time',
                                fes == 'factor(FEunit)' ~ 'unit', grepl('\\+', fes) ~ 'both'))

# RUN FUNCTION
df <- list(
  all = calculateSignShares(),
  set = calculateSignShares(dimension = 'set'),
  fes = calculateSignShares(dimension = 'fes'),
  deps = calculateSignShares(dimension = 'deps'),
  csamples = calculateSignShares(dimension = 'csamples'),
  tsamples = calculateSignShares(dimension = 'tsamples')
)

# SAVE RESULTS
fileName <- paste0('2data/4aggr/', projectName, '/res.RData')
save(df, file = fileName)

######

## CREATE DATAFRAME FOR PREDICTION EXERCISE

# LOAD ESTIMATIONS
fileName <- paste0('2data/4aggr/', projectName, '/res.RData')
load(fileName)

# CREATE DATAFRAME
df <- res %>% 
  lapply(., function(x){
    if(!is.null(x)){
      if(nrow(x$est)>0){
        temp <- x$meta %>% 
          dplyr::select(contset, fes, deps, csamples, tsamples) %>% 
          mutate(fes = case_when(fes == '' ~ 'FEnone', fes == 'factor(FEtime)' ~ 'FEtime',
                                 fes == 'factor(FEunit)' ~ 'FEunit', grepl('\\+', fes) ~ 'FEboth')) %>%
          as.character() %>% str_split(., '\\+') %>% unlist() %>% 
          data.frame(var = .) %>% mutate(temp = 1) %>% column_to_rownames('var') %>% t() %>% as.data.frame() %>%
          cbind(x$est, .) %>%
          dplyr::rename('SET' = 'set') %>%
          mutate(sig_sign = case_when(pval > 0.1 ~ 'not significant', pval <= 0.1 & coef < 0 ~ 'significant negative', 
                                      pval <= 0.1 & coef >= 0 ~ 'significant positive')) %>%
          dummy_cols(., select_columns = 'SET', remove_selected_columns = T) %>% 
          dplyr::rename("SETclustered" = "SET_clustered", "SETrobust" = "SET_robust", "SETsimple" = "SET_simple") %>%
          dplyr::select(sig_sign, matches('IV|SET|CONT|FE|DEP|CSAMPLE|TSAMPLE'))
        return(temp)
      }
    }
}) %>% bind_rows() %>% na_replace(., 0)

# SAVE ML DATASET
fileName <- paste0('2data/4aggr/', projectName, '/ml.RData')
save(df, file = fileName)

