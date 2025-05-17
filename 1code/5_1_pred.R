# LOAD PREDICTION DATASET
fileName <- paste0('2data/4aggr/', projectName, '/ml.RData')
load(fileName)

# DEFINE FOCUS VARIABLES
var <- colnames(df) %>% subset(., grepl("CONT", .)) %>% sort()

# RUN MLOGIT FROM MLOGIT
results <- list()
for(i in 1:length(var)){
  
  focus <- var[i]
  print(paste0('START | ', projectName, ' | ', focus, ' | ', Sys.time()))
  
  # LOAD ML DATASET
  fileName <- paste0('2data/4aggr/', projectName, '/ml.RData')
  load(fileName)
  df <- df %>% subset(., IV == focus) %>% dplyr::select(-IV) %>% dplyr::rename('outcome' = 'sig_sign')
  
  ###
  
  # NN
  signs <- c('not significant', 'significant negative', 'significant positive')
  for(l in 1:length(signs)){
    sign <- signs[l]
    res <- df %>% dplyr::select(-focus) %>% mutate(outcome = as.numeric(outcome == sign))
    if(length(unique(res$outcome)) > 1){
      fileName <- paste0('2data/5pred/', projectName, '/NN/NN_', focus, '_', sign, '.RData')
      if(!file.exists(fileName)){
        tryCatch({
          source('1code/5_1a_dl_helper.R')
        }, error = function(e){})
      }
    }
  }
  
  ###
  
  # MULTINOMIAL LOGIT
  tryCatch({
    model <- df %>% multinom(outcome ~ ., data = .)
    predicted_classes <- predict(model, newdata = df, type = "class")
    conf_matrix <- caret::confusionMatrix(factor(predicted_classes), factor(df$outcome))
    metrics <- data.frame(
      class = rownames(conf_matrix$byClass),
      accuracy = as.numeric(conf_matrix$overall[1]),
      precision = conf_matrix$byClass[, 'Precision'],
      recall = conf_matrix$byClass[, 'Recall'],
      F1 = conf_matrix$byClass[, 'F1'],
      stringsAsFactors = F
    )
    fileName <- paste0('2data/5pred/', projectName, '/MLOGIT_performance/MLOGIT_', focus, '.RData')
    save(metrics, file = fileName)
  }, error = function(e) paste('error: ', focus) %>% print())

  # OR
  OR <- model %>%
    odds.ratio(., conf.level = 0.95) %>%
    rownames_to_column(., var = 'SPEC') %>%
    separate(., col = 'SPEC', into = c('val', 'SPEC'), sep = '/') %>%
    purrr::set_names('val', 'SPEC', 'OR', 'lCI', 'uCI', 'p') %>%
    dplyr::mutate(IV = focus)

  # PP
  predictors_baseline <- df %>% dplyr::slice(1) %>% replace(. == T, 0) %>%
    predict(model, ., type = 'probs') %>% data.frame(.) %>%
    dplyr::select('prop_baseline' = '.') %>% rownames_to_column(., var = 'val')
  specs <- colnames(df) %>% subset(., !grepl('outcome', .))
  PP <- data.frame(stringsAsFactors = F)
  for(kk in 1:length(specs)){
    temp1<- df %>% dplyr::slice(1) %>% replace(. == T, 0)
    temp1[[specs[kk]]] <- 1
    PP <- temp1 %>%
      predict(model, ., type = 'probs') %>% data.frame(.) %>%
      dplyr::select('prop' = '.') %>% rownames_to_column(., var = 'val') %>%
      left_join(., predictors_baseline, by = 'val') %>%
      dplyr::mutate(prop_diff = prop-prop_baseline, SPEC = specs[[kk]]) %>%
      bind_rows(PP) %>% mutate(IV = focus)
  }

  # STORE
  results_mlogit <- PP %>%
    left_join(., OR, by = c('IV', 'SPEC', 'val')) %>%
    dplyr::select(IV, SPEC, val, contains('PP'), contains('prop'), everything())
  fileName <- paste0('2data/5pred/', projectName, '/MLOGIT/MLOGIT_', focus, '.RData')
  save(results_mlogit, file = fileName)

  print(paste0('END | ', projectName, ' | ', focus, ' | ', Sys.time()))
}


