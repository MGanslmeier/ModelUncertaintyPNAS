pacman::p_load(dplyr, readr, purrr, stringr, data.table, pbapply, tidyr, fastDummies, 
               tibble, parallel, pbmcapply, sandwich, lmtest, multiwayvcov, imputeTS,
               ggplot2, ggthemes, stargazer, nnet, neuralnet, caret, NeuralNetTools, 
               vip, SHAPforxgboost, kernelshap, ggsci, plyr, readxl, R.utils, pryr,
               gtools, scales, tidymodels, xgboost, parsnip, baguette, klaR, kernlab, 
               kknn, stacks, keras, tensorflow, fastshap, tfruns, themis, tidyverse, 
               mltools, countrycode, patchwork, iml, lime, questionr)

######

# DEFINE PYTHON VERSION
tryCatch({
  use_python("~/miniforge3/bin/python")
  use_condaenv("tf_env")
}, error = function(e){})
options(timeout = 100000)

###

# LOAD VARIABLE LABELS
lab <- read_excel('2data/1raw/admin/variable_labels.xlsx') %>% 
  dplyr::group_by(oldval) %>% dplyr::slice(1) %>% ungroup() %>% unique() %>%
  subset(., complete.cases(.))

###

# FUNCTION TO ESTIMATE MODEL SPACE
runEstimation <- function(modelID){
  
  tryCatch({
    
    # SPECIFICATIONS
    spec_temp <- specs %>% subset(., MODELID == modelID)
    data_temp <- df %>% subset(., base::get(spec_temp$csamples) == 1 & base::get(spec_temp$tsamples) == 1)
    form_temp <- spec_temp$deps %>% paste(., spec_temp$contset, sep = '~') %>% 
      paste(., spec_temp$fes, sep = '+') %>% sub("\\++$", "", .)
    reg <- lm(formula = form_temp, data = data_temp)
    
    # A) STAND SE
    temp <- coeftest(reg, vcov = vcovHC(reg, "const"))
    class(temp) <- 'matrix'
    tempNormSE <- temp %>% as.data.frame() %>% mutate(set = 'simple')
    
    # B) ROBUST SE
    temp <- coeftest(reg, vcov = vcovHC(reg, "HC1"))
    class(temp) <- 'matrix'
    tempRobSE <- temp %>% as.data.frame() %>% mutate(set = 'robust')
    
    # C) CLUSTURED SE
    temp <- coeftest(reg, cluster.vcov(reg, data_temp$FEunit))
    class(temp) <- 'matrix'
    tempCluSE <- temp %>% as.data.frame() %>% mutate(set = 'clustered')
    
    # COMBINE RESULTS
    res <- list()
    res$est <- list(normSE = tempNormSE, robSE = tempRobSE, cluSE = tempCluSE) %>%
      lapply(., function(x) x %>% rownames_to_column('IV')) %>%
      bind_rows() %>% subset(., !grepl('Intercept|factor', IV)) %>%
      purrr::set_names("IV", "coef", "se", "tval", "pval", "set") %>% 
      dplyr::select(IV, coef, se, pval, set)
    res$meta <-  spec_temp %>% 
      mutate(r2 = summary(reg)[["r.squared"]], aic = AIC(reg), 
             loglh = as.numeric(logLik(reg)), nobs = length(reg[["residuals"]]))
    return(res)
  }, error = function(e){})
}

###

# FUNCTION TO RUN RANDOM DRAW ESTIMATIONS
runEstimation_draw <- function(modelID, baseline = 'baseline'){
  spec_temp <- specs %>% subset(., MODELID == modelID)
  if(baseline != 'baseline'){
    if(grepl('TSAMPLE', baseline)){
      spec_temp$tsamples <- baseline
    }else if(grepl('CSAMPLE', baseline)){
      spec_temp$csamples <- baseline
    }else if(grepl('DEP', baseline)){
      spec_temp$deps <- baseline
    }else if(grepl('factor', baseline)){
      spec_temp$fes <- baseline
    }else if(grepl('CONT', baseline)){
      if(grepl(baseline, spec_temp$contset[1])){
        spec_temp$contset[1] <- gsub(baseline, '', spec_temp$contset[1])
      }else{
        spec_temp$contset[1] <- paste0(spec_temp$contset[1], '+', baseline)
      }
    }
  }
  
  # SPECIFICATIONS
  data_temp <- df %>% 
    subset(., base::get(spec_temp$csamples) == 1 & base::get(spec_temp$tsamples) == 1)
  form_temp <- spec_temp$deps %>% paste(., spec_temp$contset, sep = '~') %>% 
    paste(., spec_temp$fes, sep = '+') %>% sub("\\++$", "", .)
  reg <- lm(formula = form_temp, data = data_temp)
  
  if(grepl('SErobust', baseline)){
    # B) ROBUST SE
    temp <- coeftest(reg, vcov = vcovHC(reg, "HC1"))
    class(temp) <- 'matrix'
    temp <- temp %>% as.data.frame() %>% mutate(set = 'robust')
  }else if(grepl('SEclustered', baseline)){
    # C) CLUSTURED SE
    temp <- coeftest(reg, cluster.vcov(reg, data_temp$FEunit))
    class(temp) <- 'matrix'
    temp <- temp %>% as.data.frame() %>% mutate(set = 'clustered')
  }else{
    # A) STAND SE
    temp <- coeftest(reg, vcov = vcovHC(reg, "const"))
    class(temp) <- 'matrix'
    temp <- temp %>% as.data.frame() %>% mutate(set = 'simple')
  }
  
  # COMBINE RESULTS
  res <- temp %>% 
    rownames_to_column('IV') %>%
    subset(., !grepl('Intercept|factor', IV)) %>%
    purrr::set_names("IV", "coef", "se", "tval", "pval", "set") %>% 
    mutate(changeSpec = baseline, modelID = modelID) %>%
    mutate(outcome = case_when(coef >= 0 & pval <= 0.1 ~ 'significant positive',
                               coef < 0 & pval <= 0.1 ~ 'significant negative',
                               T ~ 'not significant')) %>%
    dplyr::select(modelID, changeSpec, IV, outcome)
  return(res)
}

###

# FUNCTION TO CALCULATE SIGNIFICANCE SHARES
calculateSignShares <- function(dimension = NULL){
  if(is.null(dimension)){
    temp <- df %>%
      group_by(IV, sig_sign) %>% dplyr::summarize(freq = sum(freq, na.rm = T)) %>% ungroup() %>%
      group_by(IV) %>% dplyr::mutate(freq_total = sum(freq, na.rm = T)) %>% ungroup()
  }else{
    temp <- df %>%
      group_by(IV, dimension = base::get(dimension), sig_sign) %>% dplyr::summarize(freq = sum(freq, na.rm = T)) %>% ungroup() %>%
      group_by(IV, dimension) %>% dplyr::mutate(freq_total = sum(freq, na.rm = T)) %>% ungroup()
  }
  temp <- temp %>%
    dplyr::mutate(share = freq/freq_total) %>%
    dplyr::mutate(perc = case_when(sig_sign == "significant negative" ~ share*(-1), T ~ share))
  return(temp)
}

###

# FUNCTION TO CALCULATE OVERALL SIGNIFICANCE SHARES
createPlotSignShareAll <- function(){
  temp <- df[['all']] %>% subset(., sig_sign != 'not significant')
  p1 <- ggplot() +
    geom_bar(data = temp, aes(x = reorder(IV, -perc), y = perc*100, fill = sig_sign), stat = 'identity', width = 0.1) +
    geom_point(data = temp, aes(x = IV, y = perc*100, fill = sig_sign), stat = 'identity', shape = 23, size = 3) +
    labs(x = '', y = '') + theme_light() + scale_fill_wsj() + coord_flip() +
    scale_y_continuous(name = '', labels = function(x) paste0(x,"%"), limits = c(-100, 100), breaks = seq(-100, 100, 25)) +
    geom_hline(yintercept = 0) + geom_hline(yintercept = c(90, -90), color = 'darkgreen', linetype = "dashed") +
    theme(axis.text.x = element_text(size = 12), 
          axis.text.y = element_text(size = 11), 
          legend.position = "bottom", 
          legend.title = element_blank(),
          legend.direction = 'horizontal')
  return(p1)
}

# FUNCTION TO CREATE PLOT FOR SIGNIFICANCE SHARE BY DIMENSION
createPlotSignShareDim <- function(dim){
  temp <- df[[dim]] %>% subset(., sig_sign != 'not significant')
  p1 <- ggplot() +
    geom_point(data = temp, aes(x = reorder(IV, -perc), y = perc*100, color = sig_sign, shape = dimension), size = 2.5, stat = 'identity') +
    labs(x = '', y = '') + theme_light() + scale_color_wsj() + coord_flip() +
    scale_y_continuous(name = '', labels = function(x) paste0(x,"%"), limits = c(-100, 100), breaks = seq(-100, 100, 25)) +
    scale_shape_manual(values = c(0:11)) +
    geom_hline(yintercept = 0) + geom_hline(yintercept = c(90, -90), color = 'darkgreen', linetype = "dashed") +
    theme(axis.text.x = element_text(size = 12), 
          axis.text.y = element_text(size = 11), 
          legend.position = "bottom", 
          legend.title = element_blank()) +
    guides(colour = 'none', shape = guide_legend(ncol = 4))
  return(p1)
}

# FUNCTION TO CREATE PLOT FOR RANGE OF SIGNIFICANCE SHARE
createPlotSignShareDimRange <- function(){
  temp <- list(df[['set']] %>% mutate(dim = 'standard error type'), 
               df[['fes']] %>% mutate(dim = 'fixed effect structure'),
               df[['deps']] %>% mutate(dim = 'measure of dependent variable'), 
               df[['csamples']] %>% mutate(dim = 'country sample'),
               df[['tsamples']] %>% mutate(dim = 'period sample')) %>% 
    bind_rows() %>% subset(., sig_sign != 'not significant') %>% 
    mutate(dim = factor(dim, c('country sample', 'period sample', 'measure of dependent variable', 
                               'fixed effect structure', 'standard error type'))) %>%
    dplyr::group_by(IV, dim) %>% dplyr::summarize(range = abs(diff(range(perc)))) %>% ungroup()
  p1 <- ggplot() +
    geom_point(data = temp, aes(x = reorder(IV, range), y = range*100, color = dim, shape = dim), size = 4, stat = 'identity') +
    labs(x = '', y = 'range of signifiance shares') + theme_light() + coord_flip() +
    scale_y_continuous(name = '', labels = function(x) paste0(x,"%"), limits = c(0, 150), breaks = seq(0, 1000, 10)) +
    scale_shape_manual(values = c(15, 17, 18, 19, 12)) +
    scale_color_jama() +
    theme(axis.text.x = element_text(size = 10), 
          legend.position = "bottom", 
          legend.title = element_blank())
  return(p1)
}

###

# FUNCTION TO COMBINE MLOGIT AND NN RESULTS
combineMlogitNNresults <- function(projectName){
  files_mlogit <- list.files(paste0('2data/5pred/', projectName, '/MLOGIT'), recursive = T, full.names = T) %>% subset(., grepl('MLOGIT\\_CONT|SOCSPEND', .))
  df_mlogit <- data.frame(stringsAsFactors = F)
  for(i in 1:length(files_mlogit)){
    load(files_mlogit[i])
    df_mlogit <- bind_rows(df_mlogit, results_mlogit)
  }
  files_nn <- list.files(paste0('2data/5pred/', projectName, '/NN'), recursive = T, full.names = T) %>% subset(., grepl('NN\\_CONT|SOCSPEND', .))
  df_nn <- data.frame(stringsAsFactors = F)
  for(i in 1:length(files_nn)){
    load(files_nn[i])
    df_nn <- results_nn[["fip"]][["Importance"]] %>% as.data.frame() %>%
      rownames_to_column(., 'SPEC') %>% purrr::set_names('SPEC', 'FIP') %>%
      as.data.frame() %>% mutate(FIP = as.numeric(FIP)) %>%
      mutate(val = results_nn$sign, IV = results_nn$var) %>%
      bind_rows(df_nn, .)
  }
  df_nn <- df_nn %>% mutate(FIP = FIP/100)
  df <- df_mlogit %>% 
    left_join(., df_nn, by = c('IV', 'val', 'SPEC')) %>%
    subset(., complete.cases(.)) %>%
    mutate(TYPE = case_when(grepl('CSAMPLE', SPEC) ~ 'country sample',
                            grepl('TSAMPLE', SPEC) ~ 'period sample',
                            grepl('DEP|sstran|socexp|socrig|change', SPEC) ~ 'dependent variable',
                            grepl('FE|model\\_type', SPEC) ~ 'fixed effect',
                            grepl('SET|se\\_type', SPEC) ~ 'standard error',
                            T ~ 'control set') %>%
             factor(., levels = c("control set", "standard error", "fixed effect", 
                                  "period sample", 'dependent variable',  "country sample"))) %>%
    mutate(PPchange = abs(prop_diff)) %>%
    group_by(IV, SPEC = TYPE) %>% dplyr::summarize_at(dplyr::vars(dplyr::matches('OR|CI|PPchange|FIP')), mean) %>% ungroup() %>%
    group_by(IV) %>% mutate(PPmin = min(PPchange), PPmax = max(PPchange)) %>% ungroup() %>%
    mutate(PPchange_bin = 'change in predicted probability (logistic regression)',
           FIP_bin = 'average feature importance score (SHAP value)')
  return(df)
}

# FUNCTION TO CREATE PLOT FOR FIP AND CHANGE IN PREDICTED PROBABILITY
createPPchangeFIPplot <- function(df){
  ggplot() +
    geom_bar(data = df, aes(y = PPchange, x = reorder(SPEC, desc(SPEC)), fill = PPchange_bin), stat = 'identity') +
    geom_point(data = df %>% subset(., SPEC == 'control set'), 
               aes(y = (FIP), x = reorder(SPEC, desc(SPEC)), fill = FIP_bin), 
               color = "black", size = 7, shape = 1, stroke = 1, fill = 'transparent',
               show.legend = FALSE) +
    geom_point(data = df, aes(y = (FIP), x = reorder(SPEC, desc(SPEC)), fill = FIP_bin), shape = 23, size = 5) +
    geom_vline(xintercept = 0) +
    theme_light() +
    scale_y_continuous(name = 'average change in predicted probability of signifiance classification (grey bars)',
                       labels = scales::percent,
                       sec.axis = sec_axis(~ ., labels = scales::percent,
                                           name = 'average feature importance score (SHAP value) of signifiance classification (red diamonds)')) +
    scale_fill_manual(values = c('#E91D0E', 'grey70')) +
    xlab('model specification') + 
    theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1, size = 12),
          axis.text.y = element_text(size = 8),
          strip.text.x = element_text(size = 10),
          plot.margin = margin(20, 20, 100, 20),
          legend.position = 'top', 
          legend.text = element_text(size = 12),
          legend.title = element_blank()) +
    facet_wrap(~IV, ncol = 3, scales = 'free_y')
}

# FUNCTION TO CREATE ODDS RATIO PLOT
createOddsRatioPlot <- function(projectName){
  files_mlogit <- list.files(paste0('2data3/', projectName, '_results/RES/MLOGIT'), recursive = T, full.names = T) %>% subset(., grepl('MLOGIT\\_CONT|SOCSPEND', .))
  df_mlogit <- data.frame(stringsAsFactors = F)
  for(i in 1:length(files_mlogit)){
    load(files_mlogit[i])
    df_mlogit <- bind_rows(df_mlogit, results_mlogit)
  }
  temp <- df_mlogit %>% 
    subset(., val != 'not significant') %>%
    subset(., IV != 'CONTpolinterest') %>%
    mutate(TYPE = case_when(grepl('CSAMPLE', SPEC) ~ 'country sample',
                            grepl('TSAMPLE', SPEC) ~ 'period sample',
                            grepl('DEP|sstran|socexp|socrig|change', SPEC) ~ 'dependent variable',
                            grepl('FE|model\\_type', SPEC) ~ 'fixed effect',
                            grepl('SET|se\\_type', SPEC) ~ 'standard error',
                            T ~ 'control set') %>%
             factor(., levels = c("control set", "standard error", "fixed effect", 
                                  "period sample", 'dependent variable',  "country sample"))) %>%
    group_by(TYPE, sign_class = val) %>% dplyr::summarize(OR = mean(abs(prop_diff), na.rm = T)) %>% ungroup() %>%
    mutate(sign_class = sign_class %>% gsub('positive significant', 'significant positive', .) %>% gsub('negative significant', 'significant negative', .))
  p1 <- ggplot() +
    geom_point(data = temp, aes(y = OR, x = TYPE, color = sign_class, fill = sign_class, group = sign_class), 
               size = 5, shape = 23, position = position_dodge(width = 0.6)) +
    geom_hline(yintercept = 0, linetype = 'dashed') +
    scale_fill_manual(values = c('significant negative' = "#EE0000FF",
                                 'significant positive' = "#008B45FF",
                                 'not significant' = "#3B4992FF")) + 
    scale_color_manual(values = c('significant negative' = "#EE0000FF",
                                  'significant positive' = "#008B45FF",
                                  'not significant' = "#3B4992FF")) +
    theme_light() +
    xlab('') +
    scale_y_continuous(name = paste0('(mean) change in predicted probability'), 
                       breaks = seq(0, 2, 0.025)) +
    ylab('independent variables') + 
    coord_flip() +
    theme(axis.text.x = element_text(vjust = 1, hjust = 1, size = 12),
          axis.text.y = element_text(size = 12),
          strip.text.x = element_text(size = 12),
          legend.text = element_text(size = 12),
          legend.position = 'bottom', 
          legend.title = element_blank(),
          legend.direction = 'horizontal')
  return(p1)
}

# FUNCTION TO CREATE FIP PLOT
createFIPPlot <- function(projectName){
  files_nn <- list.files(paste0('2data/5pred/', projectName, '/NN'), recursive = T, full.names = T) %>% subset(., grepl('NN\\_CONT|SOCSPEND', .))
  df_nn <- data.frame(stringsAsFactors = F)
  for(i in 1:length(files_nn)){
    load(files_nn[i])
    df_nn <- results_nn[["fip"]][["Importance"]] %>% as.data.frame() %>%
      rownames_to_column(., 'SPEC') %>% purrr::set_names('SPEC', 'FIP') %>%
      as.data.frame() %>% mutate(FIP = as.numeric(FIP)) %>%
      mutate(val = results_nn$sign, IV = results_nn$var) %>%
      bind_rows(df_nn, .)
  }
  df_nn <- df_nn %>% mutate(FIP = FIP/100)
  temp <- df_nn %>% 
    mutate(TYPE = case_when(grepl('CSAMPLE', SPEC) ~ 'country sample',
                            grepl('TSAMPLE', SPEC) ~ 'period sample',
                            grepl('DEP|sstran|socexp|socrig|change', SPEC) ~ 'dependent variable',
                            grepl('FE|model\\_type', SPEC) ~ 'fixed effect',
                            grepl('SET|se\\_type', SPEC) ~ 'standard error',
                            T ~ 'control set') %>%
             factor(., levels = c("control set", "standard error", "fixed effect", 
                                  "period sample", 'dependent variable',  "country sample"))) %>%
    group_by(TYPE, sign_class = val) %>% dplyr::summarize_at(dplyr::vars(dplyr::matches('FIP')), mean, na.rm = T) %>% ungroup() %>%
    mutate(sign_class = sign_class %>% gsub('positive significant', 'significant positive', .) %>% gsub('negative significant', 'significant negative', .))
  p1 <- ggplot() +
    geom_point(data = temp, aes(y = FIP, x = TYPE, color = sign_class, fill = sign_class, group = sign_class), 
               size = 5, shape = 23, position = position_dodge(width = 0.6)) +
    geom_hline(yintercept = 0, linetype = 'dashed') +
    scale_fill_manual(values = c('significant negative' = "#EE0000FF",
                                 'significant positive' = "#008B45FF",
                                 'not significant' = "#3B4992FF")) + 
    scale_color_manual(values = c('significant negative' = "#EE0000FF",
                                'significant positive' = "#008B45FF",
                                'not significant' = "#3B4992FF")) +
    theme_light() +
    xlab('') +
    scale_y_continuous(name = paste0('feature importance'), 
                       breaks = seq(0, 2, 0.25), limits = c(0, 1)) +
    coord_flip() +
    ylab('independent variables') + 
    theme(axis.text.x = element_text(vjust = 1, hjust = 1, size = 12),
          axis.text.y = element_text(size = 12),
          strip.text.x = element_text(size = 12),
          legend.text = element_text(size = 12),
          legend.position = 'bottom', 
          legend.title = element_blank(),
          legend.direction = 'horizontal')
  return(p1)
}

# FUNCTION TO CREATE PLOT OF FIP BY VARIABLES
createFIPVARIABLEPlot <- function(projectName){
  files_nn <- list.files(paste0('2data3/', projectName, '_results/RES/NN'), recursive = T, full.names = T) %>% subset(., grepl('NN\\_CONT|SOCSPEND', .))
  df_nn <- data.frame(stringsAsFactors = F)
  for(i in 1:length(files_nn)){
    load(files_nn[i])
    df_nn <- results_nn[["fip"]][["Importance"]] %>% as.data.frame() %>%
      rownames_to_column(., 'SPEC') %>% purrr::set_names('SPEC', 'FIP') %>%
      as.data.frame() %>% mutate(FIP = as.numeric(FIP)) %>%
      mutate(val = results_nn$sign, IV = results_nn$var) %>%
      bind_rows(df_nn, .)
    print(i)
  }
  df_nn <- df_nn %>% mutate(FIP = FIP/100)
  temp <- df_nn %>% 
    mutate(TYPE = case_when(grepl('CSAMPLE', SPEC) ~ 'country sample',
                            grepl('TSAMPLE', SPEC) ~ 'period sample',
                            grepl('DEP|sstran|socexp|socrig|change', SPEC) ~ 'dependent variable',
                            grepl('FE|model\\_type', SPEC) ~ 'fixed effect',
                            grepl('SET|se\\_type', SPEC) ~ 'standard error',
                            T ~ 'control set') %>%
             factor(., levels = c("control set", "standard error", "fixed effect", 
                                  "period sample", 'dependent variable',  "country sample"))) %>%
    group_by(TYPE, sign_class = val) %>% dplyr::summarize_at(vars(matches('FIP')), mean, na.rm = T) %>% ungroup() %>%
    mutate(sign_class = sign_class %>% gsub('positive significant', 'significant positive', .) %>% gsub('negative significant', 'significant negative', .))
  p1 <- ggplot() +
    geom_point(data = temp, aes(y = FIP, x = TYPE, color = sign_class, fill = sign_class, group = sign_class), 
               size = 5, shape = 23, position = position_dodge(width = 0.6)) +
    geom_hline(yintercept = 0, linetype = 'dashed') +
    scale_fill_manual(values = c('significant negative' = "#EE0000FF",
                                 'significant positive' = "#008B45FF",
                                 'not significant' = "#3B4992FF")) + 
    scale_color_manual(values = c('significant negative' = "#EE0000FF",
                                  'significant positive' = "#008B45FF",
                                  'not significant' = "#3B4992FF")) +
    theme_light() +
    xlab('') +
    scale_y_continuous(name = paste0('feature importance'), 
                       breaks = seq(0, 2, 0.25), limits = c(0, 1)) +
    coord_flip() +
    ylab('independent variables') + 
    theme(axis.text.x = element_text(vjust = 1, hjust = 1, size = 12),
          axis.text.y = element_text(size = 12),
          strip.text.x = element_text(size = 12),
          legend.text = element_text(size = 12),
          legend.position = 'bottom', 
          legend.title = element_blank(),
          legend.direction = 'horizontal')
  return(p1)
}

######

# FUNCTION TO CREATE PLOT FOR CHANGE IN PREDICTED PROBABILITY AND FIP
createPPandFIPplot <- function(projectName){
  files_nn <- list.files(paste0('2data3/', projectName, '_results/RES/NN'), recursive = T, full.names = T) %>% subset(., grepl('NN\\_CONT|SOCSPEND', .))
  df_nn <- data.frame(stringsAsFactors = F)
  for(i in 1:length(files_nn)){
    load(files_nn[i])
    df_nn <- results_nn[["fip"]][["Importance"]] %>% as.data.frame() %>%
      rownames_to_column(., 'SPEC') %>% purrr::set_names('SPEC', 'FIP') %>%
      as.data.frame() %>% mutate(FIP = as.numeric(FIP)) %>%
      mutate(val = results_nn$sign, IV = results_nn$var) %>%
      bind_rows(df_nn, .)
    print(i)
  }
  df_nn <- df_nn %>% mutate(FIP = FIP/100)
  df_nn <- df_nn %>% 
    mutate(TYPE = case_when(grepl('CSAMPLE', SPEC) ~ 'country sample',
                            grepl('TSAMPLE', SPEC) ~ 'period sample',
                            grepl('DEP|sstran|socexp|socrig|change', SPEC) ~ 'dependent variable',
                            grepl('FE|model\\_type', SPEC) ~ 'fixed effect',
                            grepl('SET|se\\_type', SPEC) ~ 'standard error',
                            T ~ 'control set') %>%
             factor(., levels = c("control set", "standard error", "fixed effect", 
                                  "period sample", 'dependent variable',  "country sample"))) %>%
    group_by(TYPE, sign_class = val) %>% dplyr::summarize_at(vars(matches('FIP')), mean, na.rm = T) %>% ungroup() %>%
    mutate(sign_class = sign_class %>% gsub('positive significant', 'significant positive', .) %>% gsub('negative significant', 'significant negative', .))
  
  files_mlogit <- list.files(paste0('2data3/', projectName, '_results/RES/MLOGIT'), recursive = T, full.names = T) %>% subset(., grepl('MLOGIT\\_CONT|SOCSPEND', .))
  df_mlogit <- data.frame(stringsAsFactors = F)
  for(i in 1:length(files_mlogit)){
    load(files_mlogit[i])
    df_mlogit <- bind_rows(df_mlogit, results_mlogit)
  }
  df_mlogit <- df_mlogit %>% 
    subset(., val != 'not significant') %>%
    mutate(TYPE = case_when(grepl('CSAMPLE', SPEC) ~ 'country sample',
                            grepl('TSAMPLE', SPEC) ~ 'period sample',
                            grepl('DEP|sstran|socexp|socrig|change', SPEC) ~ 'dependent variable',
                            grepl('FE|model\\_type', SPEC) ~ 'fixed effect',
                            grepl('SET|se\\_type', SPEC) ~ 'standard error',
                            T ~ 'control set') %>%
             factor(., levels = c("control set", "standard error", "fixed effect", 
                                  "period sample", 'dependent variable',  "country sample"))) %>%
    group_by(TYPE, sign_class = val) %>% dplyr::summarize(OR = mean(abs(prop_diff), na.rm = T)) %>% ungroup()
  ggplot() +
    geom_point(data = df_nn, aes(y = FIP, x = TYPE, color = sign_class, fill = sign_class, group = sign_class), 
               size = 5, shape = 23, alpha = 0.75, position = position_dodge(width = 0.6)) +
    geom_hline(yintercept = 0, linetype = 'dashed') +
    theme_light() +
    xlab('') +
    scale_fill_manual(values = c('significant negative' = "#EE0000FF",
                                 'significant positive' = "#008B45FF",
                                 'not significant' = "#3B4992FF")) + 
    scale_color_manual(values = c('significant negative' = "#EE0000FF",
                                  'significant positive' = "#008B45FF",
                                  'not significant' = "#3B4992FF")) +
    scale_y_continuous(name = paste0('feature importance (SHAP values) (diamonds)'), 
                       breaks = seq(0, 2, 0.25), limits = c(0, 1)) +
    ylab('independent variables') + 
    theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1),
          strip.text.x = element_text(size = 12),
          legend.text = element_text(size = 12),
          legend.position = 'bottom', 
          legend.title = element_blank())
}

######

# FUNCTION TO CREATE PLOT FOR SIGNIFICANCE CHANGE AFTER ONE MODEL CHANGE
createModelSpecChangePlot <- function(projectName){
  load(paste0("2data/6nneigh/", projectName, "/neigh.RData"))
  temp <- res %>%
    mutate(sig_sign_change = as.numeric(outcome != outcome_change)) %>%
    mutate(TYPE = case_when(grepl('CSAMPLE', changeSpec) ~ 'country sample',
                            grepl('TSAMPLE', changeSpec) ~ 'period sample',
                            grepl('DEP|change', changeSpec) ~ 'dependent variable',
                            grepl('FE', changeSpec) ~ 'fixed effect',
                            grepl('SE', changeSpec) ~ 'standard error',
                            T ~ 'control set') %>%
             factor(., levels = c("control set", "standard error", "fixed effect", 
                                  "period sample", 'dependent variable',  "country sample"))) %>%
    dplyr::group_by(TYPE) %>% dplyr::summarize(val = mean(sig_sign_change, na.rm = T)) %>% ungroup() %>%
    mutate(projectName = projectName)
  p1 <- ggplot() +
    geom_point(data = temp, aes(x = val, y = TYPE, color = TYPE, fill = TYPE, group = TYPE), 
               size = 5, shape = 23, position = position_dodge(width = 0.6)) +
    geom_hline(yintercept = 0, linetype = 'dashed') +
    scale_color_aaas() + scale_fill_aaas() +
    theme_minimal() +
    xlab('') +
    scale_x_continuous(name = paste0('share of changing significance class\n(positive/negative/not significant)'), limits = c(0, 0.5), labels = scales::percent) +
    ylab('model specification') + 
    theme(axis.text.x = element_text(vjust = 1, hjust = 0.5, size = 12),
          axis.text.y = element_text(size = 12),
          strip.text.x = element_text(size = 12),
          legend.text = element_text(size = 12),
          legend.position = 'bottom', 
          legend.title = element_blank(),
          legend.direction = 'horizontal',
          panel.border = element_rect(colour = "black", fill = NA, size = 0.5))
  return(p1)
}

# FUNCTION TO ADD VARIABLE LABELS FOR VISUALZIATIONS
addVariableLabels <- function(df){
  df <- df %>%
    lapply(., function(x){
      if(!'dimension' %in% colnames(x)){
        x <- x %>% 
          mutate(IV = IV %>% gsub('_', '', .)) %>%
          left_join(., lab, by = c('IV' = 'oldval')) %>% mutate(IV = newval)
      }else{
        x <- x %>% 
          mutate(IV = IV %>% gsub('_', '', .)) %>%
          left_join(., lab, by = c('IV' = 'oldval')) %>% mutate(IV = newval) %>% dplyr::select(-newval) %>%
          left_join(., lab, by = c('dimension' = 'oldval')) %>% mutate(dimension = newval)
      }
    })
  return(df)
}
