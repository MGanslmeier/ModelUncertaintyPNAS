# SPLIT DATA
spec <- c(train = 0.6, test = 0.2, cv = 0.2)
g <- sample(cut(seq(nrow(res)), nrow(res)*cumsum(c(0, spec)), labels = names(spec)))
temp = res %>% .[sample(nrow(.)),] %>% split(., g)
train <- temp$train
cv <- temp$cv
test <- temp$test

# DEFINE OUTCOME AND FEATURES
train.x <- as.matrix(train %>% dplyr::select(-outcome))
train.y <- matrix(as.numeric(train$outcome)) %>% to_categorical(.)
test.x <- as.matrix(test %>% dplyr::select(-outcome))
test.y <- matrix(as.numeric(test$outcome)) %>% to_categorical(.)
cv.x <- as.matrix(cv %>% dplyr::select(-outcome))
cv.y <- matrix(as.numeric(cv$outcome)) %>% to_categorical(.)

# DEFINE HYPERPARAMETER
grid_search <- list(
  dropout = c(0, 0.3),
  layers = c(5, 7),
  units = c(32, 64),
  learning_rate = c(0.001)
)

# RUN GRID SEARCH
runs_dir <- paste0('2data/5pred/', projectName, '/NN/runs', focus)
unlink(runs_dir, recursive = T, force = T)
runs <- tuning_run('1code/5_1b_dl_helper.R', flags = grid_search, runs_dir = runs_dir, confirm = F, echo = F)

# RUN BEST MODEL
best_run <- ls_runs(runs_dir =  runs_dir) %>%
  subset(., metric_val_accuracy == max(metric_val_accuracy, na.rm = T)) %>% .[1,]
FLAGS <- list(layers = as.integer(best_run$flag_layers),
              units = as.integer(best_run$flag_units),
              dropout = as.numeric(best_run$flag_dropout),
              learning_rate = as.numeric(best_run$flag_learning_rate))
run <- training_run('1code/5_1b_dl_helper.R', flags = FLAGS, run_dir = runs_dir)

# MAKE PREDICTIONS WITH BEST MODEL
perform_res <- list()
dat_types <- c('train', 'cv', 'test')
for(k in 1:length(dat_types)){
  x <- mget(paste0(dat_types[k], '.x'))[[1]]
  y <- mget(paste0(dat_types[k], '.y'))[[1]]
  pred <- network %>% predict(x) %>% .[,2] %>% as.data.frame() %>% 
    mutate(pred = round(.)) %>%
    cbind(., y[,2]) %>% as.data.frame() %>%
    purrr::set_names('pred_prob', 'pred', 'truth')
  cm <- table(pred$pred, pred$truth)
  auc_roc <- auc_roc(pred$pred_prob, pred$truth)
  pred_temp <- pred %>% mutate(truth = as.factor(truth)) %>% mutate(pred = factor(pred, levels = levels(truth)))
  met <- data.frame(stringsAsFactors = F, auc_roc = auc_roc,
                    accuracy = yardstick::accuracy(pred_temp, truth = 'truth', estimate = 'pred') %>% pull(.estimate),
                    precision = yardstick::precision(pred_temp, truth = 'truth', estimate = 'pred', event_level = 'first') %>% pull(.estimate),
                    recall = yardstick::recall(pred_temp, truth = 'truth', estimate = 'pred', event_level = 'first') %>% pull(.estimate),
                    f1 = yardstick::f_meas(pred_temp, truth = 'truth', estimate = 'pred', event_level = 'first') %>% pull(.estimate),
                    kap = yardstick::kap(pred_temp, truth = 'truth', estimate = 'pred', event_level = 'first') %>% pull(.estimate),
                    dat_type = dat_types[k])
  perform_res[[k]] <- list(cm = cm, met = met)
}

# CALCULATE SHAP VALUES
pred_wrapper <- function(object, newdata) {
  predict(object, x = as.matrix(newdata)) %>% as.vector()
}
train_temp <- as.data.frame(train.x)
fip <- network %>%
  vip::vi(., train = train_temp, feature_names = colnames(train_temp),
          pred_wrapper = pred_wrapper, method = "shap", scale = T)

# CALCULATE FURTHER FEATURE IMPORTANCE MEASURES
pred_wrapper_perm <- function(model, newdata) {
  predict(model, x = as.matrix(newdata))[,2]
}
test_temp <- as.data.frame(test.x)
predictor <- Predictor$new(model = network, data = test_temp, y = test.y[, 2], predict.fun = pred_wrapper_perm)
tryCatch({
  FeatureImp_imp <- FeatureImp$new(predictor, loss = "ce")[['results']]
}, error = function(e){FeatureImp_imp = list()})
if(!exists('FeatureImp_imp')){
  FeatureImp_imp <- list()
}
tryCatch({
  FeatureEffect_imp <- pblapply(colnames(train.x), function(x) iml::FeatureEffect$new(predictor, feature = x, method = "ale") %>%
                                  .[['results']] %>% purrr::set_names('type', 'value', 'feature') %>% mutate(IV = x)) %>% bind_rows()
}, error = function(e){FeatureEffect_imp = list(NULL)})
if(!exists('FeatureEffect_imp')){
  FeatureEffect_imp <- list()
}
tryCatch({
  K <- length(colnames(train))-1
  LocalModel_imp <- pblapply(sample((1:nrow(train)), 500), function(x) LocalModel$new(predictor, x.interest = train[x,], k = K)[['results']]) %>% bind_rows()
}, error = function(e){LocalModel_imp = list()})
if(!exists('LocalModel_imp')){
  LocalModel_imp <- list()
}
fip2 <- list(FeatureImp = FeatureImp_imp, FeatureEffect = FeatureEffect_imp, LocalModel = LocalModel_imp)

# STORE RESULTS
results_nn <- list(sign = sign, var = focus, fip = fip, fip2 = fip2,
                   train_run = run, tune_run = runs,  
                   best_run = best_run, perform_res = perform_res)
save(results_nn, file = fileName)
unlink(runs_dir, recursive = T, force = T)

