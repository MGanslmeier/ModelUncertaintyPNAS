# LOAD DATA
fileName <- paste0('2data/1raw/', projectName, '/temp.RData')
load(fileName)

###

# DEFINE PARAMETERS
conts <- colnames(df) %>% subset(., grepl('CONT', .))
deps <- colnames(df) %>% subset(., grepl('DEP', .))
csamples <- colnames(df) %>% subset(., grepl('CSAMPLE', .))
tsamples <- colnames(df) %>% subset(., grepl('TSAMPLE', .))
fes <- colnames(df) %>% subset(., grepl("FE", .)) %>% paste0('factor(', ., ')') %>% c("factor(FEunit)+factor(FEtime)", '')

###

# CREATE CONTROL SETS
contset <- 1:(length(conts)-1) %>%
  lapply(., function(x) t(combn(conts, length(conts)-x))) %>%
  lapply(., function(x) apply(x, 1, paste, collapse = '+')) %>% unlist()

###

# DEFINE MAXIMUM MODEL SPACE
space_n <- 150000

# DEFINE SPECIFICATIONS
spec_pool <- list(deps = deps, contset = contset, fes = fes, csamples = csamples, tsamples = tsamples)
universe_n <- lapply(spec_pool, function(x) length(x)) %>% as.numeric() %>% prod()
specs <- expand.grid(spec_pool, stringsAsFactors = F)
specs <- specs %>%
  mutate(MODELID = row.names(.) %>% str_pad(., width = 15, pad = '0')) %>%
  dplyr::select(MODELID, everything(.)) %>%
  sample_n(space_n)

# SAVE SPEC
fileName <- paste0('2data/2spec/', projectName, '/spec.RData')
save(specs, file = fileName)
