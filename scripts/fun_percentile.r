library(tidyverse)
library(bayesboot)

pairwisePercentileTest = function (column = NULL, group = NULL, data = NULL,
          tau = 0.5, type = 7, threshold = NA, comparison = "<", r = 1000, 
          digits = 4, progress = "FALSE", method = "fdr"){
  x = data[[column]]
  g = data[[group]]
  
  if (is.factor(g)) {
    g = droplevels(g)
  }
  if (!is.factor(g)) {
    g = factor(g)
  }
  n = length(levels(g))
  N = n * (n - 1)/2
  d = data.frame(x = x, g = g)
  Z = data.frame(Comparison = rep("A", N), p.value = rep(NA, N), p.adjust = rep(NA, N), stringsAsFactors = FALSE)
  k = 0
  for (i in 1:(n - 1)) {
    for (j in (i + 1):n) {
      k = k + 1
      if (progress) {
        cat("Comparison ", k, "\n")
      }
      Namea = as.character(levels(g)[i])
      Nameb = as.character(levels(g)[j])
      Datax = subset(d, g == levels(g)[i])
      Datay = subset(d, g == levels(g)[j])
      Dataz = rbind(Datax, Datay)
      Dataz$g2 = droplevels(Dataz$g)
      z = percentileTest(x ~ g2, data = Dataz,
                         tau = tau, type = type, threshold = threshold, 
                         comparison = comparison, r = r, digits = digits, 
                         progress = progress)
      P = signif(z[["Result"]]$p.value, digits = digits)
      P.adjust = NA
      Z[k, ] = c(paste0(Namea, " - ", Nameb), P, 
                 P.adjust)
    }
  }
  Z$p.adjust = signif(p.adjust(Z$p.value, method = method), 
                      digits = digits)
  return(Z)
}

percentileTest = function (formula = NULL, data = NULL, x = NULL, y = NULL, 
          tau = 0.5, type = 7, threshold = NA, comparison = "<", r = 1000, 
          digits = 4, progress = "FALSE", test = 'percentile'){
  if (is.null(formula)) {
    xy = c(x, y)
    xname = paste(as.character(substitute(x)), collapse = " ")
    yname = paste(as.character(substitute(y)), collapse = " ")
    g = factor(c(rep(xname, length(x)), rep(yname, length(y))))
  }
  if (!is.null(formula)) {
    xy = eval(parse(text = paste0("data", "$", all.vars(formula[[2]])[1])))
    g = eval(parse(text = paste0("data", "$", all.vars(formula[[3]])[1])))
    if (!is.factor(g)) {
      g = factor(g)
    }
  }
  Complete = complete.cases(xy, g)
  xy = xy[Complete]
  g = g[Complete]
  g = droplevels(g)
  x = xy[g == levels(g)[1]]
  y = xy[g == levels(g)[2]]
  xy = c(x, y)
  PropA = NA
  PropB = NA
  Diff = abs(quantile(x, probs = tau, type = type) - quantile(y, probs = tau, type = type))

    Count = 0
  if (progress) {
    tick = r/100
  }
  for (i in 1:r) {
    S = sample(xy, replace = TRUE)
    Sx = S[g == levels(g)[1]]
    Sy = S[g == levels(g)[2]]
    Sdiff = abs(quantile(Sx, probs = tau, type = type) - quantile(Sy, probs = tau, type = type))
    if (Sdiff >= Diff) {
      Count = Count + 1
    }
  }
  P.value = Count/r
  Z = data.frame(Statistic = rep("NA", 2), 
                 n = rep(as.numeric(NA), 2), 
                 mean = rep(as.numeric(NA), 2), 
                 sd = rep(as.numeric(NA), 2), 
                 min = rep(as.numeric(NA), 2), 
                 p25 = rep(as.numeric(NA), 2), 
                 median = rep(as.numeric(NA), 2), 
                 p75 = rep(as.numeric(NA), 2), 
                 max = rep(as.numeric(NA), 2),
                 iqr = rep(as.numeric(NA), 2), 
                 comparison = rep("NA", 2), 
                 threshold = rep(as.numeric(NA), 2), 
                 proportion = rep(as.numeric(NA), 2), stringsAsFactors = FALSE)
  
  Z[1, 1] = levels(g)[1]
  Z[1, 2] = signif(length(x), digits)
  Z[1, 3] = signif(mean(x), digits)
  Z[1, 4] = signif(sd(x), digits)
  Z[1, 5] = signif(min(x), digits)
  Z[1, 6] = signif(quantile(x, 0.25), digits)
  Z[1, 7] = signif(median(x), digits)
  Z[1, 8] = signif(quantile(x, 0.75), digits)
  Z[1, 9] = signif(max(x), digits)
  Z[1, 10] = signif(IQR(x), digits)
  Z[1, 11] = comparison
  Z[1, 12] = signif(threshold, digits)
  Z[1, 13] = signif(PropA, digits)
  Z[2, 1] = levels(g)[2]
  Z[2, 2] = signif(length(y), digits)
  Z[2, 3] = signif(mean(y), digits)
  Z[2, 4] = signif(sd(y), digits)
  Z[2, 5] = signif(min(y), digits)
  Z[2, 6] = signif(quantile(y, 0.25), digits)
  Z[2, 7] = signif(median(y), digits)
  Z[2, 8] = signif(quantile(y, 0.75), digits)
  Z[2, 9] = signif(max(y), digits)
  Z[2, 10] = signif(IQR(y), digits)
  Z[2, 11] = comparison
  Z[2, 12] = signif(threshold, digits)
  Z[2, 13] = signif(PropB, digits)
  colnames(Z)[1] = ""

  U = data.frame(Statistic = rep("NA", 1), p.value = rep(as.numeric(NA), 
                                                         1), stringsAsFactors = FALSE)
  U[1, 1] = "p-value"
  U[1, 2] = signif(P.value, digits)
  colnames(U)[1] = ""
  V = data.frame(Formula = "NA", Data = "NA", Test = "NA", 
                 tau = as.numeric(NA), stringsAsFactors = FALSE)
  if (!is.null(formula)) {
    V[1, 1] = deparse(formula)
  }
  if (!is.null(formula)) {
    V[1, 2] = as.character(substitute(data))
  }
  V[1, 3] = test
  V[1, 4] = tau
  if (is.null(formula)) {
    V[1, 1] = xname
    V[1, 2] = yname
    colnames(V)[1:2] = c("x", "y")
  }

  W = list(Test = V, Summary = Z, Result = U)
  return(W)
}


bootanalysis = function (data = NULL, column = NULL, r = NULL, upper_conf = NULL){
  
  low_conf = 1 - upper_conf

  # Filter Samples
  good.cp = data %>% 
    filter(Region == 'Coastal Plain') %>% 
    filter(Biotic.Classification == 'Good Condition')
  
  moderate.cp = data %>% 
    filter(Region == 'Coastal Plain') %>% 
    filter(Biotic.Classification == 'Moderately Degraded')
  
  severe.cp = data %>% 
    filter(Region == 'Coastal Plain') %>% 
    filter(Biotic.Classification == 'Severely Degraded')
  
  if (!all(is.na(good.cp[[column]]))){
    good.boot.cp = bayesboot(good.cp[[column]], 
                              weighted.mean, na.rm = TRUE, 
                              R = r, R2 = r, use.weights = TRUE)
    
    moderate.boot.cp = bayesboot(moderate.cp[[column]], 
                                  weighted.mean, na.rm = TRUE, 
                                  R = r, R2 = r, use.weights = TRUE)
    
    severe.boot.cp = bayesboot(severe.cp[[column]], 
                                weighted.mean, na.rm = TRUE, 
                                R = r, R2 = r, use.weights = TRUE)
  }

  good.pied = data %>% 
    filter(Region == 'Piedmont') %>% 
    filter(Biotic.Classification == 'Good Condition')
  
  moderate.pied = data %>% 
    filter(Region == 'Piedmont') %>% 
    filter(Biotic.Classification == 'Moderately Degraded')
  
  severe.pied = data %>% 
    filter(Region == 'Piedmont') %>% 
    filter(Biotic.Classification == 'Severely Degraded')

  if (!all(is.na(good.pied[[column]]))){
    good.boot.pied = bayesboot(good.pied[[column]], 
                                weighted.mean, na.rm = TRUE, 
                                R = r, R2 = r, use.weights = TRUE)
    
    moderate.boot.pied = bayesboot(moderate.pied[[column]], 
                                    weighted.mean, na.rm = TRUE, 
                                    R = r, R2 = r, use.weights = TRUE)
    
    severe.boot.pied = bayesboot(severe.pied[[column]], 
                                  weighted.mean, na.rm = TRUE, 
                                  R = r, R2 = r, use.weights = TRUE)
  }
  
  if (!all(is.na(good.cp[[column]])) & !all(is.na(good.pied[[column]]))){
      boot.combined = tribble(~'Region', ~'Condition', ~'Lower CI', ~'Mean',  ~'Upper CI', 
                        'Coastal Plain', 'Good Condition', signif(quantile(good.boot.cp$V1, low_conf), digits = 4), signif(mean(good.boot.cp$V1), digits = 4), signif(quantile(good.boot.cp$V1, upper_conf), digits = 4), 
                        'Coastal Plain', 'Moderately Degraded', signif(quantile(moderate.boot.cp$V1, low_conf), digits = 4), signif(mean(moderate.boot.cp$V1), digits = 4), signif(quantile(moderate.boot.cp$V1, upper_conf), digits = 4),
                        'Coastal Plain', 'Severely Degraded', signif(quantile(severe.boot.cp$V1, low_conf), digits = 4), signif(mean(severe.boot.cp$V1), digits = 4), signif(quantile(severe.boot.cp$V1, upper_conf), digits = 4),
                        'Piedmont', 'Good Condition', signif(quantile(good.boot.pied$V1, low_conf), digits = 4), signif(mean(good.boot.pied$V1), digits = 4), signif(quantile(good.boot.pied$V1, upper_conf), digits = 4), 
                        'Piedmont', 'Moderately Degraded', signif(quantile(moderate.boot.pied$V1, low_conf), digits = 4), signif(mean(moderate.boot.pied$V1), digits = 4), signif(quantile(moderate.boot.pied$V1, upper_conf), digits = 4),
                        'Piedmont', 'Severely Degraded', signif(quantile(severe.boot.pied$V1, low_conf), digits = 4), signif(mean(severe.boot.pied$V1), digits = 4), signif(quantile(severe.boot.pied$V1, upper_conf), digits = 4)
      )
  } else if(!all(is.na(good.cp[[column]]))){
      boot.combined = tribble(~'Region', ~'Condition', ~'Lower CI', ~'Mean',  ~'Upper CI', 
                              'Coastal Plain', 'Good Condition', signif(quantile(good.boot.cp$V1, low_conf), digits = 4), signif(mean(good.boot.cp$V1), digits = 4), signif(quantile(good.boot.cp$V1, upper_conf), digits = 4), 
                              'Coastal Plain', 'Moderately Degraded', signif(quantile(moderate.boot.cp$V1, low_conf), digits = 4), signif(mean(moderate.boot.cp$V1), digits = 4), signif(quantile(moderate.boot.cp$V1, upper_conf), digits = 4),
                              'Coastal Plain', 'Severely Degraded', signif(quantile(severe.boot.cp$V1, low_conf), digits = 4), signif(mean(severe.boot.cp$V1), digits = 4), signif(quantile(severe.boot.cp$V1, upper_conf), digits = 4)
      )
  } else if (!all(is.na(good.pied[[column]]))){
    boot.combined = tribble(~'Region', ~'Condition', ~'Lower CI', ~'Mean',  ~'Upper CI', 
                            'Piedmont', 'Good Condition', signif(quantile(good.boot.pied$V1, low_conf), digits = 4), signif(mean(good.boot.pied$V1), digits = 4), signif(quantile(good.boot.pied$V1, upper_conf), digits = 4), 
                            'Piedmont', 'Moderately Degraded', signif(quantile(moderate.boot.pied$V1, low_conf), digits = 4), signif(mean(moderate.boot.pied$V1), digits = 4), signif(quantile(moderate.boot.pied$V1, upper_conf), digits = 4),
                            'Piedmont', 'Severely Degraded', signif(quantile(severe.boot.pied$V1, low_conf), digits = 4), signif(mean(severe.boot.pied$V1), digits = 4), signif(quantile(severe.boot.pied$V1, upper_conf), digits = 4)
    )
  }

  return(boot.combined)
}

ratioanalysis = function(data = NULL, column = NULL, r = NULL, upper_conf = NULL, tau = NULL){

  # Filter based on region
  cp = data %>% filter(Region == 'Coastal Plain')
  
  pied = data %>% filter(Region == 'Piedmont')
  
  # Run percentile tests

  if (!all(is.na(pied[[column]]))){
    dat.pied = pairwisePercentileTest(data = pied,
                                  group = 'Biotic.Classification',
                                  column = column,
                                  tau = tau,
                                  r = r)

    table.pied = dat.pied %>% 
      select(Comparison, p.adjust) %>% 
      mutate('Significant?' = ifelse(p.adjust <= 0.001, '***', 
                                    ifelse(p.adjust <= 0.01, '**', 
                                            ifelse(p.adjust <= 0.05, '*', NA))), 
            Region = 'Piedmont') %>%
      as_tibble()
  } else {
    table.pied = tibble('Comparison' = NA,
                        'p.adjust' = NA,
                        'Significant?' = NA,
                        'Region' = 'Piedmont')
  }

  if (!all(is.na(cp[[column]]))) {
      dat.cp = pairwisePercentileTest(data = cp,
                                    group = 'Biotic.Classification',
                                    column = column,
                                    tau = tau,
                                    r = r)
      
      table.cp = dat.cp %>% 
        select(Comparison, p.adjust) %>% 
        mutate('Significant?' = ifelse(p.adjust <= 0.001, '***', 
                                       ifelse(p.adjust <= 0.01, '**', 
                                              ifelse(p.adjust <= 0.05, '*', NA))), 
               Region = 'Coastal Plain') %>% 
        as_tibble()
    } else {
      table.cp = tibble('Comparison' = NA,
                          'p.adjust' = NA,
                          'Significant?' = NA,
                          'Region' = 'Coastal Plain')
    }

  table = bind_rows(table.cp, table.pied) %>% filter(!is.na(Comparison))

  # Run bootstrapping
  ibi.boot = bootanalysis(data = data, 
                          column = column, 
                          r = r,
                          upper_conf = upper_conf) %>% 
    unite(Factor, Condition, Region, remove = FALSE)
  

  
  # Create combined significance table
  sig_table = bind_cols(ibi.boot, table) %>% 
    select(-Factor, -Region1)
  
  cp.mean = ibi.boot %>% 
    filter(Region == 'Coastal Plain', Condition == 'Good Condition') %>% 
    select(Mean)
  
  pied.mean = ibi.boot %>% 
    filter(Region == 'Piedmont', Condition == 'Good Condition') %>% 
    select(Mean)
  
  cp.sig = sig_table %>% 
    filter(!is.na(`Significant?`)) %>% 
    filter(Region == 'Coastal Plain') %>% 
    filter(Comparison == 'Good Condition - Moderately Degraded' | Comparison == 'Good Condition - Severely Degraded')
  
  pied.sig = sig_table %>% 
    filter(!is.na(`Significant?`)) %>% 
    filter(Region == 'Piedmont') %>% 
    filter(Comparison == 'Good Condition - Moderately Degraded' | Comparison == 'Good Condition - Severely Degraded')
  
  # If tau is lower than 0.5 the beyond samples are those that are of lower than the mean
  # for instance low DO is a potential stressor so beyond that threshold will be lower than
  # the 
  if(tau <= 0.5){
    if(is.tibble(cp.sig) && nrow(cp.sig) > 0){
      cp.ratio = cp %>% 
        mutate(Threshold = ifelse(cp[[column]] > cp.mean$Mean, 'Within Threshold', 'Beyond Threshold'), 
               Classification = ifelse(Biotic.Classification == 'Good Condition', 'Control', 'Cases')) %>% 
        select(Classification, Threshold) %>%
        xtabs(~Threshold + Classification, data = .) %>%
        addmargins()
      
      cp_ratio = tibble('Region' = 'Coastal Plain',
                        'Odds Ratio' = (cp.ratio[1, 1] * cp.ratio[2, 2])/(cp.ratio[1, 2] * cp.ratio[2, 1]),
                        'Risk Cases' = cp.ratio[1, 1]/cp.ratio[3, 1],
                        'Risk Controls' = cp.ratio[1, 2]/cp.ratio[3, 2]) %>% 
        mutate('Attributable Risk' = `Risk Cases` - `Risk Controls`)
    } else {
      cp.ratio = tibble('Cases' = c(NA, NA),
                          'Controls' = c(NA, NA))
      cp_ratio = tibble('Region' = 'Coastal Plain',
                        'Odds Ratio' = NA,
                        'Risk Cases' = NA,
                        'Risk Controls' = NA) %>% 
        mutate('Attributable Risk' = NA)
    }
    
    if(is.tibble(pied.sig) && nrow(pied.sig) > 0){
      pied.ratio = pied %>% 
        mutate(Threshold = ifelse(pied[[column]] > pied.mean$Mean, 'Within Threshold', 'Beyond Threshold'), 
               Classification = ifelse(Biotic.Classification == 'Good Condition', 'Control', 'Cases')) %>% 
        select(Classification, Threshold) %>%
        xtabs(~Threshold + Classification, data = .) %>%
        addmargins()
      
      pied_ratio = tibble('Region' = 'Piedmont',
                        'Odds Ratio' = (pied.ratio[1, 1] * pied.ratio[2, 2])/(pied.ratio[1, 2] * pied.ratio[2, 1]),
                        'Risk Cases' = pied.ratio[1, 1]/pied.ratio[3, 1],
                        'Risk Controls' = pied.ratio[1, 2]/pied.ratio[3, 2]) %>% 
        mutate('Attributable Risk' = `Risk Cases` - `Risk Controls`)
    } else {
      pied.ratio = tibble('Cases' = c(NA, NA),
                          'Controls' = c(NA, NA))
      pied_ratio = tibble('Region' = 'Piedmont',
                        'Odds Ratio' = NA,
                        'Risk Cases' = NA,
                        'Risk Controls' = NA) %>% 
        mutate('Attributable Risk' = NA)
    }
    
  } else {

    if(is.tibble(cp.sig) && nrow(cp.sig) > 0){
      cp.ratio = cp %>% 
        mutate(Threshold = ifelse(cp[[column]] <= cp.mean$Mean, 'Within Threshold', 'Beyond Threshold'), 
               Classification = ifelse(Biotic.Classification == 'Good Condition', 'Control', 'Cases')) %>% 
        select(Classification, Threshold) %>%
        xtabs(~Threshold + Classification, data = .) %>%
        addmargins()
    
      cp_ratio = tibble('Region' = 'Coastal Plain',
                        'Odds Ratio' = (cp.ratio[1, 1] * cp.ratio[2, 2])/(cp.ratio[1, 2] * cp.ratio[2, 1]),
                        'Risk Cases' = cp.ratio[1, 1]/cp.ratio[3, 1],
                        'Risk Controls' = cp.ratio[1, 2]/cp.ratio[3, 2]) %>% 
        mutate('Attributable Risk' = `Risk Cases` - `Risk Controls`)
    } else {
      cp.ratio = tibble('Cases' = c(NA, NA),
                        'Controls' = c(NA, NA))
      cp_ratio = tibble('Region' = 'Coastal Plain',
                        'Odds Ratio' = NA,
                        'Risk Cases' = NA,
                        'Risk Controls' = NA) %>% 
        mutate('Attributable Risk' = NA)
    }
    
    if(is.tibble(pied.sig) && nrow(pied.sig) > 0){
      pied.ratio = pied %>% 
        mutate(Threshold = ifelse(pied[[column]] <= pied.mean$Mean, 'Within Threshold', 'Beyond Threshold'), 
               Classification = ifelse(Biotic.Classification == 'Good Condition', 'Control', 'Cases')) %>% 
        select(Classification, Threshold) %>%
        xtabs(~Threshold + Classification, data = .) %>%
        addmargins()
      
      pied_ratio = tibble('Region' = 'Piedmont',
                          'Odds Ratio' = (pied.ratio[1, 1] * pied.ratio[2, 2])/(pied.ratio[1, 2] * pied.ratio[2, 1]),
                          'Risk Cases' = pied.ratio[1, 1]/pied.ratio[3, 1],
                          'Risk Controls' = pied.ratio[1, 2]/pied.ratio[3, 2]) %>% 
        mutate('Attributable Risk' = `Risk Cases` - `Risk Controls`)
    } else {
      pied.ratio = tibble('Cases' = c(NA, NA),
                        'Controls' = c(NA, NA))
      pied_ratio = tibble('Region' = 'Piedmont',
                          'Odds Ratio' = NA,
                          'Risk Cases' = NA,
                          'Risk Controls' = NA) %>% 
        mutate('Attributable Risk' = NA)
    }
  }
  
  ibi_ratio = bind_rows(cp_ratio, pied_ratio)
  
  table_sig = table %>% select(Region, Comparison, 'Adjusted p-value' = p.adjust, `Significant?`)
  
  ibi_list = lst(Boot = ibi.boot,
                 Sig = table_sig,
                 Ratio = ibi_ratio,
                 CP_Ratio_Table = cp.ratio,
                 Pied_Ratio_Table = pied.ratio)
  return(ibi_list)
}