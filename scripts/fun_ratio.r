library(tidyverse)

ratio_analysis = function(data = NULL, tau = NULL, threshold = NULL, column = NULL) {
  
  cp = data %>%
    filter(Region == 'Coastal Plain')
  
  pied = data %>%
    filter(Region == 'Piedmont')

  # If tau is lower than 0.5 the beyond samples are those that are of lower than the mean
  # for instance low DO is a potential stressor so beyond that threshold will be lower than
  # the 
  if(tau <= 0.5){

    
    if(!all(is.na(cp[[column]]))){
      cp.ratio = cp %>% 
        mutate(Threshold = ifelse(cp[[column]] > threshold, 'Within Threshold', 'Beyond Threshold'), 
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
    
    if(!all(is.na(pied[[column]]))){
      pied.ratio = pied %>% 
        mutate(Threshold = ifelse(pied[[column]] > threshold, 'Within Threshold', 'Beyond Threshold'), 
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

    if(!all(is.na(cp[[column]]))){
      cp.ratio = cp %>% 
        mutate(Threshold = ifelse(cp[[column]] <= threshold, 'Within Threshold', 'Beyond Threshold'), 
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
    
    if(!all(is.na(pied[[column]]))){
      pied.ratio = pied %>% 
        mutate(Threshold = ifelse(pied[[column]] <= threshold, 'Within Threshold', 'Beyond Threshold'), 
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
  
  ibi_list = lst(Ratio = ibi_ratio,
                 CP_Ratio_Table = cp.ratio,
                 Pied_Ratio_Table = pied.ratio)
  
  return(ibi_list)
}
