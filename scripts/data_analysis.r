library(tidyverse)
library(slipper)

# Bootstrapping function to get the mean
boot.mean = function(x, i){
  boot.mean = mean(x[i], na.rm = TRUE)
  }

# Read in data as a tibble
cp = read_csv('csv/Coastal_plain_cleaned.csv')

cp %>% 
  group_by(Biotic.Classification) %>% 
  do(slipper_ci(mean(CM, na.rm = TRUE), df = ., B = 20000, lower = 0.2, upper = 0.8))

cp %>% 
  group_by(Biotic.Classification) %>% 
  do(slipper(mean(CM), df = ., B = 20000)) %>% 
  summarise(ci_low = quantile(value, 0.2), 
            mean = mean(value, na.rm = TRUE), 
            ci_high = quantile(value, 0.8))

# Filter Good Condition and bootstrap
cp.good = cp %>% 
  filter(Biotic.Classification == 'Good Condition') %>%
  select(-Site, -Watershed, -Biotic.Classification)

good.boot = as.data.frame(apply(cp.good, 2, function(y){
  b = boot::boot(y, boot.mean, R = 20000, parallel = 'multicore', ncpus = 4);
  c(boot::boot.ci(b, type="perc", conf=0.8)$percent[4], mean(b$t), boot::boot.ci(b, type="perc", conf=0.8)$percent[5])
}))
row.names(good.boot) = c('Lower 20% Limit', 'Mean', 'Upper 20% Limit')
good.boot = good.boot %>% mutate(Condition = 'Good Condition')

# Filter Moderately Degraded and bootstrap
cp.mod = cp %>% 
  filter(Biotic.Classification == 'Moderately Degraded') %>%
  select(-Site, -Watershed, -Biotic.Classification)

mod.boot = as.data.frame(apply(cp.mod, 2, function(y){
  b = boot::boot(y, boot.mean, R = 20000, parallel = 'multicore', ncpus = 4);
  c(boot::boot.ci(b, type="perc", conf=0.8)$percent[4], mean(b$t), boot::boot.ci(b, type="perc", conf=0.8)$percent[5])
}))
mod.boot = mod.boot %>% mutate(Condition = 'Moderarely Degraded')
row.names(mod.boot) = c('Lower 20% Limit', 'Mean', 'Upper 20% Limit')

# Filter Severely Degraded and bootstrap
cp.severe = cp %>% 
  filter(Biotic.Classification == 'Severely Degraded') %>%
  select(-Site, -Watershed, -Biotic.Classification)

severe.boot = as.data.frame(apply(cp.severe, 2, function(y){
  b = boot::boot(y, boot.mean, R = 20000, parallel = 'multicore', ncpus = 4);
  c(boot::boot.ci(b, type="perc", conf=0.8)$percent[4], mean(b$t), boot::boot.ci(b, type="perc", conf=0.8)$percent[5])
}))
severe.boot = severe.boot %>% mutate(Condition = 'Severely Degraded')
row.names(severe.boot) = c('Lower 20% Limit', 'Mean', 'Upper 20% Limit')

