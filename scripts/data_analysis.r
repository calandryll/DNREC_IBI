library(tidyverse)
library(boot)

# Bootstrapping function to get the mean
boot.mean = function(x, i){
  boot.mean = mean(x[i], na.rm = TRUE)
  }

# Read in data as a tibble
cp = read_csv('csv/Coastal_plain_cleaned.csv')

# Using the same setup as MD
# Filter Good Condition and bootstrap
cp.good = cp %>% 
  filter(Biotic.Classification == 'Good Condition') %>%
  select(-Site, -Watershed, -Biotic.Classification)

good.boot = as.data.frame(apply(cp.good, 2, function(y){
  b = boot(y, boot.mean, R = 20000, parallel = 'multicore', ncpus = 4);
  c(boot.ci(b, type="perc", conf=0.8)$percent[4], mean(b$t), boot::boot.ci(b, type="perc", conf=0.8)$percent[5])
}))
good.boot = good.boot %>% mutate(Condition = 'Good Condition') %>% select(Condition, CM:Turbidity)
row.names(good.boot) = c('Lower 20% Limit', 'Mean', 'Upper 20% Limit')

# Filter Moderately Degraded and bootstrap
cp.mod = cp %>% 
  filter(Biotic.Classification == 'Moderately Degraded') %>%
  select(-Site, -Watershed, -Biotic.Classification)

mod.boot = as.data.frame(apply(cp.mod, 2, function(y){
  b = boot(y, boot.mean, R = 20000, parallel = 'multicore', ncpus = 4);
  c(boot.ci(b, type="perc", conf=0.8)$percent[4], mean(b$t), boot::boot.ci(b, type="perc", conf=0.8)$percent[5])
}))
mod.boot = mod.boot %>% mutate(Condition = 'Moderarely Degraded') %>% select(Condition, CM:Turbidity)
row.names(mod.boot) = c('Lower 20% Limit', 'Mean', 'Upper 20% Limit')

# Filter Severely Degraded and bootstrap
cp.severe = cp %>% 
  filter(Biotic.Classification == 'Severely Degraded') %>%
  select(-Site, -Watershed, -Biotic.Classification)

severe.boot = as.data.frame(apply(cp.severe, 2, function(y){
  b = boot(y, boot.mean, R = 20000, parallel = 'multicore', ncpus = 4);
  c(boot.ci(b, type="perc", conf=0.8)$percent[4], mean(b$t), boot::boot.ci(b, type="perc", conf=0.8)$percent[5])
}))
severe.boot = severe.boot %>% mutate(Condition = 'Severely Degraded') %>% select(Condition, CM:Turbidity)
row.names(severe.boot) = c('Lower 20% Limit', 'Mean', 'Upper 20% Limit')

boot.cp = bind_rows(good.boot, mod.boot, severe.boot) %>% mutate(Stat = c('Lower 20% Limit', 'Mean', 'Upper 20% Limit', 'Lower 20% Limit', 'Mean', 'Upper 20% Limit', 'Lower 20% Limit', 'Mean', 'Upper 20% Limit')) %>% select(Condition, Stat, CM:Turbidity) %>% group_by(Condition) %>% nest()

boot.cp = bind_rows(good.boot, mod.boot, severe.boot) %>% mutate(Stat = c('Lower', 'Mean', 'Upper', 'Lower', 'Mean', 'Upper', 'Lower', 'Mean', 'Upper')) %>% select(Condition, Stat, CM:Turbidity) %>% gather(Test, Data, CM:Turbidity)

boot.cp2 = bind_rows(good.boot, mod.boot, severe.boot) %>% mutate(Stat = c('Lower', 'Mean', 'Upper', 'Lower', 'Mean', 'Upper', 'Lower', 'Mean', 'Upper')) %>% unite(Factor, Condition, Stat, sep = "_") %>% gather(Test, Data, CM:Turbidity)

write_csv(boot.cp, 'csv/Coastal_plain_bootstrap.csv')

# Other ways?

cp %>% 
  group_by(Biotic.Classification) %>% 
  do(slipper_ci(mean(CM, na.rm = TRUE), df = ., B = 20000, lower = 0.2, upper = 0.8))

cp %>% 
  group_by(Biotic.Classification) %>% 
  do(slipper(mean(CM), df = ., B = 20000)) %>% 
  summarise(ci_low = quantile(value, 0.2), 
            mean = mean(value, na.rm = TRUE), 
            ci_high = quantile(value, 0.8))

cp %>%
  filter(Biotic.Classification == 'Good Condition') %>%
  sample_n(size = 20000, replace = TRUE) %>%
  summarise_all(funs(mean), na.rm = TRUE)

cp %>%
  group_by(Biotic.Classification) %>%
  sample_n(size = 20000, replace = TRUE) %>%
  select(-Site, -Watershed) %>%
  summarize_all(funs(ci_low = quantile(., probs = 0.2),
                     mean = mean,
                     ci_high = quantile(., probs = 0.8)),
                na.rm = TRUE, digits = 2)
