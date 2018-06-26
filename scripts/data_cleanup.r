library(tidyverse)

# Clean up of Chemical tests
chem = read.csv('csv/Chem_2005-Present.csv')
chem = chem %>% separate(SampleNumber, c('OrderID', 'Sample'), '-')
chem.spread = chem %>% spread(Test, Results) %>% rename(Collection.Date = CollectDate)

# Clean up of Habitat numbers
cp.hab = read.csv('csv/CP_Hab_assess_1999-Present.csv')
cp.hab = cp.hab %>% separate(Sample.., c('OrderID', 'Sample'), '-') %>% rename(Site = Site.ID)
cp.hab$Site = as.factor(cp.hab$Site)

# Clean up of Bug numbers
cp.bugs = read.csv('csv/CP_Bio_assess_1999-Present.csv')
cp.bugs = cp.bugs %>% separate(Sample.., c('OrderID', 'Sample'), '-') %>% rename(Site = Site.ID)
cp.bugs$Site = as.factor(cp.bugs$Site)

# Join everything with bug data
cp = inner_join(cp.hab, cp.bugs, by = c('OrderID', 'Site')) %>% inner_join(chem.spread) %>% select(-Sample.y, -Collection.Date.y, -Sample.x, -Comment, -CustomerSampleNumber, -Collection.Date.x) %>% distinct(Site, OrderID, .keep_all = TRUE)

# Combine Good and Excellent Conditions
cp = cp %>% mutate(Biotic.Classification = replace(Biotic.Classification, which(Biotic.Classification == 'Good condition'), 'Good Condition'), Biotic.Classification = replace(Biotic.Classification, which(Biotic.Classification == 'Moderately degraded'), 'Moderately Degraded'), Biotic.Classification = replace(Biotic.Classification, which(Biotic.Classification == 'Severely degraded'), 'Severely Degraded'), Biotic.Classification = replace(Biotic.Classification, which(Biotic.Classification == 'Excellent'), 'Good Condition'))

cp_cleaned = cp %>%
  select(-OrderID, -Site.Location, -TOTAL, -Habitat.Classification, -Habitat.Index.., -Replicate, -Number.Taxa, -EPT.Index, -X...E.Composition, -X..Clinger, -Hilsenhoff.Biotic.Index, -Biotic.Index...., -Sample, -Collection.Date)

# save data for future use
write.csv(cp, 'csv/Coastal_plain.csv', row.names = FALSE)
write.csv(cp_cleaned, 'csv/Coastal_plain_cleaned.csv', row.names = FALSE)
