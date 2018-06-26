library(tidyverse)
library(slipper)

# Bootstrapping function to get the mean
boot.mean = function(x, i){
  boot.mean = mean(x[i], na.rm = TRUE)
  }

# Read in data as a tibble
cp = read_csv('csv/Coastal_plain_cleaned.csv')



cp %>% group_by(Biotic.Classification) %>% tidyboot::tidyboot_mean(., nboot = 10000, na.rm = TRUE)

cp %>% group_by(Biotic.Classification) %>% select(-Site, -Watershed, -Biotic.Classification) %>% map(~{ 
  b = boot::boot(.x, boot.mean, R = 20000, parallel = 'multicore', ncpus = 4);
  c(boot::boot.ci(b, type="perc", conf=0.975)$percent[4], mean(b$t), boot::boot.ci(b, type="perc", conf=0.975)$percent[5])
})

# cp.good = cp %>% 
#   filter(Biotic.Classification == 'Good Condition') %>% 
#   as.data.frame(apply(2, function(y){ 
#     b = boot::boot(y, boot.mean, R = 20000, parallel = 'multicore', ncpus = 4);
#     c(boot::boot.ci(b, type="perc", conf=0.975)$percent[4], mean(b$t), boot::boot.ci(b, type="perc", conf=0.975)$percent[5])
#   }))
# row.names(good.boot) = c('Lower 20% Limit', 'Mean', 'Upper 20% Limit')
# 
# mod.boot = as.data.frame(apply(x.mod, 2, function(y){ 
#   b = boot::boot(y, boot.mean, R = 20000, parallel = 'multicore', ncpus = 4); 
#   c(boot::boot.ci(b, type="perc", conf=0.8)$percent[4], mean(b$t), boot::boot.ci(b, type="perc", conf=0.8)$percent[5])
# }))
# row.names(mod.boot) = c('Lower 20% Limit', 'Mean', 'Upper 20% Limit')
# 
# severe.boot = as.data.frame(apply(x.severe, 2, function(y){ 
#   b = boot::boot(y, boot.mean, R = 20000, parallel = 'multicore', ncpus = 4); 
#   c(boot::boot.ci(b, type="perc", conf=0.8)$percent[4], mean(b$t), boot::boot.ci(b, type="perc", conf=0.8)$percent[5])
# }))
# row.names(severe.boot) = c('Lower 20% Limit', 'Mean', 'Upper 20% Limit')


# # Pool classifications together and rename columns for easy data analysis
# # CM = Channel Modification
# # IH = Instream Habitat
# # P = Pools
# # BSL = Bank Stability (Left)
# # BSR = Bank Stability (Right)
# # BVL = Bank Vegetative (Left)
# # BVR = Bank Vegetative (Right)
# # S = Shading
# # RZL = Riparian Zone Width (Left)
# # RZR = Riparian Zone Width (Right)
# cp = cp %>% 
#   select(Biotic.Classification, CM, IH, P, BSL, BSR, BVL, BVR, S, RZL, RZR, Alkalinity = Alkalinity..Titrimetric..pH.4.5., NH3 = Ammonia.as.N..Total, CBOD20 = BOD..20.Day..N.Inhib..CBOD20., CBOD5 = BOD..5.Day..N.Inhib..CBOD5., DOC = Carbon..Organic...Dissolved..DOC., TOC = Carbon..Organic...Total..TOC., Chloride = Chloride..Total, Chla = Chlorophyll.a, SpCond = Conductance..Specific...Field, Hardness = Hardness.as.CaCO3, NOx = Nitrate.Nitrite.as.N..Total, TN = Nitrogen..Total..Alkaline.Persulfate, OP = Orthophosphorus..Soluable, DO = Oxygen..Dissolved...Membrane.Electrode, OSat = Oxygen..Dissolved...Saturation, pH = pH..Field, TP = Phosphorus..Total..Alkaline.Persulfate, TSS = Residue..Nonfilterable..TSS., Salinity, ATemp = Temperature..Air, Temp = Temperature..Water, Turbidity = Turbidity..Nephelometric., Flow = Flow..Cubic.Feet.Per.Second)