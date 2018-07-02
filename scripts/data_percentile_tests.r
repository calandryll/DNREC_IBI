library(tidyverse)
source('scripts/fun_percentile.r')

# Read in data as a tibble
cp = read_csv('csv/Coastal_plain_cleaned.csv')

# From http://rcompanion.org/handbook/F_15.html

# Find optimum Percentile
tau = c(0.1, 0.2, 0.3, 0.4, 0.5, 0.6, 0.7, 0.8, 0.9)

for(i in 1:9){
  print(tau[i])
  x = pairwisePercentileTest(Chlorophyll.a  ~ Biotic.Classification, test = 'percentile', r = 20000, tau = tau[i], data = cp, progress = FALSE)
  print(x)
}

# Channel Modification, set to 10th Percentile
CM = pairwisePercentileTest(column = 'CM',  group = 'Biotic.Classification', r = 20000, tau = 0.1, data = cp)
pairwisePercentileTest(column = column,  group = 'Biotic.Classification', r = 20000, tau = 0.1, data = cp)

# Instream Habitat, set to 10th Percentile
IH = pairwisePercentileTest(IH  ~ Biotic.Classification, test = 'percentile', r = 20000, tau = 0.1, data = cp)

# Pools
P = pairwisePercentileTest(P  ~ Biotic.Classification, test = 'percentile', r = 20000, tau = 0.1, data = cp)

# Bank Stability, left and right. Median and Mean of Stability between Classifications are not vastly different
BSL = pairwisePercentileTest(BSL  ~ Biotic.Classification, test = 'percentile', r = 20000, tau = 0.8, data = cp)

BSR = pairwisePercentileTest(BSR  ~ Biotic.Classification, test = 'percentile', r = 20000, tau = 0.8, data = cp)

# Bank Vegetative, left and right. Median and Mean of Stability between Classifications are not vastly different
BVL = pairwisePercentileTest(BVL  ~ Biotic.Classification, test = 'percentile', r = 20000, tau = 0.8, data = cp)

BVR = pairwisePercentileTest(BVR  ~ Biotic.Classification, test = 'percentile', r = 20000, tau = 0.8, data = cp)

# Shading, set to 10th Percentile
S = pairwisePercentileTest(S  ~ Biotic.Classification, test = 'percentile', r = 20000, tau = 0.1, data = cp)

# Riparian Zone Left and Right Banks, set to 10th Percentile
RZL = pairwisePercentileTest(RZL  ~ Biotic.Classification, test = 'percentile', r = 20000, tau = 0.1, data = cp)

RZR = pairwisePercentileTest(RZR  ~ Biotic.Classification, test = 'percentile', r = 20000, tau = 0.1, data = cp)

Alk = pairwisePercentileTest(Alkalinity  ~ Biotic.Classification, test = 'percentile', r = 20000, tau = 0.8, data = cp)

NH3 = pairwisePercentileTest(Ammonia  ~ Biotic.Classification, test = 'percentile', r = 20000, tau = 0.8, data = cp)

BOD20 = pairwisePercentileTest(BOD20  ~ Biotic.Classification, test = 'percentile', r = 20000, tau = 0.8, data = cp)

BOD5 = pairwisePercentileTest(BOD5  ~ Biotic.Classification, test = 'percentile', r = 20000, tau = 0.8, data = cp)

DOC = pairwisePercentileTest(DOC  ~ Biotic.Classification, test = 'percentile', r = 20000, tau = 0.8, data = cp)

TOC = pairwisePercentileTest(TOC  ~ Biotic.Classification, test = 'percentile', r = 20000, tau = 0.8, data = cp)

Cl = pairwisePercentileTest(Chloride  ~ Biotic.Classification, test = 'percentile', r = 20000, tau = 0.8, data = cp)

Chla = pairwisePercentileTest(Chlorophyll_a  ~ Biotic.Classification, test = 'percentile', r = 20000, tau = 0.8, data = cp)

SpCond = pairwisePercentileTest(SpCond  ~ Biotic.Classification, test = 'percentile', r = 20000, tau = 0.8, data = cp)

cfs = pairwisePercentileTest(cfs  ~ Biotic.Classification, test = 'percentile', r = 20000, tau = 0.8, data = cp)

Hardness = pairwisePercentileTest(Hardness  ~ Biotic.Classification, test = 'percentile', r = 20000, tau = 0.8, data = cp)

NOx = pairwisePercentileTest(NOx  ~ Biotic.Classification, test = 'percentile', r = 20000, tau = 0.8, data = cp)

TN = pairwisePercentileTest(TN  ~ Biotic.Classification, test = 'percentile', r = 20000, tau = 0.8, data = cp)

OP = pairwisePercentileTest(Orthophosphorus..Soluable  ~ Biotic.Classification, test = 'percentile', r = 20000, tau = 0.8, data = cp)

# Using 10th Percentile as per MD
DO = pairwisePercentileTest(DO  ~ Biotic.Classification, test = 'percentile', r = 20000, tau = 0.1, data = cp)

DOSat = pairwisePercentileTest(DOSat  ~ Biotic.Classification, test = 'percentile', r = 20000, tau = 0.1, data = cp)

pH = pairwisePercentileTest(pH  ~ Biotic.Classification, test = 'percentile', r = 20000, tau = 0.8, data = cp)

TP = pairwisePercentileTest(TP  ~ Biotic.Classification, test = 'percentile', r = 20000, tau = 0.8, data = cp)

TSS = pairwisePercentileTest(TSS  ~ Biotic.Classification, test = 'percentile', r = 20000, tau = 0.8, data = cp)

Sal = pairwisePercentileTest(Salinity  ~ Biotic.Classification, test = 'percentile', r = 20000, tau = 0.8, data = cp)

Atemp = pairwisePercentileTest(ATemp  ~ Biotic.Classification, test = 'percentile', r = 20000, tau = 0.8, data = cp)

Temp = pairwisePercentileTest(Temp  ~ Biotic.Classification, test = 'percentile', r = 20000, tau = 0.8, data = cp)

Turb = pairwisePercentileTest(Turbidity  ~ Biotic.Classification, test = 'percentile', r = 20000, tau = 0.8, data = cp)