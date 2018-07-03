library(tidyverse)
library(ggthemes)


cp_plot = read_csv('csv/Coastal_plain_bootstrap.csv')

cp_plot %>% filter(Test == 'CM') %>% spread(Stat, Data) %>% ggplot(aes(y = Condition, x = Mean, color = Condition)) + geom_point() + geom_segment(aes(xend = Upper, x = Lower, y = Condition, yend = Condition)) + theme_tufte() + theme(axis.title = element_blank(), legend.position = 'none')
