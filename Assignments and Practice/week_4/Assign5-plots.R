library(tidyr)
library(ggplot2)
library(dplyr)
library(patchwork)
library(gganimate)
devtools::install_github("dgrtwo/gganimate")


mr = read.csv("C:/Users/Ranae/Desktop/Data_Course/data/mushroom_growth.csv")

head(df)

subPO = subset(df, Species %in% c("P.ostreotus"))
subPC = subset(df, Species %in% c("P.cornucopiae"))


ggplot(mr, aes(x= Temperature, y=GrowthRate, col=Humidity)) +
  geom_point(alpha = .5) +
  facet_wrap(facets = ~Species) +
  geom_violin() +
  stat_smooth(formula = y ~ x) +
  theme_bw() +
  labs(y = "Growth Rate",
       title = "Effects of temperature and humidty on growth rate") 

