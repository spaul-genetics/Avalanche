library(readxl)
library(tidyverse)
library(ggplot2)
dat = readxl::read_excel('./data/Avalanche_data_raw.xlsx', sheet = 1, na = 'NR')

dat$Shovel = recode(dat$Shovel,None = 0, Some = 1, All = 2)
dat$Beacon = recode(dat$Beacon,None = 0, Some = 1, All = 2)
dat$Probe = recode(dat$Probe,None = 0, Some = 1, All = 2)


# Impute missing values by group mean
impute.mean <- function(x) replace(x, is.na(x), mean(x, na.rm = TRUE))

dat = dat %>%
  group_by(Tourer) %>%
  mutate(
    Shovel = impute.mean(Shovel),
    Beacon = impute.mean(Beacon),
    Probe = impute.mean(Probe),
    Total_in_party = impute.mean(as.numeric(Total_in_party))
  )

dat = dat%>%
  rowwise()%>%mutate(Preparedness = sum(c(Shovel, Beacon, Probe), na.rm = T)* Total_in_party)



ggplot(data = dat, aes(x = Tourer, y = Preparedness, fill = Tourer))+
  geom_violin()+geom_boxplot(width = 0.05)+geom_jitter(shape = 16, position = position_jitter(0.05))

dat%>%group_by(Tourer)%>%
  summarise(
    count = n(),
    mean = mean(Preparedness),
    sd = sd(Preparedness)
  )

library(ggpubr)
ggline(dat, x = "Tourer", y = "Preparedness", add = c('mean_se','jitter'))

prep.aov = aov(Preparedness ~ Tourer, data = dat)
summary(prep.aov)

# Significant difference between groups in terms of preparedness. 

TukeyHSD(prep.aov)




