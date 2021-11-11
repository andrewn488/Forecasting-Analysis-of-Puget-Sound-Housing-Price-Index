library(haven)
library(tidyverse)

housing <- read_csv("fmhpi_master_file.csv")

summary(housing)

#create table of Seattle-Tacoma-Bellevue area
puget <- filter(housing, GEO_Name == 'Seattle-Tacoma-Bellevue WA')

puget$YearMonth <- as.Date(with(puget,paste(Year,Month,1,sep="-")),"%Y-%m-%d")

puget[, c('YearMonth', 'Index_NSA')]

head(df)

ggplot(data = puget) +
  geom_line(mapping = aes(x = YearMonth, y = Index_NSA, group = GEO_Name))

acf(puget[, c('YearMonth', 'Index_NSA')], lag.max=20)
pacf(puget[, c('YearMonth', 'Index_NSA')], lag.max=20)

#drill down to pandemic-influenced pricing
PandemicPricing <- subset(puget, YearMonth>"2019-12-31")

ggplot(data = PandemicPricing) +
  geom_line(mapping = aes(x = YearMonth, y = Index_NSA, group = GEO_Name))

acf(PandemicPricing[, c('YearMonth', 'Index_NSA')], lag.max=18)
pacf(PandemicPricing[, c('YearMonth', 'Index_NSA')], lag.max=18)
