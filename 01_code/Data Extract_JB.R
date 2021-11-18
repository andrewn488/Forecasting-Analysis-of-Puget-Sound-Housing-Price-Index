library(haven)
library(tidyverse)
library(forecast)

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

PandemicPricing <- puget %>%
  filter(YearMonth >= '2020-1-1')
puget_post <- ts(PandemicPricing$Index_NSA, frequency=12, start=2020)  # monthly data)

plot(puget_post)
hist(puget_post)

# shows non-stationary time series; need to convert to diff

ld_puget_post <- diff(log(puget_post))
plot(ld_puget_post)

# seems to also suggest a non-stationary time series... need to figure out another
# transformation possibly?

acf(puget_post, lag.max=20, main='Puget Pandemic')
pacf(puget_post, lag.max=20, main='Puget Pandemic')

acf(ld_puget_post, lag.max=20, main = 'Log Diff Puget Pandemic')
pacf(ld_puget_post, lag.max=20, main = 'Log Diff Puget Pandemic')

pacf.plot <- pacf(puget_post)

model_ar1 <- arima(puget_post, order = c(1,0,0))
model_ar1
model_best <- auto.arima(ld_puget_post)
arima.model_1

model_ldma1 <- arima(ld_puget_post, order = c(2,0,0))
model_ldma1