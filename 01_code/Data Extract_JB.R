library(tidyverse)
library(forecast)

#housing <- read_csv("../02_raw_data/fmhpi_master_file.csv")
# wd is being weird
housing <- data

head(housing)

#create table of Seattle-Tacoma-Bellevue area
puget_postpan <- filter(housing, GEO_Name == 'Seattle-Tacoma-Bellevue WA')

puget_postpan$YearMonth <- as.Date(with(puget_postpan,paste(Year,Month,1,sep="-")),"%Y-%m-%d")

puget_postpan[, c('YearMonth', 'Index_NSA')]

ggplot(data = puget_postpan) +
  geom_line(mapping = aes(x = YearMonth, y = Index_NSA, group = GEO_Name))

ggsave("NSA-graph.png")

acf(puget_postpan[, c('YearMonth', 'Index_NSA')], lag.max=20)
pacf(puget_postpan[, c('YearMonth', 'Index_NSA')], lag.max=20)

#drill down to pandemic-influenced pricing
#PandemicPricing <- subset(puget, YearMonth>"2019-12-31")

ggplot(data = PandemicPricing) +
  geom_line(mapping = aes(x = YearMonth, y = Index_NSA, group = GEO_Name))

acf(PandemicPricing[, c('YearMonth', 'Index_NSA')], lag.max=18)
pacf(PandemicPricing[, c('YearMonth', 'Index_NSA')], lag.max=18)

PandemicPricing <- puget_postpan %>%
  filter(YearMonth >= '2020-1-1')
puget_post <- ts(PandemicPricing$Index_NSA, frequency=12, start=2020)  # monthly data)

pacf.plot <- pacf(puget_post)

# ar 1 and ar 2 models for the time series and log diff, respectively
# create models, residuals, and fitted values

model_ar1 <- arima(puget_post, order = c(1,0,0))
model_ar1
#model_best <- auto.arima(ld_puget_post)
#arima.model_1
mod1_res <- residuals(model_ar1)

model_ldar2 <- arima(ld_puget_post, order = c(2,0,0))
model_ldar2
mod2_res <- residuals(model_ldar2)

mod1_fitted <- puget_post - mod1_res
mod2_fitted <- ld_puget_post - mod2_res

ts.plot(puget_post) + points(mod1_fitted, type="l", col=2, lty=2)

ts.plot(ld_puget_post) + points(mod2_fitted, type="l", col=2, lty=2)

#qrtly ma as comparison
PandemicPricing$qrtly_nsa_ma = ma(puget_post, order=4)
ggplot(PandemicPricing, aes(x=YearMonth)) +
  geom_line(aes(y=qrtly_nsa_ma, colour='Qrtly MA')
  ) +
  geom_line(aes(y=Index_SA, colour='Monthly')
  ) +
  scale_colour_manual(""
                      , breaks = c("Qrtly MA", "Monthly")
                      , values = c("red", "blue")
  )