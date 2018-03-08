###########################
# Libraries and file loading
###########################
library(dplyr)
library(lubridate)
library(ggplot2)

bitcoin_market<- read.delim("../raw_data/btc_historical.tsv", header = T, comment.char = "#")
coindesk <- read.delim("../raw_data/coindesk_historical.tsv", comment.char = "#", header=F, col.names = c("Title", "Date"))
telegraph<- read.delim("../raw_data/cointelegraph_historical.tsv", comment.char = "#", header = F, col.names = c("Title", "Date"))
coinist<- read.delim("../raw_data/bitcoinist_historical.tsv",comment.char = "#", header = F, col.names = c("Title", "Date"))
###########################
# Convert timestamps to date class
###########################
coinist$Source<- "coinist"
coindesk$Source<- "coindesk"
telegraph$Source<- "telegraph"

news_data<- bind_rows(coinist, telegraph, coindesk) %>%
            mutate(Date   = ymd(substr(Date, 1, 10), tz=NULL),
                    Source = factor(Source)) %>%
            filter(!is.na(Date))
###########################
# Build frequency table and visualize
###########################
# Count articles per source per day
news_data <- as_tibble(news_data) %>% group_by(Source)
site_article_vol<- count(news_data, Date)

# Subset from July 2017 - Feb. 2018
site_article_vol %>%
  filter(between(Date, ymd("2017-07-01"), ymd("2018-02-01"))) %>%
  ggplot(aes(Date, n)) +
    facet_grid(Source ~ .) + 
    geom_line()+ 
    ggtitle("Article Density From Bitcoin News Outlets") +
    xlab("Month (2017-2018)") +
    ylab("Number of Articles Published Daily") +
    coord_fixed(ratio=1)

ggsave("../results/article_density_july17_jan18.pdf")

# Subset again to Jan. 2018
site_article_vol %>%
  filter(between(Date, ymd("2018-01-01"), ymd("2018-02-01"))) %>%
  ggplot(aes(Date, n)) +
    facet_grid(Source ~ .) +
    geom_line() + 
    ggtitle("Article Density From Bitcoin News Outlets")+
    xlab("Day") +
    ylab("Number of Articles Published Daily") +
    coord_fixed(ratio=0.2)

ggsave("../results/article_density_jan2018.pdf")
###########################
# Superimpose market data
###########################
bitcoin_market$Date<- ymd(bitcoin_market$Date)

# Visualize article density and market trend
site_article_vol %>%
  filter(between(Date, ymd("2017-07-01"), ymd("2018-02-01"))) %>%
  left_join(bitcoin_market, by=c("Date")) %>%
  mutate(Volume = Volume / first(Volume, order_by=-Volume)) %>%
  group_by(Source) %>%
  mutate(n = n / first(n, order_by=-n)) %>%
  ungroup() %>%
  ggplot(aes(Date)) +
    facet_grid(Source ~ .) +
    geom_line(aes(y=n)) +
    geom_line(aes(y=Volume), color="red") +
    ggtitle("Bitcoin Market Trends (Red) Superimposed on Daily Article Volume")

ggsave("../results/site_article_vol_bitcoin_trend.pdf")

###########################
# Examine non-site specific trends
###########################
# Count articles per source per day
all_article_vol<- count(ungroup(news_data), Date)

# Visualize article density and market volume
all_article_vol %>%
  filter(between(Date, ymd("2017-07-01"), ymd("2018-02-01"))) %>%
  left_join(bitcoin_market, by=c("Date")) %>%
  mutate(Volume = Volume / first(Volume, order_by=-Volume)) %>%
  mutate(n = n / first(n, order_by=-n)) %>%
  ggplot(aes(Date)) +
    geom_line(aes(y=n)) +
    geom_line(aes(y=Volume), color="red") +
    ylab("Max Normalized Value") +
    ggtitle("Bitcoin Market Volume (Red) Superimposed on Daily Article Volume")

ggsave("../results/all_article_vol_bitcoin_trend.pdf")

# Prepare bitcoin data for faceting
bitcoin_melted<- bitcoin_market %>%
      mutate(High = High / first(High, order_by=-High), 
             Volume = Volume / first(Volume, order_by=-Volume),
             Open = Open / first(Open, order_by=-Open),
             Market.Cap = Market.Cap / first(Market.Cap, order_by=-Market.Cap)) %>%
      select(Date, Open, High, Volume, Market.Cap) %>%
      melt(c("Date"), variable.name= "Market_Stat", value.name= "Stat_Value") %>%
      filter(!is.na(Market_Stat))

# Visualize article density and all statistics
all_article_vol %>%
  filter(between(Date, ymd("2017-07-01"), ymd("2018-02-01"))) %>%
  left_join(bitcoin_melted, by=c("Date")) %>%
  mutate(n = n / first(n, order_by=-n)) %>%
  filter(!is.na(n), !is.na(Date), !is.na(Market_Stat), !is.na(Stat_Value)) %>%
  ggplot(aes(Date)) +
    geom_line(aes(y=n)) +
    geom_line(aes(y=Stat_Value), color="red") +
    facet_grid(Market_Stat ~ .) +
    ylab("Max Normalized Value") +
    ggtitle("Market Trends (Red) Superimposed on Article Volume: All Sources")
  
ggsave("../results/all_article_market_trends.pdf")
  
  
  
  
