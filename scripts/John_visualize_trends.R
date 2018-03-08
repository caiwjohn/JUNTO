library(dplyr)
library(tidyr)
library(lubridate)
library(ggplot2)

# ---- load data and visualize ---- #

bitcoin_market <- read.delim("../raw_data/btc_historical.tsv",
                             header=T, comment.char="#")

mutate(bitcoin_market, Date=ymd(Date))

coindesk       <- read.delim("../raw_data/coindesk_historical.tsv",
                             header=F, comment.char="#", stringsAsFactors=F,
                             col.names=c("Title", "Date"))

telegraph      <- read.delim("../raw_data/cointelegraph_historical.tsv",
                             header=F, comment.char="#", stringsAsFactors=F,
                             col.names=c("Title", "Date"))

coinist        <- read.delim("../raw_data/bitcoinist_historical.tsv",
                             header=F, comment.char="#", stringsAsFactors=F,
                             col.names=c("Title", "Date"))

coinist$Source   <- "coinist"
coindesk$Source  <- "coindesk"
telegraph$Source <- "telegraph"

news_data <- bind_rows(coinist, coindesk, telegraph) %>%
    mutate(Date   = ymd(substr(Date, 1, 10), tz=NULL),
           Source = factor(Source)) %>%
    filter(!is.na(Date))

# ---- build frequency tables and visualize ---- #
article_vol <- news_data %>% count(Source, Date)

# -- subset from July 2017 - February 2018
article_vol %>%
    filter(between(Date, ymd("2017-07-01"), ymd("2018-02-01"))) %>%
    ggplot(aes(Date, n)) +
        facet_grid(Source ~ .) +
        geom_line() +
        ggtitle("Article Density From Bitcoin") +
        xlab("Month (2017-2018)") +
        ylab("Number of Articles Published Daily") +
        coord_fixed(ratio=1)

ggsave("../results/article_density_july17_jan18.pdf")

# -- subset again from January 2018

article_vol %>%
    filter(between(Date, ymd("2018-01-01"), ymd("2018-02-01"))) %>%
    ggplot(aes(Date, n)) +
      facet_grid(Source ~ .) +
      geom_line() +
      ggtitle("Article Density From Bitcoin News Outlets") +
      xlab("Day") +
      ylab("Number of Articles Published Daily") +
      coord_fixed(ratio=0.2)

ggsave("../results/article_density_jan2018.pdf")


# -- incorporate market data
# todo: normalize market trend and superimpose on plot of article density
article_vol %>%
    filter(between(Date, ymd("2017-07-01"), ymd("2018-02-01"))) %>%
    left_join(bitcoin_market, by=c("Date")) %>%
    mutate(Volume = Volume / first(Volume, order_by=-Volume)) %>%
    group_by(Source) %>%
    mutate(n = n / first(n, order_by=-n)) %>%
    ungroup() %>%
    ggplot(aes(Date)) +
        facet_grid(Source ~ .) +
        geom_line(aes(y=n), color="red") +
        geom_line(aes(y=Volume))
