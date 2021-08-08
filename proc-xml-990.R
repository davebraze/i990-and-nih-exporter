library(fs)
library(here)
library(purrr)
library(dplyr)
library(tidyr)
library(ggplot2)
library(ggrepel)
library(cowplot)
library(readr)
library(ParseIRS990) ## https://github.com/p3lab/ParseIRS990


########## Get the data

## Get xml files and pull financial data from recent 990s.
## Parsing xml is slow, so do it once and stash it for later.
if(FALSE) {

    ein <- "131628174" ## Haskins Labs
    years <- 2014:2019; names(years) <- paste(years)

    get.financials <- function(year, ein){
        xml <- get_990(ein, year)
        return(get_all_financial_data(xml))
    }

    financials <-
        map_dfr(years, get.financials, ein, .id = "year") %>%
        mutate(across(.fns = as.integer)) %>%
        mutate(net_assets = assets_total_assets-assets_total_liabilities)

    labels <-
        financials %>%
        filter(year == max(year)) %>%
        select(year, revenue_total, expenses_total, net_assets) %>%
        rename(Revenue = revenue_total, Expenses = expenses_total, `Net Assets` = net_assets) %>%
        pivot_longer(cols = -1)

    save(financials, file="financials.rdata")

}

load("financials.rdata")

## Get cpi data for all urban consumers (cu series) from the Bureau of Labor Statistics.
## An index of available data sets is here: https://download.bls.gov/pub/time.series/overview.txt.
## A list of available "series" (see series_id variable) is here: https://download.bls.gov/pub/time.series/cu/cu.series
## Data dictionary for cu series is here: https://download.bls.gov/pub/time.series/cu/cu.txt.
tmp <- tempfile()
download.file("http://download.bls.gov/pub/time.series/cu/cu.data.1.AllItems",tmp)

## "CUUR0000SA0" == "All items in U.S. city average, all urban consumers, not seasonally adjusted"

cpi <-
    read_delim(tmp,
               delim = "\t",
               trim_ws = TRUE) %>%
    filter(year %in% 2014:2019,
           period == "M13",                  ## M13 is the annual average
           series_id == "CUUR0000SA0") %>%   ## "All items in U.S. city average, all urban consumers, not seasonally adjusted"
    rename(cpi = value) %>%
    select(year, cpi)

deflate.currency <- function(old.values, price.index) {
    ## old.values: vector of currency values to 'deflate'
    ## price.index: price index that reflects rate of inflation
    ## arguments must be of equal length
    ## base period is always the first in the series (so, 1st period remains constant after adj)
    base.cpi <- price.index[1]
    (old.values * base.cpi) / price.index
}

## Merge cpi and financial data
financials <-
    left_join(financials, cpi) %>%
    mutate(net_assets_adj = deflate.currency (net_assets, cpi),
           revenue_total_adj = deflate.currency (revenue_total, cpi),
           expenses_total_adj = deflate.currency (expenses_total, cpi))

########## Plots

## unadjusted revenue, expenses, and assets
labels <-
    financials %>%
    filter(year == max(year)) %>%
    select(year, revenue_total_adj, expenses_total_adj, net_assets_adj) %>%
    rename(`Revenue` = revenue_total, `Expenses` = expenses_total, `Net Assets` = net_assets) %>%
    pivot_longer(cols = -1)
size=10
p1 <-
    financials %>%
    ggplot(aes(x = year)) +
    geom_line(aes(y = revenue_total), size = size*.1, color = "dark green") +
    geom_line(aes(y = expenses_total), size = size*.1, color = "dark red") +
    geom_line(aes(y = net_assets), size = size*.1, color = "dark orange") +
    scale_y_continuous(limits = c(0,NA),
                       labels = ~paste(.x/10^6, "M")) +
    labs(title = "Haskins Laboratories",
         y = "Dollars (unadjusted)",
         x = "Tax Year",
         caption = "Data is taken from IRS form 990s.\nDollar values are not adjusted for inflation.") +
    geom_text_repel(data = labels,
                    aes(label = name, x = year, y = value-150000),
                    segment.size = .25,
                    size = size*.2,
                    hjust = -0,
                    direction = "y",
                    force = 1/4,
                    nudge_x = 5,
                    nudge_y = 5) +
    coord_fixed(.0000004) +
    theme_cowplot(font_size=size) +
    background_grid(major = c("y"),
                    minor = c("y"),
                    size.major = size*.075)

ggsave("financials-unadj.pdf", p1, device="pdf", width=6.5, height=4)

## revenue, expenses, and assets adjusted for inflation to 2014 value
labels <-
    financials %>%
    filter(year == max(year)) %>%
    select(year, revenue_total_adj, expenses_total_adj, net_assets_adj) %>%
    rename(`Revenue` = revenue_total_adj, `Expenses` = expenses_total_adj, `Net Assets` = net_assets_adj) %>%
    pivot_longer(cols = -1)

size=10
p1 <-
    financials %>%
    ggplot(aes(x = year)) +
    geom_line(aes(y = revenue_total_adj), size = size*.1, color = "dark green") +
    geom_line(aes(y = expenses_total_adj), size = size*.1, color = "dark red") +
    geom_line(aes(y = net_assets_adj), size = size*.1, color = "dark orange") +
    scale_y_continuous(limits = c(0,NA),
                       labels = ~paste(.x/10^6, "M")) +
    labs(title = "Haskins Laboratories",
         y = "Dollars (2014 purchasing power)",
         x = "Tax Year",
         caption = "Data is taken from IRS form 990s.\nDollars are adjusted for inflation to 2014 values.") +
    geom_text_repel(data = labels,
                    aes(label = name, x = year, y = value-150000),
                    segment.size = .25,
                    size = size*.2,
                    hjust = -0,
                    direction = "y",
                    force = 1/4,
                    nudge_x = 5,
                    nudge_y = 5) +
    coord_fixed(.0000004) +
    theme_cowplot(font_size=size) +
    background_grid(major = c("y"),
                    minor = c("y"),
                    size.major = size*.075)

ggsave("financials-adj.pdf", p1, device="pdf", width=6.5, height=4)


## revenue, expenses, and assets: adjusted and unadjusted in one figure
labels <-
    financials %>%
    filter(year == max(year)) %>%
    select(year, revenue_total, expenses_total, net_assets) %>%
    rename(`Revenue` = revenue_total, `Expenses` = expenses_total, `Net Assets` = net_assets) %>%
    pivot_longer(cols = -1)
size=10
p1 <-
    financials %>%
    ggplot(aes(x = year)) +
    geom_line(aes(y = revenue_total), size = size*.1, color = "dark green", alpha=.5) +
    geom_line(aes(y = expenses_total), size = size*.1, color = "dark red", alpha=.5) +
    geom_line(aes(y = net_assets), size = size*.1, color = "dark orange", alpha=.5) +
    geom_line(aes(y = revenue_total_adj), size = size*.05, color = "dark green", alpha=1/3) +
    geom_line(aes(y = expenses_total_adj), size = size*.05, color = "dark red", alpha=1/3) +
    geom_line(aes(y = net_assets_adj), size = size*.05, color = "dark orange", alpha=1/3) +
    scale_y_continuous(limits = c(0,NA),
                       labels = ~paste(.x/10^6, "M")) +
    labs(title = "Haskins Laboratories",
         y = "Dollars",
         x = "Tax Year",
         caption = "Data is taken from IRS form 990s.\nFat lines reflect reported dollar values.\nThin lines are adjusted for inflation to 2014 value.") +
    geom_text_repel(data = labels,
                    aes(label = name, x = year, y = value-150000),
                    segment.size = .25,
                    size = size*.2,
                    hjust = -0,
                    direction = "y",
                    force = 1/4,
                    nudge_x = 5,
                    nudge_y = 5) +
    coord_fixed(.0000004) +
    theme_cowplot(font_size=size) +
    background_grid(major = c("y"),
                    minor = c("y"),
                    size.major = size*.075)

ggsave("financials-adj-unadj.pdf", p1, device="pdf", width=6.5, height=4)
