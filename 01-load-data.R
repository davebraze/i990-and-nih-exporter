## Load data from several sources, including:
## o IRS form 990s for basic financial info
## o Bureau of Labor Statistics (for CPI-based inflation adjustment)
## o NIH Exporter for NIH grant funding

#################### Start with ParseIRS990::
########## Pull data from 990s

## Get xml files and pull financial data from recent 990s.
## Parsing xml is slow, so do it once and stash it for later.
if(FALSE) {

    ein <- "131628174" ## Haskins Labs
    years <- 2014:2019; names(years) <- paste(years) ## years before 2014 are not available in xml format

    get.financials <- function(year, ein){
        xml <- get_990(ein, year) ## pull info from xml files on the IRS AWS bucket without downloading the files
        ## xml2::write_xml(xml, here("irs990", paste(ein, year, "irs990.xml", sep="-")))
        return(get_all_financial_data(xml))
    }

    financials <-
        map_dfr(years, get.financials, ein, .id = "year") %>%
        mutate(across(.fns = as.integer)) %>%
        mutate(net_assets = assets_total_assets-assets_total_liabilities)

    save(financials, file="financials.rdata")

}

load("financials.rdata")

######### adjust purchasing power for inflation
## Get agreggate cpi data for all urban consumers (cu series) from the Bureau of Labor Statistics.
## An index of available data sets is here: https://download.bls.gov/pub/time.series/overview.txt.
## A list of available "series" (see series_id variable) is here: https://download.bls.gov/pub/time.series/cu/cu.series
## Data dictionary for cu series is here: https://download.bls.gov/pub/time.series/cu/cu.txt.
tmp <- tempfile()
download.file("http://download.bls.gov/pub/time.series/cu/cu.data.1.AllItems",tmp)

## "CUUR0000SA0" == "All items in U.S. city average, all urban consumers, not seasonally adjusted"

cpi <-
    read_delim(tmp,
               delim = "\t", trim_ws = TRUE) %>%
    filter(year %in% 2014:2019,
           period == "M13",                  ## M13 is the annual average
           series_id == "CUUR0000SA0") %>%   ## "All items in U.S. city average, all urban consumers, not seasonally adjusted"
    rename(cpi = value) %>%
    mutate(year = as.integer(year)) %>%
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


######### Pull NIH grant data for Haskins Labs

## Download some NIH Exporter "project" info as csv files and stash them locally

if(FALSE){
    fy <- 1990:2012
    fnames <- paste0("RePORTER_PRJ_C_FY", fy, ".zip")
    localdatadir <- "nih-exporter"
    fy <- 1990:2012
    fnames <- paste0("RePORTER_PRJ_C_FY", fy, ".zip")
    localdatadir <- "nih-exporter"
    url0 <- "https://exporter.nih.gov/CSVs/final"
    dest <- here::here(localdatadir, fnames)
    urls <- paste(url0, fnames, sep="/")
    purrr::map2(urls,
                dest,
                ~ downloader::download(url=.x, destfile=.y))
}

## read Project files, scrape Haskins data, and tidy it up

duns <- c("60010147", "060010147") ## Haskins Labs' proper DUNS is 060010147. NIH sometimes drops the leading "0"

fy <- 1990:2020
fnames <- paste0("RePORTER_PRJ_C_FY", fy, ".zip")
localdatadir <- "nih-exporter"
infiles <- here::here(localdatadir, fnames)

read.nih.projects  <- function(infiles, duns){
    retval <-
        read_csv(infiles,
                 col_types=cols(.default="c",
                                FY="i",
                                SUPPORT_YEAR = "i",
                                DIRECT_COST_AMT = "i",    ## this and the next col don't exist before 2012
                                INDIRECT_COST_AMT = "i",
                                TOTAL_COST = "i")) %>%
        rename_with(tolower) %>%
        filter(str_detect(org_name, "^HASKINS LABORATORIES")) %>%
        ##        filter(org_duns %in% duns)  %>% ## organizations to retain
        filter(is.na(subproject_id)) %>% ## drop subprojects; may need to revist this
        select(-c(subproject_id, total_cost_sub_project)) %>%
        select(-c(phr)) %>% ## problematic column (I don't remember why)
        mutate(award_notice_date = mdy(award_notice_date), ## fixup some date variables
               budget_start = mdy(budget_start),
               budget_end = mdy(budget_end),
               project_start = mdy(project_start),
               project_end = mdy(project_end))

    if (unique(retval$fy) < 2012) {
        return(retval)
    }

    retval %>%
        mutate(indirect_rate = indirect_cost_amt/direct_cost_amt) ## calc effective indirect rates for each grant/year where possible

}

D0_nih  <-
    map_dfr(infiles, read.nih.projects, duns) %>%
    mutate(grant = str_remove(core_project_num, "^[A-Z][0-9]{2}")) %>% ## make unique grant ID
    ## fixup PI Names
    mutate(pi_names = str_replace(pi_names, ";$", ""),
           pi_names = str_replace(pi_names, "\\.", ""),
           pi_names = str_replace(pi_names, "\\(contact\\)", ""),
           pi_names = str_squish(pi_names)) %>%
    mutate(pi_names = recode_factor(pi_names,
                                    `LILLO-MARTIN, DIANE CAROLYN` = "LILLO-MARTIN, DIANE C",
                                    `LILLO-MARTIN, DIANE` = "LILLO-MARTIN, DIANE C",
                                    `SHANKWEILER, DONALD PAUL` = "SHANKWEILER, DONALD P",
                                    `SHANKWEILER, DONALD` = "SHANKWEILER, DONALD P",
                                    `SHADLE, CHRISTINE HELEN` = "SHADLE, CHRISTINE H",
                                    `WHALEN, DOUG H` = "WHALEN, DOUGLAS H"))

D_nih_recent  <-
    D0_nih %>%
    filter(fy %in% 2013:2020) %>%
    group_by(fy) %>%
    summarize(nih_direct_cost = sum(direct_cost_amt),
              nih_indirect_cost = sum(indirect_cost_amt, na.rm=TRUE), ## sometimes indirect costs are missing ("NA"). Need to sort out why.
              nih_total_cost = sum(total_cost),
              nih_effective_indirect_rate = sum(indirect_cost_amt, na.rm=TRUE)/sum(direct_cost_amt, na.rm=TRUE))

## join some NIH data into the i990 dataset from
D_financials <-
    right_join(financials, D_nih_recent, by = c("year" = "fy"))

######### Another take at 990 data, this time using hand-rolled code wrapping xml2::

## load 990s from local xml files
infiles <- dir_ls(here("irs990", "xml-files"), regexp="xml$")
xml.990 <- map(infiles, read_xml)


