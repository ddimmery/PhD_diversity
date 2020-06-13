library(shiny)
library(readr)
library(dplyr)

files <- paste0("data/C", 2011:2018, "_A.csv")
dat <- do.call(plyr::rbind.fill, lapply(files, read_csv))

cip <- read_csv("data/CIPCode2020.csv")

institutions <- read_csv("data/hd2018.csv")

d <- left_join(dat, cip, by=c("CIPCODE"="CIPCode"))

d <- left_join(d, institutions, by="UNITID")

tab <- d %>%
    group_by(CIPTitle, INSTNM) %>%
    summarize(
        Total = sum(CBKAAT[AWLEVEL == 17], na.rm=TRUE),
        Pct = sum(CBKAAT[AWLEVEL == 17], na.rm=TRUE)/
            sum(CTOTALT[AWLEVEL == 17], na.rm=TRUE),
        Denom = sum(CTOTALT[AWLEVEL == 17], na.rm=TRUE)
    ) %>%
    filter(Denom > 0)

margin_field <- d %>%
    group_by(CIPTitle) %>%
    summarize(
        INSTNM = "Overall",
        Total = sum(CBKAAT[AWLEVEL == 17], na.rm=TRUE),
        Pct = sum(CBKAAT[AWLEVEL == 17], na.rm=TRUE)/
            sum(CTOTALT[AWLEVEL == 17], na.rm=TRUE),
        Denom = sum(CTOTALT[AWLEVEL == 17], na.rm=TRUE)
    ) %>%
    filter(Denom > 0)

margin_school <- d %>%
    group_by(INSTNM) %>%
    summarize(
        CIPTitle = "Overall",
        Total = sum(CBKAAT[AWLEVEL == 17], na.rm=TRUE),
        Pct = sum(CBKAAT[AWLEVEL == 17], na.rm=TRUE)/
            sum(CTOTALT[AWLEVEL == 17], na.rm=TRUE),
        Denom = sum(CTOTALT[AWLEVEL == 17], na.rm=TRUE)
    ) %>%
    filter(Denom > 0)

overall_margin <- d %>%
    summarize(
        CIPTitle = "Overall",
        INSTNM = "Overall",
        Total = sum(CBKAAT[AWLEVEL == 17], na.rm=TRUE),
        Pct = sum(CBKAAT[AWLEVEL == 17], na.rm=TRUE)/
            sum(CTOTALT[AWLEVEL == 17], na.rm=TRUE),
        Denom = sum(CTOTALT[AWLEVEL == 17], na.rm=TRUE)
    )

all = bind_rows(
        tab,
        margin_field,
        margin_school,
        overall_margin
    ) %>%
    arrange(-Denom) %>%
    filter(!is.na(CIPTitle), !is.na(INSTNM))

write_csv(all, path="phd_diversity/data/aggregated_data.csv")
