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

write_csv(tab, path="shiny_app/data/aggregated_data.csv")
