library(tidyverse)
library(janitor)
library(sf)

match_state_abbreviation <- function(name) {
    ret <- state.abb[which(name == state.name)]
    if (length(ret) == 1) {ret}
    else {""}
}

mode <- function(vector) {
    vwhich.max(tabulate(vector))
}

minwage <- read_csv("data/minwage.csv") %>%
    # standardize column nmaes
    clean_names() %>%
    filter(year %in% 2017:2020) %>%
    # compute monthly wage
    select(year, state, state_minimum_wage, federal_minimum_wage) %>%
    mutate(effective_min_wage = ifelse(state_minimum_wage > federal_minimum_wage,
                                       state_minimum_wage,
                                       federal_minimum_wage),
           monthly_wage = effective_min_wage * 40 * 4,
           # convert state name to abbreviation
           state_abb = map_chr(state, match_state_abbreviation)) %>%
    select(-state) %>%
    # filter guam, DC, etc.
    filter(state_abb != "")
    
polys <- st_read("data/us_counties_hud_zip.geojson") %>%
    # each row is a distinct county, year, apt_type combination
    # we only consider studios/1brs because that is what a minimum wage could reasonably afford
    select(areaname, city, state, fips, contains("br0"), contains("br1"), -contains("2021")) %>%
    pivot_longer(contains("br"), names_to="apt_year", values_to="rent") %>%
    separate(apt_year, into=c("apt", "year"), sep="_") %>%
    mutate(year=parse_number(year),
           state=as.character(state)) %>%
    left_join(minwage, by=c("state"="state_abb", "year"="year")) %>%
    mutate(apt = recode(apt,
                        "br0"="Studio",
                        "br1"="1 Bedroom"))

# % of counties with rent > 33% minimum wage for a 1br/studio by year
polys %>%
    drop_na() %>%
    group_by(year, apt) %>%
    summarize(n_gt_33percent = mean((rent / monthly_wage) > 0.33)) %>%
    ggplot(aes(x=year, y=n_gt_33percent)) +
    geom_line(aes(group=apt, color=apt), size=2) +
    labs(y="Proportion With Rent > 33% Minimum Wage",
         x="Year",
         color="Rental Type")

# Histograms by year of monthly rent, colored by apt type
fed_min_33percent <- (7.25 * 40 * 4) * 0.33
min_15hr_33percent <- (15 * 40 * 4) * 0.33
polys %>%
    drop_na() %>%
    ggplot(aes(x=rent)) +
    geom_histogram(aes(fill=apt)) +
    geom_vline(xintercept=fed_min_33percent,
               linetype="dashed",
               size=1) +
    geom_vline(xintercept=min_15hr_33percent,
               linetype="dotted",
               size=1) +
    facet_wrap(~ year) +
    xlim(c(0, 2000)) +
    labs(x="Monthly Rent (USD)",
         y="Count",
         fill="Rental Type")

# By-state scatterplot of rent and minimum wage
polys %>%
    drop_na() %>%
    filter(year == 2020) %>%
    group_by(state, year, apt) %>%
    summarize(rent = mean(rent),
              minwage = mean(effective_min_wage)) %>%
    ggplot(aes(x=minwage, y=rent)) +
    geom_jitter(aes(color=apt)) +
    labs(y="Monthly Rent (USD)", x="Hourly Minimum Wage (USD)",
         color="Rental Type")
