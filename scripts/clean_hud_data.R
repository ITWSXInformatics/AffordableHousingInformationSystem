library(tidyverse)
library(janitor)
library(sf)

# there's only 5 spreadsheets and they all have specific
# inconsistencies that mean we have to do this manually
hud_2017 <- read_excel("hud/FY2017_50_rev.xlsx") %>%
    clean_names() %>%
    select(-fips2000) %>%
    rename(cntyname=countyname,
           name=county_town_name,
           cousub=cou_sub) %>%
    mutate(pop2017 = NA,
           hu2017  = NA,
           year = 2017)

hud_2018 <- read_excel("hud/FY2018_50_County_rev.xlsx") %>%
    clean_names() %>%
    rename(cbsasub  = cbsasub18,
           areaname = areaname18) %>%
    mutate(pop2017 = NA,
           hu2017  = NA,
           year = 2018)

hud_2019 <- read_excel("hud/FY2019_50_County_rev.xlsx") %>%
    clean_names() %>%
    rename(cbsasub  = cbsasub19,
           areaname = areaname19) %>%
    mutate(pop2017 = NA,
           hu2017  = NA,
           year = 2019)

hud_2020 <- read_excel("hud/FY2020_50_County_rev.xlsx") %>%
    clean_names() %>%
    rename(cbsasub = cbsasub20,
           areaname = areaname20) %>%
    mutate(pop2010 = NA,
           hu2010  = NA,
           year = 2020)

hud_2021 <- read_excel("hud/FY2021_50_County.xlsx") %>%
    clean_names() %>%
    rename(cbsasub = cbsasub21,
           areaname = areaname21) %>%
    mutate(pop2010 = NA,
           hu2010 = NA,
           year = 2021)

# row-bind everything together, take the mean across subcounties
hud_all <- rbind(hud_2017,
                 hud_2018,
                 hud_2019,
                 hud_2020,
                 hud_2021) %>%
    # discard these - either duplicate or unneeded info
    select(-name, -cbsasub, -state, -county) %>%
    # discard subcounty number from FIPS code
    mutate(fipsalt = as.numeric(fips2010) %/% 100000) %>%
    # Take means across subcounties
    group_by(year, fipsalt) %>%
    summarize(rent_0br = mean(rent50_0),
              rent_1br = mean(rent50_1),
              rent_2br = mean(rent50_2),
              rent_3br = mean(rent50_3),
              rent_4br = mean(rent50_4),
              # some info is discarded from areaname, but the county
              # names are all unique
              areaname = first(areaname),
              cntyname = first(cntyname)) %>%
    rename(fips=fipsalt) %>%
    # pivot the table longer then wider to standardize columns
    gather(key="bedrooms", value="rent", contains("rent_")) %>%
    mutate(bedrooms = parse_number(bedrooms),
           fips = str_pad(fips, 5, side="left", pad="0")) %>%
    pivot_wider(names_from=bedrooms, values_from=rent, names_prefix="br") %>%
    pivot_wider(names_from=year, values_from=br0:br4, names_prefix="yr", names_sep="_")

# save it out
#write_csv(hud_all, "hud/hud_median_rent_county_all.csv")

counties <- st_read("us_counties_zip.goejson")

counties_hud <- inner_join(hud_all, counties, by=c("fips"="FIPS")) %>%
    clean_names()

st_write(counties_hud, "us_counties_hud_zip.geojson")
