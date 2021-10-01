#new tidycensus w redistricting data *airhorn* thank you kw!
remotes::install_github("walkerke/tidycensus")

#setup
if (!require("pacman")) install.packages("pacman"); library(pacman)
p_load(tidyverse, tidycensus, rio, sf, tigris, janitor, ggplot2, mapview)

var2010 <- load_variables(year = 2010, dataset = "sf1")
var2020 <- load_variables(year = 2020, dataset = "pl")

#looks like 2020 to 2010 block relationship files are here (not sure if tigris has some shortcut): 
#https://www.census.gov/geographies/reference-files/time-series/geo/relationship-files.html

#explanation of crosswalk: https://www2.census.gov/geo/pdfs/maps-data/data/rel/blockrelfile.pdf
crosswalk_20202010 <- import("./01_inputs/TAB2010_TAB2020_ST41/tab2010_tab2020_st41_or.txt", format = "|", colClasses = "character")
#what percentage of multco blocks are the same geographies as 2010?
#how many am i going to have to attribute or split the data, maybe by area?

#woof, what a mess. maybe i should look at ipums? 
#yeah, this looks better: https://www.nhgis.org/geographic-crosswalks


#2020 pulls
w_poc <- get_decennial(geography = "block", 
              variables = c(wahn = "P2_005N", tot_pop = "P1_001N"),
              county = "Multnomah County",
              state = "OR",
              year = 2020, 
              geometry = F) %>%
  clean_names()
w_poc2 <- w_poc %>%
  group_by(geoid, name) %>%
  summarize(value = value[variable == "tot_pop"] - value[variable == "wahn"],
            variable = "poc") %>%
  bind_rows(w_poc) %>%
  ungroup()





