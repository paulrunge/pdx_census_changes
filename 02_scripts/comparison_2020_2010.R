#new tidycensus w redistricting data *airhorn* thank you dr. walker!
remotes::install_github("walkerke/tidycensus")

#setup
if (!require("pacman")) install.packages("pacman"); library(pacman)
p_load(tidyverse, tidycensus, rio, sf, tigris, janitor, ggplot2, mapview)

var2010 <- load_variables(year = 2010, dataset = "sf1")
var2020 <- load_variables(year = 2020, dataset = "pl")

#ipums census geographies crosswalks: https://www.nhgis.org/geographic-crosswalks
crosswalk_20to10 <- import("./01_inputs/nhgis_blk2020_blk2010_gj_41/nhgis_blk2020_blk2010_gj_41.csv") %>%
  clean_names() %>%
  mutate(geoid10 = str_sub(gjoin2010, start = 2),
         geoid20 = str_sub(gjoin2020, start = 2)) %>%
  select(geoid10, geoid20, weight)


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





