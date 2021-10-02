#new tidycensus w redistricting data *airhorn* thank you dr. walker!
remotes::install_github("walkerke/tidycensus")
options(scipen = 999)

#setup
if (!require("pacman")) install.packages("pacman"); library(pacman)
p_load(tidyverse, tidycensus, rio, sf, tigris, janitor, ggplot2, mapview, RColorBrewer)

var2010 <- load_variables(year = 2010, dataset = "sf1")
var2020 <- load_variables(year = 2020, dataset = "pl")

#ipums census geographies crosswalks: https://www.nhgis.org/geographic-crosswalks
crosswalk_20to10 <- import("./01_inputs/nhgis_blk2020_blk2010_ge_41/nhgis_blk2020_blk2010_ge_41.csv", colClasses = "character") %>%
  clean_names() %>%
  mutate(weight = as.numeric(weight)) %>%
  select(geoid10, geoid20, weight)

#pop density 
#gis
multco_blks <- blocks(state = "OR", county = "Multnomah County", year = 2010)
multco_blks2 <- multco_blks %>%
  clean_names() %>%
  mutate(acres = aland10 * 0.000247105) %>% #aland10 is sq meters
  select(geoid10, acres) 

pdx_boundary <- places(state = "OR", cb = T, year = 2019) %>%
  clean_names() %>%
  filter(str_detect(name, "Portland")) %>%
  select(city = "name")

#data
dens20 <- get_decennial(geography = "block",
                        variables = c(pop = "P1_001N"),
                        county = "Multnomah County",
                        state = "OR",
                        year = 2020,
                        geometry = F) 

dens20_2 <- dens20 %>%
  clean_names() %>%
  inner_join(crosswalk_20to10, by = c("geoid" = "geoid20")) %>%
  mutate(atom_value = value * weight) %>%
  group_by(geoid10) %>%
  summarize(pop20 = sum(atom_value))

dens10 <- get_decennial(geography = "block",
                        variables = c(pop10 = "P001001"),
                        county = "Multnomah County",
                        state = "OR",
                        year = 2010,
                        output = "wide",
                        geometry = F)

density_multco <- dens10 %>%
  clean_names() %>%
  left_join(dens20_2, by = c("geoid" = "geoid10")) 

density_multco2 <- multco_blks2 %>%
  left_join(density_multco, by = c("geoid10" = "geoid")) %>%
  mutate(pop_acre10 = pop10 / acres,
         pop_acre20 = pop20 / acres,
         pop_acre_chg = (pop20 - pop10) / acres,
         pop_chg = pop20 - pop10)


density_pdx <- density_multco2 %>%
  st_intersection(pdx_boundary) %>%
  st_set_geometry(NULL)

density_pdx_land <- multco_blks2 %>%
  select(-acres) %>%
  left_join(density_pdx, by = ("geoid10")) %>%
  filter(acres != 0)

#mapviews
mapview(density_pdx_land, zcol = "pop_acre_chg", at = c(-400, -250, -100, -30, -10, 10, 30, 100, 250, 400), col.regions = brewer.pal(n=9, name = "BrBG"), lwd = 0)
mapview(density_pdx_land, zcol = "pop_chg", at = c(-1500,-1000,-500,-100,-20,20,100,500,1000,1500), col.regions = brewer.pal(n = 9, name = "BrBG"), lwd = 0)
mapview(density_pdx_land, zcol = "pop_acre20", lwd = 0)

#leaflets


#racial distribution
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





