##### sign on #####
library(dplyr)
library(readr)
library(stringr)
library(sf)


##### wrangle species data #####

# read in data
species <- read_csv("species_US.csv")
states <- c(state.abb, "DC")

# preallocation
stats_output <- data.frame(state = character(),
                           species_ct = integer(),
                           species_rank = integer(),
                           extinct_ct = integer(),
                           extinct_rank = integer(),
                           risk_pct = double(),
                           risk_rank = integer(),
                           endemic_ct = integer(),
                           endemic_rank = integer())

# checking for NA values
any(is.na(species$Distribution))
any(is.na(species$`NatureServe Rounded Global Rank`))
any(is.na(species$`Species Group (Fine)`))

# calculating statistics
for (i in states) {
  tmp_species <- species %>% filter(str_detect(Distribution, i)) %>% nrow()
  tmp_extinct <- species %>% filter(str_detect(Distribution, i)) %>%
    {sum(str_count(.$`NatureServe Rounded Global Rank`, "GX|GH"))}
  tmp_risk <- species %>% filter(str_detect(Distribution, i)) %>% 
    {sum(str_count(.$`NatureServe Rounded Global Rank`, "GX|GH|G1|G2|G3"))}/(tmp_species)*100
  tmp_endemic <- species %>% filter(str_detect(Distribution, i)) %>%
    {sum(str_length(.$Distribution) <= 23)}
  stats_output[i, 1] <- i
  stats_output[i, 2] <- tmp_species
  stats_output[i, 4] <- tmp_extinct
  stats_output[i, 6] <- round(tmp_risk, 1)
  stats_output[i, 8] <- tmp_endemic
}

# calculating ranks
stats_output[3] <- as.integer(rank(-stats_output$species_ct))
stats_output[5] <- as.integer(rank(-stats_output$extinct_ct))
stats_output[7] <- as.integer(rank(-stats_output$risk_pct))
stats_output[9] <- as.integer(rank(-stats_output$endemic_ct))


##### combining biodiversity stats to US spatial data #####

# read in data
states_sf <- st_read("cb_2018_us_state_5m.shp")
states_sf <- st_transform(states_sf, 4326)

# joining data
map_output <- states_sf %>%
  select(STUSPS, NAME, geometry) %>%
  merge(stats_output, by.x = "STUSPS", by.y = "state")


##### export #####
write_csv(stats_output, "biodiversity_data.csv")
st_write(map_output, "biodiversity_map.shp", driver = "ESRI Shapefile")


###### sign off #####
save.image()
